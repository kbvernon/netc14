#' Bayesian Gaussian Mixture Model
#'
#' Fit a Bayesian Gaussian Mixture Model to calibrated radiocarbon dates using
#' Hamiltonian Monte Carlo, with gradients supplied by torch autograd. The
#' sampler is written in TorchScript, so it runs entirely in libtorch.
#'
#' @param calibrated_dates a `CalGrid` matrix of calibrated dates
#' @param n_clusters integer scalar or vector. If a scalar, uses that many
#'   clusters. If a vector (e.g., `2:10`), selects the best K by BIC on
#'   k-means fits. Default is 1:9.
#' @param n_iter integer, total MCMC iterations (default 1000). Burn-in is not
#'   removed from the output.
#' @param n_jumps integer, number of leapfrog steps per HMC proposal
#'   (default 12).
#' @param alpha_shape,alpha_rate numeric, shape and rate of the Gamma hyperprior
#'   on the Dirichlet concentration for mixture weights (default 1 and 1).
#' @param cv numeric, coefficient of variation for the Gamma prior, allows
#'   shape and rate to adapt to a mean for the Gamma defined by the width of the
#'   time range and number of mixture components, with `shape = 1 / cv^2` and
#'   `rate = shape / (n_years / n_clusters)`. Note that mean for the Gamma is
#'   `shape/rate`, and that standard deviation is `sqrt(shape)/rate`.
#'
#' @return A list with class `BGMM` containing full MCMC traces of mixture
#'   `weights`, `means`, and `errors`, each an `n_iter x n_clusters` matrix,
#'   and of the Dirichlet concentration `alpha`, an `n_iter x 1` matrix.
#'   Attributes include `n_clusters`, `n_iter`, `n_samples`, `n_accept`,
#'   `step_size`, `start`, `end`, and `elapsed`.
#'
#' @details When `n_clusters` is a vector, the number of components is chosen
#'   by fitting k-means for each candidate K to the expected calendar ages
#'   (posterior means from the calibrated dates), then selecting the K that
#'   minimizes BIC. This is a fast proxy for mixture model selection that avoids
#'   fitting the full MCMC multiple times. Parameters are initialized from the
#'   selected k-means fit: cluster centers become initial means, cluster
#'   proportions become initial weights, and within-cluster standard deviations
#'   become initial errors.
#'
#'   **Priors**
#'   - weights = Dirichlet(alpha), sampled as normalized Gamma(alpha, 1) draws
#'   - alpha = Gamma(alpha_shape, alpha_rate)
#'   - means = flat on ordered means between `start` and `end`
#'   - errors = Gamma(shape, rate)
#'
#'   **Hamiltonian Monte Carlo**
#'   Each HMC proposal follows `n_jumps` steps of leapfrog dynamics along the
#'   autograd gradient of the log posterior. During burn-in, step size is tuned
#'   by dual averaging toward 70% acceptance (Hoffman & Gelman 2014) and a
#'   diagonal mass matrix is estimated from mid-burn-in draws. Both are held
#'   fixed thereafter.
bgmm <- function(
  calibrated_dates,
  n_clusters = 1:9,
  n_iter = 1000,
  n_jumps = 12,
  alpha_shape = 1,
  alpha_rate = 1,
  cv = 0.6
) {
  .start <- Sys.time()
  torch::local_torch_manual_seed(STARGATE)

  start <- attr(calibrated_dates, "start")
  end <- attr(calibrated_dates, "end")
  years <- seq(start, end)

  # initialize parameters with kmeans
  means <- initialize(calibrated_dates, years, n_clusters)
  n_clusters <- length(means)
  weights <- rep(1 / n_clusters, n_clusters)
  deltas <- diff(c(start, means)) / (end - means[n_clusters])
  errors <- rep(length(years) / n_clusters, n_clusters)

  # adapt gamma parameters to time range and number of clusters
  shape <- 1 / cv^2
  rate <- shape / (length(years) / n_clusters)

  # evaluate the likelihood on a 5-year grid to reduce cost
  i <- seq(1, length(years), by = 5)
  M <- calibrated_dates[, i] / rowSums(calibrated_dates[, i])

  # do bayesian inference
  hmc <- hmc_script$sample(
    torch::torch_tensor(log(c(weights, deltas, errors, 1))),
    torch::jit_scalar(as.integer(n_iter)),
    torch::jit_scalar(as.integer(n_jumps)),
    torch::torch_tensor(M),
    torch::torch_tensor(years[i]),
    torch::jit_scalar(as.integer(start)),
    torch::jit_scalar(as.integer(end - start)),
    torch::jit_scalar(alpha_shape),
    torch::jit_scalar(alpha_rate),
    torch::jit_scalar(shape),
    torch::jit_scalar(rate),
    torch::jit_scalar(EPS)
  )

  # return
  out <- hmc_script$constrain(
    hmc[[1]],
    torch::jit_scalar(as.integer(start)),
    torch::jit_scalar(as.integer(end - start))
  )
  out <- setNames(
    lapply(out, as.matrix),
    c("weights", "means", "errors", "alpha")
  )

  structure(
    out,
    class = c("BGMM", class(out)),
    n_clusters = n_clusters,
    n_iter = n_iter,
    n_samples = nrow(calibrated_dates),
    n_accept = hmc[[2]],
    step_size = hmc[[3]],
    start = start,
    end = end,
    elapsed = as.numeric(Sys.time() - .start, units = "secs")
  )
}

# k-means to initialize means and select number of clusters
initialize <- function(calibrated_dates, years, n_clusters) {
  # row normalize measurement matrix first
  point_estimates <- (calibrated_dates / rowSums(calibrated_dates)) %*% years
  n <- length(point_estimates)

  if (length(n_clusters) > 1) {
    fits <- lapply(
      n_clusters,
      FUN = \(.k) kmeans(point_estimates, centers = .k)
    )
    scores <- mapply(
      FUN = \(fit, k) bic(fit[["tot.withinss"]], n, k),
      fit = fits,
      k = n_clusters
    )
    km <- fits[[which.min(scores)]]
  } else {
    km <- kmeans(point_estimates, centers = n_clusters)
  }

  sort(as.vector(km[["centers"]]))
}

# BIC for a k-means fit: N * log(RSS/N) + 3K * log(N)
bic <- function(rss, n, k) {
  n * log(rss / n) + 3 * k * log(n)
}

# torchscript: log posterior, gradient, leapfrog, and hmc sampler of bgmm
# unfortunately, this requires python code...
hmc_script <- torch::jit_compile(
  "
def exp_(x: float) -> float:
    return float(torch.exp(torch.tensor(x)))

def log_(x: float) -> float:
    return float(torch.log(torch.tensor(x)))

def split(theta: Tensor) -> List[Tensor]:
    k = (theta.size(-1) - 1) // 3
    return torch.split(theta, [k, k, k, 1], dim=-1)

def constrain(theta: Tensor, start: int,
              n_years: int) -> Tuple[Tensor, Tensor, Tensor, Tensor]:
    p = split(theta)
    weights = torch.softmax(p[0], dim=-1)
    deltas = torch.exp(p[1])
    errors = torch.exp(p[2])
    alpha = torch.exp(p[3])

    # ordered means bounded to [start, end], ordering enforces identifiability
    total = 1 + deltas.sum(dim=-1, keepdim=True)
    means = start + n_years * torch.cumsum(deltas, dim=-1) / total

    return weights, means, errors, alpha

def logp(theta: Tensor, M: Tensor, years: Tensor, start: int, n_years: int,
         alpha_shape: float, alpha_rate: float, shape: float, rate: float,
         eps: float) -> Tensor:
    p = split(theta)
    logits = p[0]
    log_deltas = p[1]
    log_errors = p[2]
    log_alpha = p[3]
    deltas = torch.exp(log_deltas)
    weights, means, errors, alpha = constrain(theta, start, n_years)

    Z =(years - means.unsqueeze(1)) / errors.unsqueeze(1)
    D = torch.exp(-0.5 * Z ** 2) / errors.unsqueeze(1)
    f = torch.matmul(weights, D)
    v = f / (f.sum() + eps)

    ll = torch.log(torch.matmul(M, v) + eps).sum()
    ld = (alpha * logits - torch.exp(logits) - torch.lgamma(alpha)).sum()
    la = (alpha_shape * log_alpha - alpha_rate * alpha).sum()
    lg = (shape * log_errors - rate * errors).sum()
    jd = log_deltas.sum() - (log_deltas.size(0) + 1) * torch.log1p(deltas.sum())

    return ll + ld + la + lg + jd

def gradient(theta: Tensor, M: Tensor, years: Tensor, start: int,
             n_years: int, alpha_shape: float, alpha_rate: float, shape: float,
             rate: float, eps: float) -> Tuple[Tensor, Tensor]:
    theta = theta.detach().requires_grad_(True)
    lp = logp(theta, M, years, start, n_years, alpha_shape, alpha_rate, shape,
              rate, eps)
    g = torch.autograd.grad([lp], [theta])[0]
    assert g is not None
    return lp.detach(), g

def sample(theta: Tensor, n_iter: int, n_jumps: int, M: Tensor, years: Tensor,
           start: int, n_years: int, alpha_shape: float, alpha_rate: float,
           shape: float, rate: float, eps: float) -> Tuple[Tensor, int, float]:
    n_par = theta.size(0)
    draws = torch.empty(n_iter, n_par)
    inv_mass = torch.ones(n_par)
    lp, g = gradient(theta, M, years, start, n_years, alpha_shape, alpha_rate,
                     shape, rate, eps)
    n_accept = 0

    # dual averaging (Hoffman & Gelman 2014, algorithm 5)
    step = 0.01
    t = 0.0
    u = log_(10 * step)
    e = 0.0
    h = 0.0
    y = 0.05
    o = 10.0
    k = 0.75
    d = 0.7

    for i in range(1, n_iter + 1):
        # leapfrog
        momentum = torch.randn(n_par) / torch.sqrt(inv_mass)
        theta_new = theta
        momentum_new = momentum
        g_new = g
        lp_new = lp
        for s in range(n_jumps):
            momentum_new = momentum_new + 0.5 * step * g_new
            theta_new = theta_new + step * inv_mass * momentum_new
            lp_new, g_new = gradient(theta_new, M, years, start, n_years,
                                     alpha_shape, alpha_rate, shape, rate, eps)
            momentum_new = momentum_new + 0.5 * step * g_new

        # metropolis-hastings
        H_new = lp_new - 0.5 * (inv_mass * momentum_new ** 2).sum()
        H_old = lp - 0.5 * (inv_mass * momentum ** 2).sum()
        a = float(torch.nan_to_num(torch.exp(H_new - H_old), nan=0.0).clamp(max=1.0))
        if float(torch.rand(1)) < a:
            theta = theta_new
            lp = lp_new
            g = g_new
            n_accept += 1
        draws[i - 1] = theta

        # adapt step size
        if i <= n_iter // 2:
            t += 1
            h = (1 - 1 / (t + o)) * h + (d - a) / (t + o)
            log_step = u - (t ** 0.5 / y) * h
            w = t ** -k
            e = w * log_step + (1 - w) * e
            step = exp_(log_step)

        # freeze step size
        if i == n_iter // 2:
            step = exp_(e)
            n_accept = 0

        # estimate mass matrix and restart dual averaging
        if i == n_iter // 4:
            inv_mass = draws[(i // 2):i].var(dim=0).clamp(min=1e-6)
            u = log_(10 * step)
            h = 0.0
            e = 0.0
            t = 0.0

    return draws, n_accept, step
"
)

#' @rdname density
#' @param n_burn integer, number of initial MCMC iterations to discard as
#'   burn-in. If `NULL` (default), uses half of `n_iter`.
#' @details For BGMM, evaluates the mixture density at each posterior sample
#'   and returns the posterior predictive density (the pointwise mean over
#'   draws) with 95% credible intervals.
density.BGMM <- function(x, years, n_burn = NULL, ...) {
  n_iter <- attr(x, "n_iter")
  if (is.null(n_burn)) {
    n_burn <- n_iter %/% 2
  }
  iters <- seq(n_burn + 1, n_iter)

  M <- matrix(0, length(iters), length(years))
  for (i in seq_along(iters)) {
    means <- x[["means"]][iters[i], ]
    errors <- x[["errors"]][iters[i], ]
    weights <- x[["weights"]][iters[i], ]

    D <- component_densities(means, errors, years)
    f <- as.vector(weights %*% D)
    M[i, ] <- f / (sum(f) + EPS)
  }

  d <- colMeans(M)
  q <- apply(M, 2, quantile, probs = c(0.025, 0.975))

  D <- data.frame(
    model = "BGMM",
    cal_age = years,
    density = d,
    conf_lo = q["2.5%", ],
    conf_hi = q["97.5%", ]
  )

  structure(D, class = c("Density", "data.frame"))
}

#' @rdname predict
#' @param n_burn integer, number of initial MCMC iterations to discard as
#'   burn-in. If `NULL` (default), uses half of `n_iter`.
#' @param n_burn integer, number of initial MCMC iterations to discard as
#'   burn-in. If `NULL` (default), uses half of `n_iter`.
#' @details For BGMM, weights each date's calibrated posterior by the posterior
#'   predictive density (after discarding `n_burn` iterations) and draws
#'   calendar age samples via inverse transform sampling.
predict.BGMM <- function(
  object,
  calibrated_dates,
  n_burn = NULL,
  n_draws = 1000,
  ...
) {
  years <- seq(
    attr(calibrated_dates, "start"),
    attr(calibrated_dates, "end")
  )

  v <- density(object, years, n_burn = n_burn)[["density"]]
  q <- t(t(calibrated_dates) * v)
  q <- q / rowSums(q)

  # inverse transform sampling
  ages <- t(apply(
    q,
    MARGIN = 1,
    FUN = inv_transform_sample,
    years = years,
    n = n_draws
  ))

  structure(ages, class = c("Ages", class(ages)))
}

#' Print BGMM
#'
#' @param x a fitted `BGMM` model
#' @param n_burn integer, number of initial MCMC iterations to discard as
#'   burn-in. If `NULL` (default), uses half of `n_iter`.
#' @param ... additional arguments (unused)
print.BGMM <- function(x, n_burn = NULL, ...) {
  template <- paste(
    "Bayesian Gaussian Mixture Model -----",
    "Years: %d-%d",
    "N Samples: %d",
    "N Iter: %d",
    "N Mixtures: %d\n",
    sep = "\n"
  )

  cat(sprintf(
    template,
    attr(x, "start"),
    attr(x, "end"),
    attr(x, "n_samples"),
    attr(x, "n_iter"),
    count_components(x)
  ))

  print(get_parameters(x, n_burn = n_burn))
}
