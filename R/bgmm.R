#' Bayesian Gaussian Mixture Model
#'
#' Fit a Bayesian Gaussian Mixture Model to calibrated radiocarbon dates using
#' Hamiltonian Monte Carlo, with gradients supplied by torch autograd. The
#' sampler is written in TorchScript, so it runs entirely in libtorch.
#'
#' @param calibrated_dates a `CalGrid` matrix of calibrated dates
#' @param n_clusters integer scalar or vector. If a scalar, uses that many
#'   clusters. If a vector (e.g., `2:10`), selects the best K by BIC at the
#'   posterior mode. Default is 1:12.
#' @param n_iter integer, total MCMC iterations (default 4000). Burn-in is not
#'   removed from the output.
#' @param n_jumps integer, number of leapfrog steps per HMC proposal
#'   (default 12).
#' @param alpha numeric, Dirichlet concentration for mixture weights
#'   (default 1).
#' @param cv numeric, coefficient of variation for the Gamma prior, allows
#'   shape and rate to adapt to a mean for the Gamma defined by the width of the
#'   time range and number of mixture components, with `shape = 1 / cv^2` and
#'   `rate = shape / (n_years / n_clusters)`. Note that mean for the Gamma is
#'   `shape/rate`, and that standard deviation is `sqrt(shape)/rate`.
#' @param seed integer, torch seed for the sampler (default `STARGATE`)
#'
#' @return A list with class `BGMM` containing full MCMC traces of mixture
#'   `weights`, `means`, and `errors`, each an `n_iter x n_clusters` matrix.
#'   Attributes include `n_clusters`, `n_iter`, `n_samples`, `n_accept`,
#'   `step_size`, `start`, `end`, and `elapsed`.
#'
#' @details When `n_clusters` is a vector, each candidate K is initialized with
#'   means at evenly spaced quantiles of the expected calendar ages (posterior
#'   means from the calibrated dates) and optimized to its posterior mode with
#'   Adam. The K minimizing BIC, computed from the mixture likelihood at the
#'   mode, is selected. The mode does not depend on `seed`, so chains with
#'   different seeds start from the same mode, jittered by Gaussian noise
#'   (sd 0.1) on the unconstrained scale.
#'
#'   **Priors**
#'   - weights = Dirichlet(alpha), sampled as normalized Gamma(alpha, 1) draws
#'   - means = flat on ordered means between `start` and `end`
#'   - errors = Gamma(shape, rate)
#'
#'   **Hamiltonian Monte Carlo**
#'   Each HMC proposal follows `n_jumps` steps of leapfrog dynamics along the
#'   autograd gradient of the log posterior. During burn-in, step size is tuned
#'   by dual averaging toward 70% acceptance (Hoffman & Gelman 2014) and a
#'   dense mass matrix is estimated from draws between 1/8 and 3/8 of
#'   `n_iter`, after which dual averaging restarts. Both are held fixed after
#'   burn-in.
bgmm <- function(
  calibrated_dates,
  n_clusters = 1:12,
  n_iter = 4000,
  n_jumps = 12,
  alpha = 1,
  cv = 0.6,
  seed = STARGATE
) {
  .start <- Sys.time()
  torch::local_torch_manual_seed(seed)

  start <- attr(calibrated_dates, "start")
  end <- attr(calibrated_dates, "end")
  years <- seq(start, end)

  # evaluate the likelihood on a 5-year grid to reduce cost
  i <- seq(1, length(years), by = 5)
  M <- calibrated_dates[, i] / rowSums(calibrated_dates[, i])
  M <- torch::torch_tensor(M)
  grid <- torch::torch_tensor(years[i])

  # select number of clusters by BIC at the posterior mode
  point_estimates <- (calibrated_dates / rowSums(calibrated_dates)) %*% years
  modes <- lapply(
    n_clusters,
    find_mode,
    point_estimates = point_estimates,
    M = M,
    grid = grid,
    start = start,
    end = end,
    alpha = alpha,
    cv = cv
  )
  best <- modes[[which.min(sapply(modes, `[[`, "bic"))]]

  # start sampling from the jittered mode
  init <- best[["mode"]] + 0.1 * torch::torch_randn(3 * best[["k"]])

  # do bayesian inference
  hmc <- hmc_script$sample(
    init,
    torch::jit_scalar(as.integer(n_iter)),
    torch::jit_scalar(as.integer(n_jumps)),
    M,
    grid,
    torch::jit_scalar(as.integer(start)),
    torch::jit_scalar(as.integer(end - start)),
    torch::jit_scalar(alpha),
    torch::jit_scalar(best[["shape"]]),
    torch::jit_scalar(best[["rate"]]),
    torch::jit_scalar(EPS)
  )

  # return
  out <- hmc_script$constrain(
    hmc[[1]],
    torch::jit_scalar(as.integer(start)),
    torch::jit_scalar(as.integer(end - start))
  )
  out <- setNames(lapply(out, as.matrix), c("weights", "means", "errors"))

  structure(
    out,
    class = c("BGMM", class(out)),
    n_clusters = best[["k"]],
    n_iter = n_iter,
    n_samples = nrow(calibrated_dates),
    n_accept = hmc[[2]],
    step_size = hmc[[3]],
    start = start,
    end = end,
    elapsed = as.numeric(Sys.time() - .start, units = "secs")
  )
}

# initialize k cluster means at quantiles of the point estimates, optimize to
# the posterior mode, and compute BIC from the mixture likelihood at the mode
find_mode <- function(
  k,
  point_estimates,
  M,
  grid,
  start,
  end,
  alpha,
  cv,
  n_steps = 500
) {
  means <- quantile(point_estimates, (seq_len(k) - 0.5) / k, names = FALSE)
  deltas <- diff(c(start, means)) / (end - means[k])
  errors <- rep((end - start + 1) / k, k)
  init <- log(c(rep(1, k), deltas, errors))

  # adapt gamma parameters to time range and number of clusters
  shape <- 1 / cv^2
  rate <- shape / ((end - start + 1) / k)

  start_ <- torch::jit_scalar(as.integer(start))
  n_years_ <- torch::jit_scalar(as.integer(end - start))

  theta <- torch::torch_tensor(init, requires_grad = TRUE)
  optimizer <- torch::optim_adam(list(theta), lr = 0.05)

  for (step in seq_len(n_steps)) {
    optimizer$zero_grad()
    loss <- -hmc_script$logp(
      theta,
      M,
      grid,
      start_,
      n_years_,
      torch::jit_scalar(alpha),
      torch::jit_scalar(shape),
      torch::jit_scalar(rate),
      torch::jit_scalar(EPS)
    )
    loss$backward()
    optimizer$step()
  }

  ll <- hmc_script$loglik(
    theta,
    M,
    grid,
    start_,
    n_years_,
    torch::jit_scalar(EPS)
  )$item()
  n <- length(point_estimates)

  list(
    k = k,
    mode = theta$detach(),
    shape = shape,
    rate = rate,
    bic = -2 * ll + (3 * k - 1) * log(n)
  )
}

# torchscript: log posterior, gradient, leapfrog, and hmc sampler of bgmm
# unfortunately, this requires python code...
hmc_script <- torch::jit_compile(
  "
def exp_(x: float) -> float:
    return float(torch.exp(torch.tensor(x)))

def log_(x: float) -> float:
    return float(torch.log(torch.tensor(x)))

def constrain(theta: Tensor, start: int,
              n_years: int) -> Tuple[Tensor, Tensor, Tensor]:
    p = torch.chunk(theta, 3, dim=-1)
    weights = torch.softmax(p[0], dim=-1)
    deltas = torch.exp(p[1])
    errors = torch.exp(p[2])

    # ordered means bounded to [start, end], ordering enforces identifiability
    total = 1 + deltas.sum(dim=-1, keepdim=True)
    means = start + n_years * torch.cumsum(deltas, dim=-1) / total

    return weights, means, errors

def loglik(theta: Tensor, M: Tensor, years: Tensor, start: int, n_years: int,
           eps: float) -> Tensor:
    weights, means, errors = constrain(theta, start, n_years)

    Z =(years - means.unsqueeze(1)) / errors.unsqueeze(1)
    D = torch.exp(-0.5 * Z ** 2) / errors.unsqueeze(1)
    f = torch.matmul(weights, D)
    v = f / (f.sum() + eps)

    return torch.log(torch.matmul(M, v) + eps).sum()

def logp(theta: Tensor, M: Tensor, years: Tensor, start: int, n_years: int,
         alpha: float, shape: float, rate: float, eps: float) -> Tensor:
    p = torch.chunk(theta, 3)
    logits = p[0]
    log_deltas = p[1]
    log_errors = p[2]
    deltas = torch.exp(log_deltas)
    errors = torch.exp(log_errors)

    ll = loglik(theta, M, years, start, n_years, eps)
    ld = (alpha * logits - torch.exp(logits)).sum()
    lg = (shape * log_errors - rate * errors).sum()
    jd = log_deltas.sum() - (log_deltas.size(0) + 1) * torch.log1p(deltas.sum())

    return ll + ld + lg + jd

def gradient(theta: Tensor, M: Tensor, years: Tensor, start: int,
             n_years: int, alpha: float, shape: float, rate: float,
             eps: float) -> Tuple[Tensor, Tensor]:
    theta = theta.detach().requires_grad_(True)
    lp = logp(theta, M, years, start, n_years, alpha, shape, rate, eps)
    g = torch.autograd.grad([lp], [theta])[0]
    assert g is not None
    return lp.detach(), g

def sample(theta: Tensor, n_iter: int, n_jumps: int, M: Tensor, years: Tensor,
           start: int, n_years: int, alpha: float, shape: float,
           rate: float, eps: float) -> Tuple[Tensor, int, float]:
    n_par = theta.size(0)
    draws = torch.empty(n_iter, n_par)
    inv_mass = torch.eye(n_par)
    mass_factor = torch.eye(n_par)
    lp, g = gradient(theta, M, years, start, n_years, alpha, shape, rate, eps)
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
        momentum = torch.mv(mass_factor, torch.randn(n_par))
        theta_new = theta
        momentum_new = momentum
        g_new = g
        lp_new = lp
        for s in range(n_jumps):
            momentum_new = momentum_new + 0.5 * step * g_new
            theta_new = theta_new + step * torch.mv(inv_mass, momentum_new)
            lp_new, g_new = gradient(theta_new, M, years, start, n_years, alpha,
                                     shape, rate, eps)
            momentum_new = momentum_new + 0.5 * step * g_new

        # metropolis-hastings
        K_new = torch.dot(momentum_new, torch.mv(inv_mass, momentum_new))
        K_old = torch.dot(momentum, torch.mv(inv_mass, momentum))
        H_new = lp_new - 0.5 * K_new
        H_old = lp - 0.5 * K_old
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

        # estimate dense mass matrix, regularized toward the identity as in
        # stan, and restart dual averaging
        if i == 3 * n_iter // 8:
            n = i - n_iter // 8
            S = torch.cov(draws[(n_iter // 8):i].T)
            I = torch.eye(n_par)
            inv_mass = (n / (n + 5.0)) * S + 1e-3 * (5.0 / (n + 5.0)) * I

            # momentum ~ N(0, inv(inv_mass)) using the inverse cholesky factor
            mass_factor = torch.inverse(torch.cholesky(inv_mass)).T
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
  M <- density_draws(x, years, iters = seq(n_burn + 1, n_iter))

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

# iters x T matrix of normalized mixture densities, one row per posterior draw
density_draws <- function(x, years, iters = seq_len(attr(x, "n_iter"))) {
  M <- matrix(0, length(iters), length(years))
  for (i in seq_along(iters)) {
    means <- x[["means"]][iters[i], ]
    errors <- x[["errors"]][iters[i], ]
    weights <- x[["weights"]][iters[i], ]

    D <- component_densities(means, errors, years)
    f <- as.vector(weights %*% D)
    M[i, ] <- f / (sum(f) + EPS)
  }

  M
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
