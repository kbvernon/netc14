#' Bayesian Gaussian Mixture Model
#'
#' Fit a Bayesian Gaussian Mixture Model to calibrated radiocarbon dates using
#' Hamiltonian Monte Carlo, with gradients supplied by torch autograd.
#'
#' @param calibrated_dates a `CalGrid` matrix of calibrated dates
#' @param n_clusters integer scalar or vector. If a scalar, uses that many
#'   clusters. If a vector (e.g., `2:10`), selects the best K by BIC on
#'   k-means fits. Default is 1:9.
#' @param n_iter integer, total MCMC iterations (default 1000). Burn-in is not
#'   removed from the output.
#' @param n_jumps integer, number of leapfrog steps per HMC proposal
#'   (default 12).
#' @param alpha numeric, Dirichlet concentration for mixture weights
#'   (default 0.6).
#' @param cv numeric, coefficient of variation for the Gamma prior, allows
#'   shape and rate to adapt to a mean for the Gamma defined by the width of the
#'   time range and number of mixture components, with `shape = 1 / cv^2` and
#'   `rate = shape / (n_years / n_clusters)`. Note that mean for the Gamma is
#'   `shape/rate`, and that standard deviation is `sqrt(shape)/rate`.
#'
#' @return A list with class `BGMM` containing full MCMC traces of mixture
#'   `weights`, `means`, and `errors`. Each is an `n_iter x n_clusters` matrix.
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
#'   - weights = Dirichlet(alpha)
#'   - means = flat on ordered means above `start`
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
  alpha = 0.6,
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
  deltas <- diff(c(start, means))
  errors <- rep(length(years) / n_clusters, n_clusters)

  # adapt gamma parameters to time range and number of clusters
  shape <- 1 / cv^2
  rate <- shape / (length(years) / n_clusters)

  # unconstrain parameters and initialize sampler
  theta <- torch::torch_tensor(log(c(weights, deltas, errors)))
  posterior <- nn_posterior(calibrated_dates, years, start, alpha, shape, rate)
  hmc <- nn_hmc(posterior, theta, n_jumps, n_iter)

  # do bayesian inference
  draws <- hmc$sample()

  # return
  out <- lapply(posterior$constrain(draws), as.matrix)

  structure(
    out,
    class = c("BGMM", class(out)),
    n_clusters = n_clusters,
    n_iter = n_iter,
    n_samples = nrow(calibrated_dates),
    n_accept = hmc$n_accept,
    step_size = hmc$step,
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

# torch module: log posterior of bgmm
nn_posterior <- torch::nn_module(
  "PosteriorBGMM",
  initialize = function(calibrated_dates, years, start, alpha, shape, rate) {
    # evaluate the likelihood on a 5-year grid to reduce cost
    i <- seq(1, length(years), by = 5)
    M <- calibrated_dates[, i] / rowSums(calibrated_dates[, i])
    self$M <- torch::torch_tensor(M)
    self$years <- torch::torch_tensor(years[i])$unsqueeze(1)
    self$start <- start
    self$alpha <- alpha
    self$shape <- shape
    self$rate <- rate
  },
  # constrain to parameter space
  constrain = function(theta) {
    p <- torch::torch_chunk(theta, 3, dim = -1)

    weights <- torch::nnf_softmax(p[[1]], dim = -1)
    deltas <- torch::torch_exp(p[[2]])
    errors <- torch::torch_exp(p[[3]])

    # the ordering constraint enforces identifiability
    means <- self$start + torch::torch_cumsum(deltas, dim = -1)

    list(weights = weights, means = means, errors = errors)
  },
  # density: p(t|w,u,s) = Σ w * N(t|u,s)
  # also denoted by `v` in (Price et al 2021)
  density = function(weights, means, errors) {
    Z <- (self$years - means$unsqueeze(2)) / errors$unsqueeze(2)
    D <- torch::torch_exp(-0.5 * Z^2) / errors$unsqueeze(2)
    f <- torch::torch_matmul(weights, D)
    f / (f$sum() + EPS)
  },
  log_likelihood = function(v) {
    h <- torch::torch_matmul(self$M, v)
    torch::torch_log(h + EPS)$sum()
  },
  log_dirichlet = function(weights) {
    (self$alpha * torch::torch_log(weights + EPS))$sum()
  },
  log_gamma = function(errors, log_errors) {
    (self$shape * log_errors - self$rate * errors)$sum()
  },
  forward = function(theta) {
    log_ <- torch::torch_chunk(theta, 3)
    log_deltas <- log_[[2]]
    log_errors <- log_[[3]]

    # constrain to parameter space
    p <- self$constrain(theta)
    weights <- p[["weights"]]
    means <- p[["means"]]
    errors <- p[["errors"]]

    # mixture density
    v <- self$density(weights, means, errors)

    ll <- self$log_likelihood(v)
    ld <- self$log_dirichlet(weights)
    lg <- self$log_gamma(errors, log_errors)

    # jacobian for log deltas with uniform prior on means
    jd <- log_deltas$sum()

    ll + ld + lg + jd
  }
)

# torch module: hamiltonian monte carlo sampler, H(θ,m) = P(θ) + K(m)
# state is maintained internally using r6 mutability
# the posterior module is jit-traced on construction
nn_hmc <- torch::nn_module(
  "HMC",
  initialize = function(posterior, theta, n_jumps, n_iter) {
    self$logp <- torch::jit_trace(posterior, theta)

    # inference control
    self$n_par <- theta$size(1)
    self$n_jumps <- n_jumps
    self$n_iter <- n_iter
    self$step <- 0.01

    # state storage
    self$iter <- 0L
    self$draws <- torch::torch_empty(n_iter, self$n_par)
    self$n_accept <- 0
    self$state <- list(
      theta = theta,
      logp = as.numeric(self$logp(theta)),
      momentum = NA,
      inv_mass = torch::torch_ones(self$n_par)
    )
    self$proposal <- list()

    # parameters for adaptive step size with dual averaging (da)
    # fmt: skip
    self$da <- list(
      t = 0,                   # n steps since averaging (re)started
      u = log(10 * self$step), # shrinkage target for log step
      e = 0,                   # running average of log step size
      h = 0,                   # running average of acceptance error
      y = 0.05,                # shrinkage strength toward target u
      o = 10,                  # offset stabilizing early adaptation
      k = 0.75,                # decay exponent for averaging weights
      d = 0.7                  # acceptance rate target
    )
  },
  # the main hmc forward pass
  sample = function() {
    n_iter <- self$n_iter

    # mcmc sampling
    # fmt: skip
    for (i in seq_len(n_iter)) {
      self$momentum()
      self$leapfrog()
      if (self$accept()) self$keep() else self$continue()
      if (i <= n_iter %/% 2) self$adapt()
      if (i == n_iter %/% 2) self$freeze()
      if (i == n_iter %/% 4) self$set_mass()
    }

    self$draws
  },
  # sample momentum
  momentum = function() {
    sqrt_inv_mass <- torch::torch_sqrt(self$state[["inv_mass"]])
    self$state[["momentum"]] <- torch::torch_randn(self$n_par) / sqrt_inv_mass
  },
  # leapfrog integration of hamiltonian dynamics
  leapfrog = function() {
    momentum <- self$state[["momentum"]]
    theta <- self$state[["theta"]]$detach()$requires_grad_(TRUE)
    logp <- self$logp(theta)

    # gradient of the log posterior: ∇logp(θ) = -dU(θ)/dθ
    gradient <- torch::autograd_grad(logp, theta)[[1]]

    for (s in seq_len(self$n_jumps)) {
      # half-step momentum update
      momentum <- momentum + (0.5 * self$step) * gradient
      # full-step parameter update
      theta <- theta + self$step * self$state[["inv_mass"]] * momentum
      # half-step momentum update
      logp <- self$logp(theta)
      gradient <- torch::autograd_grad(logp, theta)[[1]]
      momentum <- momentum + (0.5 * self$step) * gradient
    }

    self$proposal <- list(
      logp = as.numeric(logp),
      theta = theta,
      momentum = momentum
    )
  },
  # accept using metropolis-hastings acceptance rule
  accept = function() {
    logp_current <- self$state[["logp"]]
    momentum_current <- self$state[["momentum"]]
    logp_proposal <- self$proposal[["logp"]]
    momentum_proposal <- self$proposal[["momentum"]]

    # calculate difference between hamiltonian for proposal and current state
    H_proposal <- logp_proposal - self$kinetic(momentum_proposal)
    H_current <- logp_current - self$kinetic(momentum_current)
    dH <- H_proposal - H_current

    # convert to R numeric
    dH <- as.numeric(dH)

    if (is.nan(dH)) {
      dH <- -Inf
    }

    # acceptance probability saved for step size adaptation
    self$da[["a"]] <- min(1, exp(dH))
    runif(1) < self$da[["a"]]
  },
  # kinetic energy: K(m) for momentum m
  kinetic = function(momentum) {
    0.5 * torch::torch_sum(self$state[["inv_mass"]] * momentum^2)
  },
  # add accepted proposal to draws matrix and update state
  keep = function() {
    self$iter <- self$iter + 1L
    self$draws[self$iter, ] <- self$proposal[["theta"]]
    self$state[["theta"]] <- self$proposal[["theta"]]
    self$state[["logp"]] <- self$proposal[["logp"]]
    self$n_accept <- self$n_accept + 1
  },
  # add current state to draws matrix (if proposal rejected)
  continue = function() {
    self$iter <- self$iter + 1L
    self$draws[self$iter, ] <- self$state[["theta"]]
  },
  # tune the step size toward acceptance rate target (d) by dual averaging
  # see (Hoffman & Gelman 2014, algorithm 5)
  # fmt: skip
  adapt = function() {
    # the equations are easier to read without `self$` everywhere
    t <- self$da[["t"]] + 1L
    o <- self$da[["o"]]
    u <- self$da[["u"]]
    h <- self$da[["h"]]
    y <- self$da[["y"]]
    k <- self$da[["k"]]
    d <- self$da[["d"]]
    a <- self$da[["a"]]

    h <- (1 - 1 / (t + o)) * h + (d - a) / (t + o)
    e <- u - (sqrt(t) / y) * h
    w <- t^-k

    self$da[["t"]] <- t
    self$da[["h"]] <- h
    self$da[["e"]] <- w * e + (1 - w) * self$da[["e"]]
    self$step <- exp(e)
  },
  # estimate diagonal mass matrix from draws window and restart step size search
  set_mass = function() {
    window <- (self$iter %/% 2 + 1):self$iter
    inv_mass <- self$draws[window, ]$var(dim = 1)$clamp(min = 1e-6)
    self$state[["inv_mass"]] <- inv_mass

    # restart dual averaging
    self$da[["u"]] <- log(10 * self$step)
    self$da[["h"]] <- 0
    self$da[["e"]] <- 0
    self$da[["t"]] <- 0
  },
  # post-burn-in draws are non-adaptive
  freeze = function() {
    self$step <- exp(self$da[["e"]])
    self$n_accept <- 0
  }
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
