#' Gaussian Mixture Neural Network
#'
#' Fit a neural network to calibrated radiocarbon dates that approximates a
#' Gaussian Mixture Model.
#'
#' @param calibrated_dates a `CalGrid` matrix of calibrated dates
#' @param max_clusters maximum number of Gaussian clusters to initialize
#'   (default is 35)
#' @param w_gauss weight for Gaussian noise applied to parameters (default 0.02)
#' @param gamma weight for the precision penalty in the loss (default 2.5);
#'   the penalty is `gamma * sqrt(n_samples) * sum(n_years / errors^2)`, so
#'   larger values push components toward wider, lower-precision shapes
#' @param pi sparsemax temperature applied to mixture logits (default 1.75);
#'   smaller values produce sparser weights
#' @param n_epochs number of training epochs (default 512)
#' @param learning_rate learning rate for Adam optimizer (default 0.005)
#'
#' @return A torch `nn_module` of class `NetC14`. Attributes: `start`, `end`,
#'   `n_samples`, `elapsed`, and `training` (a data.frame with columns `epoch`,
#'   `total`, `nll`, `noise`).
#'
#' @details The model uses sparsemax activation on mixture logits to learn the
#'   optimal number of components, a precision penalty to prevent mode collapse,
#'   and Gaussian noise on parameters for uncertainty estimation.
netc <- function(
  calibrated_dates,
  max_clusters = 35,
  w_gauss = 0.02,
  gamma = 2.5,
  pi = 1.75,
  n_epochs = 512,
  learning_rate = 0.005
) {
  .start <- Sys.time()
  torch::local_torch_manual_seed(STARTREK)

  # set up model for training
  start <- attr(calibrated_dates, "start")
  end <- attr(calibrated_dates, "end")
  n_samples <- nrow(calibrated_dates)
  n_years <- ncol(calibrated_dates)
  n_clusters <- min(round(sqrt(n_years)), max_clusters)

  model <- nn_carbon(start, end, n_clusters, n_samples, w_gauss, pi)
  floss <- nn_loss(calibrated_dates, gamma)
  optimizer <- torch::optim_adam(model$parameters, lr = learning_rate)

  # training data: annually resolved grid of years
  years <- torch::torch_arange(start, end)

  # track training
  noise <- c()
  epoch_loss <- c()

  # training
  for (epoch in seq_len(n_epochs)) {
    optimizer$zero_grad()
    v <- model(years)
    loss <- floss(v, model$e)

    # track training metrics
    noise <- c(noise, as_array(floss$noise(model$e)))
    epoch_loss <- c(epoch_loss, loss$item())

    # back-propagation
    loss$backward()
    optimizer$step()
  }

  # return
  training <- data.frame(
    epoch = seq_len(n_epochs),
    total = epoch_loss,
    nll = epoch_loss - noise,
    noise = noise
  )

  # turn off training mode
  model$eval()

  structure(
    model,
    n_samples = n_samples,
    start = start,
    end = end,
    training = training,
    elapsed = as.numeric(Sys.time() - .start, units = "secs")
  )
}

# torch module: forward pass for the model
nn_carbon <- torch::nn_module(
  "NetC14",
  initialize = function(start, end, n_clusters, n_samples, w_gauss, pi) {
    self$w_gauss <- w_gauss
    self$pi <- pi
    self$n_clusters <- n_clusters

    # initialize parameters
    logits <- torch::torch_ones(self$n_clusters)
    self$zero <- start

    # we want the means or cluster locations to fall on the center of each
    # interval, so we add a half-spacing buffer to the start and end
    spacing <- (end - start) / n_clusters
    buffer <- spacing / 2

    spacings <- rep(spacing, n_clusters)
    spacings[1] <- buffer

    # we apply the inverse of the softplus to the mean spacing
    deltas <- torch::torch_tensor(spacings + log1p(-exp(-spacings)))
    errors <- torch::torch_full(self$n_clusters, log(buffer))

    # register parameters for updating
    self$logits <- torch::nn_parameter(logits)
    self$deltas <- torch::nn_parameter(deltas)
    self$errors <- torch::nn_parameter(errors)
  },
  mixture = function() {
    logits <- self$logits
    deltas <- self$deltas
    errors <- self$errors

    # gaussian noise
    if (self$training) {
      logits <- logits + (self$w_gauss * torch::torch_randn_like(logits))
      deltas <- deltas + (self$w_gauss * torch::torch_randn_like(deltas))
      errors <- errors + (self$w_gauss * torch::torch_randn_like(errors))
    }

    spacing <- torch::nnf_softplus(deltas)

    # constrain to parameter space
    weights <- torch::nnf_contrib_sparsemax(logits / self$pi)
    means <- torch::torch_cumsum(spacing, dim = 1) + self$zero
    errors <- torch::torch_exp(errors)

    # save errors for regularization in the loss function
    # restrict to components with non-zero weight for calculating penalty
    self$e <- errors[weights > 0]

    # mixture
    torch::distr_mixture_same_family(
      torch::distr_categorical(probs = weights),
      torch::distr_normal(loc = means, scale = errors)
    )
  },
  forward = function(years) {
    gmm <- self$mixture()
    p <- gmm$log_prob(years)$exp()
    p / p$sum()
  }
)

# torch module: custom loss function
nn_loss <- torch::nn_module(
  "LossC14",
  initialize = function(calibrated_dates, gamma) {
    self$M <- torch::torch_tensor(calibrated_dates)
    self$N <- sqrt(nrow(calibrated_dates))
    self$S <- ncol(calibrated_dates)
    self$gamma <- gamma
  },
  nll = function(v) {
    h <- torch::torch_matmul(self$M, v)
    -torch::torch_log(h + EPS)$sum()
  },
  noise = function(e) {
    self$gamma * self$N * self$S * torch::torch_sum((1 / e)^2)
  },
  forward = function(v, e) {
    self$nll(v) + self$noise(e)
  }
)

#' @rdname density
#' @details For NetC14, runs 256 forward passes with stochastic noise to
#'   estimate posterior quantiles (2.5%, 50%, 97.5%).
density.NetC14 <- function(x, years, ...) {
  torch::local_torch_manual_seed(STARTREK)
  years_tensor <- torch::torch_tensor(years)

  # set model to train mode to get random noise in each sample
  x$train()
  on.exit(x$eval(), add = TRUE)
  density <- replicate(256, {
    p <- x(years_tensor)
    as_vector(p)
  })

  # collect mean and variance
  p <- apply(
    density,
    MARGIN = 1,
    FUN = quantile,
    probs = c(0.025, 0.5, 0.975)
  )

  # return
  D <- data.frame(
    model = "NetC14",
    cal_age = years,
    density = p["50%", ],
    conf_lo = p["2.5%", ],
    conf_hi = p["97.5%", ]
  )

  structure(D, class = c("Density", "data.frame"))
}

#' @rdname predict
#' @details For NetC14, weights each date's calibrated posterior by the
#'   model's estimated global density and draws calendar age samples via
#'   inverse transform sampling.
predict.NetC14 <- function(object, calibrated_dates, n_draws = 1000, ...) {
  years <- seq(
    attr(calibrated_dates, "start"),
    attr(calibrated_dates, "end")
  )
  years_tensor <- torch::torch_tensor(years)

  v <- object(years_tensor)
  v <- as_vector(v)
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

#' Print NetC14
#'
#' @param x a fitted `NetC14` model
#' @param ... additional arguments (unused)
print.NetC14 <- function(x, ...) {
  parameters <- get_parameters(x)
  training <- attr(x, "training")
  final <- training[nrow(training), ]

  template <- paste(
    "Gaussian Mixture Neural Network -----",
    "Years: %d-%d",
    "N Samples: %d",
    "N Epochs: %d",
    "N Mixtures: %d (effective) of %d",
    "Final Loss: %.2f (NLL: %.2f + Noise: %.2f)\n",
    sep = "\n"
  )

  cat(sprintf(
    template,
    attr(x, "start"),
    attr(x, "end"),
    attr(x, "n_samples"),
    nrow(training),
    count_components(x),
    nrow(parameters),
    final[["total"]],
    final[["nll"]],
    final[["noise"]]
  ))

  print(parameters)
}

as_vector <- function(x) {
  as.vector(torch::as_array(x))
}
