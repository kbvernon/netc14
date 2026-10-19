models <- c("BGMM", "CKDE", "DPMM", "NetC14", "SPD")

# results
timing_results <- read.csv("data/timing-results.csv")
evaluation_results <- read.csv("data/evaluation-results.csv")

evaluation_results <- transform(
  evaluation_results,
  recovery = (mae - mae_oracle) / mae_oracle
)

# summaries are median and 95% interval, with the interval on its own line
summarize <- function(.data, metric) {
  .f <- as.formula(sprintf("%s ~ model", metric))
  p <- aggregate(.f, FUN = quantile, prob = c(0.025, 0.5, 0.975), data = .data)
  p <- p[match(models, p[["model"]]), metric][, c(2, 1, 3)]
  sprintf("%.3f\\\n      (%.3f, %.3f)", p[, 1], p[, 2], p[, 3])
}

tbl <- matrix(
  c(
    models,
    summarize(evaluation_results, "recovery"),
    summarize(evaluation_results, "mae"),
    summarize(evaluation_results, "tvd"),
    summarize(timing_results, "elapsed")
  ),
  ncol = 5
)

tbl <- rbind(c("Model", "MAE-Recovery", "MAE", "TVD", "Elapsed (s)"), tbl)

rows <- apply(tbl, 1, \(.row) {
  dashes <- c("* - ", rep("  - ", length(.row) - 1))
  paste0(dashes, .row, collapse = "\n")
})

cat(paste(rows, collapse = "\n"))
