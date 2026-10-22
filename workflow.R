library(quarto)

# check that netc() default matches the tuned gamma in data/tuning-summary.csv
source("R/netc14.R")
current_default <- eval(formals(netc)[["gamma"]])

tuning <- read.csv("data/tuning-summary.csv")
best <- with(tuning, max(gamma[nll <= threshold]))

# use all.equal for approximate equality in case numbers get rounded when saved
if (!isTRUE(all.equal(current_default, best))) {
  msg <- paste0(c(
    "netc() default for gamma differs from optimum.\n",
    sprintf("- Update R/netc14.R. Set `gamma=%s`.", best)
  ))
  stop(msg, call. = FALSE)
}

# run analysis and benchmarks, then render manuscript with results
quarto_render("analysis/analysis.qmd", cache_refresh = TRUE)
quarto_render("analysis/timing.qmd", cache_refresh = TRUE)
quarto_render("manuscript/manuscript.qmd")
