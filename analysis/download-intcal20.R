intcal20 <- read.csv(
  "https://intcal.org/curves/intcal20.14c",
  skip = 10,
  col.names = c("cal_age", "c14_age", "c14_error", "delta_age", "delta_error")
)

# just interpolate the whole thing in advance. it only increases size to ~1Mb,
# which is nothing.
years <- with(
  intcal20,
  seq(max(cal_age), min(cal_age), by = -1)
)

means <- with(
  intcal20,
  approx(cal_age, c14_age, xout = years, rule = 2)[["y"]]
)

errors <- with(
  intcal20,
  approx(cal_age, c14_error, xout = years, rule = 2)[["y"]]
)

intcal20 <- data.frame(
  cal_age = years,
  c14_age = means,
  c14_error = errors
)

# somewhat non-conformist ordering to increase with age
# rather than move forward through time
i <- order(years)
intcal20 <- intcal20[i, ]

write.csv(
  intcal20,
  "data/intcal20.csv",
  row.names = FALSE
)
