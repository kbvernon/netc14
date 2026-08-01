all_cols <- c("dataset", "source", "site", "id", "c14_age", "c14_error")

# Heaton (2021) datasets -------------------------------------------------
heaton <- function(x) {
  file.path(
    "https://raw.githubusercontent.com/",
    "TJHeaton",
    "NonparametricCalibration",
    "refs/heads/main",
    "RealDatasets",
    x
  )
}

# original source: Armit et al (2014)
iron_age <- read.table(
  heaton("armit2014rcc_sd01.csv"),
  sep = ",",
  header = TRUE,
  col.names = c("id", "c14_age", "c14_error"),
  colClasses = c("character", "integer", "integer")
)

iron_age <- transform(
  iron_age,
  dataset = "Iron",
  source = "Heaton2021",
  site = NA_character_
)

iron_age <- subset(
  iron_age,
  !is.na(c14_error),
  select = all_cols
)

# original source: Buchanan et al (2008)
paleo <- read.table(
  heaton("buchanan2008pde.csv"),
  sep = ",",
  header = FALSE,
  col.names = c("site", "nothing", "c14_age", "c14_error", "id"),
  colClasses = c("character", "character", "numeric", "numeric", "character"),
  fill = TRUE
)

# for whatever reason, the ages and estimates are numeric, so we round and cast
paleo <- transform(
  paleo,
  dataset = "Paleo",
  source = "Heaton2021",
  c14_age = as.integer(round(c14_age)),
  c14_error = as.integer(round(c14_error))
)

paleo <- subset(paleo, !is.na(c14_age), select = all_cols)

# original source: Kerr and McCormick (2014)
# the Kerr data are not formatted as proper comma-separated values
# there are utf-8 non-breaking spaces (U+00A) ahead of each comma
raths <- readLines(heaton("kerr2014sss_sup.csv"), warn = FALSE)
raths <- gsub("\u00A0", "", raths)
raths <- read.table(
  text = raths,
  sep = ",",
  header = FALSE,
  col.names = c("site", "id", "c14_age", "c14_error"),
  colClasses = c("character", "character", "integer", "integer")
)

raths <- transform(
  raths,
  dataset = "Raths",
  source = "Heaton2021"
)

raths <- subset(raths, select = all_cols)

# Price (2021) datasets --------------------------------------------------
# the Price data are in an xlsx file, so we pre-process manually
# the GitHub repo: MichaelHoltonPrice/price_et_al_tikal_rc
# filename: MesoRAD-v.1.2_no_locations.xlsx
# this required re-running preprocess_mesorad_data.R
# original source: Hoggarth and Ebert (2020)
tikal <- read.csv("data/mesorad.csv")

tikal <- transform(
  tikal,
  dataset = "Tikal",
  source = "Price2021"
)

tikal <- subset(tikal, site == "Tikal", select = all_cols)

# Collect and save -------------------------------------------------------
radiocarbon <- rbind(iron_age, paleo, raths, tikal)

write.csv(
  radiocarbon,
  "data/radiocarbon.csv",
  row.names = FALSE,
  na = ""
)
