# Project setup for the Icesthesia analysis.
#
# Run the analysis from the "code_and_data" folder with:
#   Rscript R/99_run_all.R
#
# This script defines relative paths, checks required R packages, creates output
# folders, and loads the packages used throughout the analysis.

options(stringsAsFactors = FALSE)
options(contrasts = c("contr.treatment", "contr.poly"))

set.seed(20260707)

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

if (!dir.exists(file.path(project_root, "R")) ||
    !dir.exists(file.path(project_root, "data"))) {
  stop(
    "Please run the analysis from the code_and_data folder, ",
    "which must contain R/ and data/."
  )
}

paths <- list(
  data = file.path(project_root, "data"),
  raw_data = file.path(project_root, "data", "EIS_R_pseudodata.xlsx"),
  codebook = file.path(project_root, "data", "variable_coding_ice_r.xlsx"),
  results = file.path(project_root, "results_pseudodata"),
  results_real = file.path(project_root, "results_real")
)

paths$derived <- file.path(paths$results, "derived_data")
paths$tables <- file.path(paths$results, "tables")
paths$figures <- file.path(paths$results, "figures")
paths$models <- file.path(paths$results, "models")
paths$logs <- file.path(paths$results, "logs")

for (folder in paths[c("results", "derived", "tables", "figures", "models", "logs")]) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}

required_packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "lme4",
  "glmmTMB",
  "ordinal",
  "broom.mixed",
  "emmeans",
  "performance",
  "readr",
  "writexl"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))

if (length(missing_packages) > 0) {
  stop(
    "Missing R packages: ",
    paste(missing_packages, collapse = ", "),
    ". Install them with install.packages() before running the analysis."
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lme4)
  library(glmmTMB)
  library(ordinal)
  library(broom.mixed)
  library(emmeans)
  library(performance)
  library(readr)
  library(writexl)
})

source(file.path(project_root, "R", "functions.R"))
