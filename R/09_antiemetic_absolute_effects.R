# Reported ITT absolute comparison for the binary antiemetic endpoint.
#
# Run after 01_prepare_data.R. This script uses the active analysis dataset;
# it does not re-fit or modify the adjusted logistic mixed models.
# The recorded endpoint combines any intraoperative or postoperative use.
# Administration timing is unavailable and must not be inferred from this flag.

source(file.path("R", "00_setup.R"))

input_file <- file.path(paths$derived, "analysis_data.rds")
if (!file.exists(input_file)) stop("Run R/01_prepare_data.R first.")
analysis_data <- readRDS(input_file)

# Resample whole randomized days, separately within each treatment arm.
# Keep all observations from a sampled day together to preserve clustering.
# Pool counts within each arm in each replicate: the target is the difference
# in participant-weighted proportions, not the difference of equally weighted
# daily proportions. No patient-level or second-stage resampling is performed.
bootstrap_replicates <- 50000L
bootstrap_seed <- 20261023L
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(bootstrap_seed)

summarise_antiemetic <- function(data, population) {
  stopifnot(!anyNA(data$study_day), !anyNA(data$treatment))
  allocation_check <- data %>%
    group_by(study_day) %>% summarise(n_arms = n_distinct(treatment), .groups = "drop")
  stopifnot(all(allocation_check$n_arms == 1L))

  # Exclude only missing values of this endpoint, if any, from its denominator.
  observed <- data %>% filter(!is.na(antiemetic))
  stopifnot(all(observed$antiemetic %in% c(0L, 1L)))
  daily <- observed %>%
    group_by(treatment, study_day, .drop = TRUE) %>%
    summarise(events = sum(antiemetic), n = n(), .groups = "drop") %>%
    arrange(treatment, study_day)

  control <- daily %>% filter(treatment == "Control")
  ice <- daily %>% filter(treatment == "Ice lolly")
  stopifnot(nrow(control) >= 2L, nrow(ice) >= 2L)
  n_control <- sum(control$n)
  n_ice <- sum(ice$n)
  events_control <- sum(control$events)
  events_ice <- sum(ice$events)
  risk_control <- events_control / n_control
  risk_ice <- events_ice / n_ice

  # Sampling within arms preserves the observed number of days per arm. A day
  # selected more than once contributes its full counts each time. Ratio-of-
  # totals estimates allow the patient denominator to vary across replicates.
  bootstrap_rd <- replicate(bootstrap_replicates, {
    c_index <- sample.int(nrow(control), nrow(control), replace = TRUE)
    i_index <- sample.int(nrow(ice), nrow(ice), replace = TRUE)
    sum(ice$events[i_index]) / sum(ice$n[i_index]) -
      sum(control$events[c_index]) / sum(control$n[c_index])
  })

  # Use a two-sided 95% percentile interval. It accounts for day-level
  # clustering but is not adjusted for covariates or multiple comparisons.
  ci <- unname(quantile(bootstrap_rd, c(0.025, 0.975), type = 7))
  stopifnot(all(is.finite(bootstrap_rd)), all(ci >= -1 & ci <= 1))

  tibble::tibble(
    population = population, outcome = "antiemetic",
    endpoint = "Any recorded intraoperative or postoperative antiemetic administration",
    n_control = n_control, events_control = events_control,
    risk_control = risk_control, n_ice_lolly = n_ice,
    events_ice_lolly = events_ice, risk_ice_lolly = risk_ice,
    missing_control = sum(data$treatment == "Control" & is.na(data$antiemetic)),
    missing_ice_lolly = sum(data$treatment == "Ice lolly" & is.na(data$antiemetic)),
    days_control = nrow(control), days_ice_lolly = nrow(ice),
    risk_difference = risk_ice - risk_control,
    conf.low = ci[1], conf.high = ci[2],
    risk_difference_percentage_points = 100 * (risk_ice - risk_control),
    ci_lower_percentage_points = 100 * ci[1],
    ci_upper_percentage_points = 100 * ci[2],
    adjustment = "Unadjusted; ice-lolly minus control",
    ci_method = "95% percentile bootstrap; whole study days resampled within arms",
    bootstrap_replicates = bootstrap_replicates, seed = bootstrap_seed,
    interpretation = "Exploratory randomized-day comparison; not an effect of actual consumption"
  )
}

# The revised report presents an absolute risk difference only for ITT. PP
# counts and the PP odds ratio remain in their existing outcome/model tables.
antiemetic_absolute <- summarise_antiemetic(analysis_data, "ITT")

write_csv_safe(antiemetic_absolute,
               file.path(paths$tables, "table_10_antiemetic_absolute_effects.csv"))
writeLines(
  c(paste("Antiemetic absolute comparisons generated:", format(Sys.time())),
    paste("ITT bootstrap replicates:", bootstrap_replicates),
    paste("Seed:", bootstrap_seed),
    "Order: control days, then ice-lolly days within each ITT replicate.",
    "CI: whole-day, treatment-stratified percentile bootstrap, quantile type 7.",
    "Existing adjusted models are unchanged. No multiplicity adjustment.",
    "", capture.output(sessionInfo())),
  file.path(paths$logs, "antiemetic_absolute_run.log")
)

print(antiemetic_absolute %>% select(
  population, events_control, n_control, events_ice_lolly, n_ice_lolly,
  risk_difference_percentage_points, ci_lower_percentage_points,
  ci_upper_percentage_points
))
