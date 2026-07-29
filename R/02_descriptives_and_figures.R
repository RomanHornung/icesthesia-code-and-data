# Descriptive summaries and distribution plots.
#
# This script produces the descriptive tables and the observed-variable plots
# used to judge distributional shape for continuous outcomes. The plots are
# saved so that the modelling choices can be checked visually.

source(file.path("R", "00_setup.R"))

# Load the analysis dataset created by 01_prepare_data.R.
analysis_data <- readRDS(file.path(paths$derived, "analysis_data.rds"))

# Table 1: participant flow. This is a compact manuscript-style overview of
# the ITT and per-protocol populations.
participant_flow <- analysis_data %>%
  summarise(
    population = "ITT",
    n = n(),
    days = n_distinct(study_day),
    control = sum(treatment == "Control"),
    ice_lolly = sum(treatment == "Ice lolly"),
    ice_consumed = sum(ice_consumed == 1),
    per_protocol = sum(pp_eligible)
  )

write_csv_safe(participant_flow, file.path(paths$tables, "table_01_participant_flow.csv"))

# Baseline variables are summarised by treatment group. The resulting table is
# stored in long format because it is easy to reshape for a manuscript table.
categorical_baseline_variables <- c(
  "age_group",
  "sex",
  "asa",
  "surgery_type",
  "anesthesia_type",
  "epidural",
  "regional_add_on"
)

baseline_categorical <- bind_rows(
  lapply(categorical_baseline_variables, function(variable) {
    analysis_data %>%
      mutate(level = as.character(.data[[variable]])) %>%
      count(treatment, level, name = "n") %>%
      group_by(treatment) %>%
      mutate(percent = 100 * n / sum(n), variable = variable) %>%
      ungroup() %>%
      transmute(
        variable = variable,
        level = level,
        treatment = as.character(treatment),
        n = n,
        percent = percent,
        mean = NA_real_,
        sd = NA_real_,
        median = NA_real_,
        q1 = NA_real_,
        q3 = NA_real_
      )
  })
)

# Anaesthesia duration is the continuous baseline covariate used in the adjusted
# models. Both mean/SD and median/IQR are saved so that the final presentation
# can follow the distributional assessment.
baseline_continuous <- analysis_data %>%
  group_by(treatment) %>%
  summarise(
    n = sum(!is.na(anesthesia_minutes)),
    mean = mean(anesthesia_minutes, na.rm = TRUE),
    mean_ci_lower = mean_ci(anesthesia_minutes)["lower"],
    mean_ci_upper = mean_ci(anesthesia_minutes)["upper"],
    sd = sd(anesthesia_minutes, na.rm = TRUE),
    median = median(anesthesia_minutes, na.rm = TRUE),
    q1 = quantile(anesthesia_minutes, 0.25, na.rm = TRUE),
    q3 = quantile(anesthesia_minutes, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  transmute(
    variable = "anesthesia_minutes",
    level = "",
    treatment = as.character(treatment),
    n = n,
    percent = NA_real_,
    mean = mean,
    sd = sd,
    median = median,
    q1 = q1,
    q3 = q3
  )

baseline_table <- bind_rows(baseline_categorical, baseline_continuous)

write_csv_safe(baseline_table, file.path(paths$tables, "table_02_baseline_characteristics.csv"))

# Outcomes are summarised descriptively before modelling. These summaries are
# not adjusted; adjusted treatment effects are saved by 03_main_models.R.
continuous_outcomes <- c(
  "pacu_minutes",
  "nrs_admission",
  "nrs_discharge",
  "piritramide_mg",
  "dexketoprofen_mg",
  "ibuprofen_mg",
  "paracetamol_mg",
  "metamizole_g"
)

continuous_labels <- c(
  pacu_minutes = "PACU length of stay (minutes)",
  nrs_admission = "NRS pain score at PACU admission",
  nrs_discharge = "NRS pain score at PACU discharge",
  piritramide_mg = "Piritramide dose (mg)",
  dexketoprofen_mg = "Dexketoprofen dose (mg)",
  ibuprofen_mg = "Ibuprofen dose (mg)",
  paracetamol_mg = "Paracetamol dose (mg)",
  metamizole_g = "Metamizole dose (g)"
)

continuous_summary <- analysis_data %>%
  pivot_longer(all_of(continuous_outcomes), names_to = "outcome", values_to = "value") %>%
  group_by(treatment, outcome) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    mean_ci_lower = mean_ci(value)["lower"],
    mean_ci_upper = mean_ci(value)["upper"],
    sd = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    q1 = quantile(value, 0.25, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    minimum = min(value, na.rm = TRUE),
    maximum = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    outcome_type = "continuous",
    level = "",
    n_observed = n,
    events = NA_real_,
    percent = NA_real_
  )

binary_summary <- analysis_data %>%
  pivot_longer(c(antiemetic, nausea, vomiting), names_to = "outcome", values_to = "value") %>%
  group_by(treatment, outcome) %>%
  summarise(
    n_observed = sum(!is.na(value)),
    events = sum(value == 1, na.rm = TRUE),
    percent = 100 * events / n_observed,
    .groups = "drop"
  ) %>%
  mutate(
    outcome_type = "binary",
    level = "event",
    n = NA_real_,
    mean = NA_real_,
    mean_ci_lower = NA_real_,
    mean_ci_upper = NA_real_,
    sd = NA_real_,
    median = NA_real_,
    q1 = NA_real_,
    q3 = NA_real_,
    minimum = NA_real_,
    maximum = NA_real_
  )

delirium_summary <- event_table(analysis_data, "delirium") %>%
  transmute(
    treatment,
    outcome = "delirium",
    value = NA_real_,
    outcome_type = "ordinal",
    level,
    n = NA_real_,
    mean = NA_real_,
    mean_ci_lower = NA_real_,
    mean_ci_upper = NA_real_,
    sd = NA_real_,
    median = NA_real_,
    q1 = NA_real_,
    q3 = NA_real_,
    minimum = NA_real_,
    maximum = NA_real_,
    n_observed = NA_real_,
    events = n,
    percent = percent
  )

outcome_descriptives <- bind_rows(
  continuous_summary,
  binary_summary,
  delirium_summary
) %>%
  select(
    outcome_type, outcome, level, treatment, n, n_observed, events, percent,
    mean, mean_ci_lower, mean_ci_upper, sd, median, q1, q3, minimum, maximum
  )

write_csv_safe(outcome_descriptives, file.path(paths$tables, "table_03_outcome_descriptives.csv"))

# Distribution plots are saved in a form that matches the model type. PACU
# duration is treated as continuous, so histogram and Q-Q plots are useful.
save_distribution_plots(
  data = analysis_data,
  variable = "pacu_minutes",
  label = continuous_labels[["pacu_minutes"]],
  folder = paths$figures
)

# NRS scores and medication doses are analysed as ordered outcomes. Bar plots of
# category frequencies are therefore more informative than normal Q-Q plots.
ordinal_plot_variables <- c(
  nrs_admission = "NRS pain score at PACU admission",
  nrs_discharge = "NRS pain score at PACU discharge",
  piritramide_dose_ord = "Piritramide ordered dose category",
  dexketoprofen_dose_ord = "Dexketoprofen ordered dose category",
  ibuprofen_dose_ord = "Ibuprofen ordered dose category",
  paracetamol_dose_ord = "Paracetamol ordered dose category",
  metamizole_dose_ord = "Metamizole ordered dose category"
)

for (outcome in names(ordinal_plot_variables)) {
  save_ordinal_distribution_plot(
    data = analysis_data,
    variable = outcome,
    label = ordinal_plot_variables[[outcome]],
    folder = paths$figures
  )
}
