# Main ITT and per-protocol analyses.
#
# This script fits the prespecified mixed-effects models:
# - primary PACU length of stay,
# - secondary pain, nausea/vomiting, antiemetic use, delirium,
# - ordinal medication dose models,
# - corresponding per-protocol analyses.

source(file.path("R", "00_setup.R"))

analysis_data <- readRDS(file.path(paths$derived, "analysis_data.rds"))

# The SAP asks for ITT as the primary analysis population and PP as a secondary
# population. Both use the same model structure where possible.
per_protocol_data <- analysis_data %>% filter(pp_eligible)

medication_outcomes <- tibble::tribble(
  ~outcome, ~positive_ordinal, ~positive_binary,
  "piritramide_mg", "piritramide_positive_ord", "piritramide_positive_binary",
  "dexketoprofen_mg", "dexketoprofen_positive_ord", "dexketoprofen_positive_binary",
  "ibuprofen_mg", "ibuprofen_positive_ord", "ibuprofen_positive_binary",
  "paracetamol_mg", "paracetamol_positive_ord", "paracetamol_positive_binary",
  "metamizole_g", "metamizole_positive_ord", "metamizole_positive_binary"
)

fit_main_population <- function(data, population) {
  models <- list()
  results <- list()

  # Primary endpoint: PACU length of stay.
  # The observed distribution is right-skewed, so the gamma-log model is the
  # primary model. A linear mixed model is kept as a sensitivity analysis.
  models$primary_gamma <- fit_gamma(data, "pacu_minutes_gamma")
  models$primary_lmm <- fit_lmm(data, "pacu_minutes")

  results$primary <- bind_rows(
    treatment_row(
      models$primary_gamma,
      exponentiate = TRUE,
      outcome = "pacu_minutes",
      component = "gamma-log mixed model: adjusted mean ratio",
      population = population
    ),
    gamma_mean_difference(
      models$primary_gamma,
      outcome = "pacu_minutes",
      component = "gamma-log mixed model: adjusted mean difference",
      population = population
    ),
    treatment_row(
      models$primary_lmm,
      exponentiate = FALSE,
      outcome = "pacu_minutes",
      component = "linear mixed model sensitivity",
      population = population
    )
  )

  # Pain scores are ordered 0-to-10 ratings with many zeros. Instead of forcing
  # a continuous distribution, use a cumulative logit mixed model. The treatment
  # effect is a proportional odds ratio for being in a higher pain-score
  # category.
  for (outcome in c("nrs_admission", "nrs_discharge")) {
    models[[outcome]] <- fit_ordinal(data, outcome, extra = "pacu_hours")
    results[[outcome]] <- treatment_row(
      models[[outcome]],
      exponentiate = TRUE,
      outcome = outcome,
      component = "cumulative logit mixed model",
      population = population
    )
  }

  # Binary outcomes are analysed using logistic mixed models.
  for (outcome in c("nausea", "vomiting", "antiemetic")) {
    models[[outcome]] <- if (population == "PP" && outcome == "vomiting") {
      # In the PP population, vomiting is sparse and the glmmTMB fit gives an
      # unstable variance-covariance matrix. lme4 retains the same fixed-effect
      # structure and gives an interpretable treatment contrast.
      fit_binary_lme4(data, outcome, extra = "pacu_hours")
    } else {
      tryCatch(
        fit_binary(data, outcome, extra = "pacu_hours"),
        error = function(e) fit_binary_lme4(data, outcome, extra = "pacu_hours")
      )
    }

    results[[outcome]] <- treatment_row(
      models[[outcome]],
      exponentiate = TRUE,
      outcome = outcome,
      component = "logistic mixed model",
      population = population
    )
  }

  # Delirium score is analysed as an ordinal outcome.
  models$delirium <- fit_ordinal(data, "delirium", collapse_sparse_surgery = TRUE)
  results$delirium <- treatment_row(
    models$delirium,
    exponentiate = TRUE,
    outcome = "delirium",
    component = "cumulative logit mixed model",
    population = population
  )

  # Medication outcomes use a two-part approach. First, a logistic mixed model
  # estimates whether the medication was administered. Second, among patients
  # with a positive dose, an ordinal positive-dose model is fitted. If that
  # positive-dose model is not estimable, a clinically collapsed binary
  # positive-dose model is used as fallback.
  for (i in seq_len(nrow(medication_outcomes))) {
    outcome <- medication_outcomes$outcome[i]
    positive_ordinal <- medication_outcomes$positive_ordinal[i]
    positive_binary <- medication_outcomes$positive_binary[i]

    medication_fit <- fit_medication_two_part(
      data = data,
      outcome = outcome,
      positive_ordinal = positive_ordinal,
      positive_binary = positive_binary,
      population = population
    )

    models[[outcome]] <- medication_fit[c("any_model", "positive_model", "positive_model_type")]
    results[[outcome]] <- medication_fit$results
  }

  list(
    models = models,
    results = bind_rows(results) %>%
      mutate(estimable = is.finite(estimate) & is.finite(conf.low) & is.finite(conf.high))
  )
}

# Fit all main models for the two analysis populations.
itt <- fit_main_population(analysis_data, "ITT")
per_protocol <- fit_main_population(per_protocol_data, "PP")

write_csv_safe(itt$results, file.path(paths$tables, "table_04_main_results_itt.csv"))
write_csv_safe(per_protocol$results, file.path(paths$tables, "table_05_main_results_per_protocol.csv"))

# The intraclass correlation coefficient is useful for interpreting the
# magnitude of day-to-day clustering in the primary endpoint.
extract_icc <- function(model) {
  icc_value <- tryCatch(
    performance::icc(model),
    error = function(e) NA_real_,
    warning = function(w) suppressWarnings(performance::icc(model))
  )

  if (is.data.frame(icc_value) && "ICC_adjusted" %in% names(icc_value)) {
    return(icc_value$ICC_adjusted[1])
  }

  if (is.list(icc_value) && "ICC_adjusted" %in% names(icc_value)) {
    return(icc_value$ICC_adjusted[1])
  }

  if (is.numeric(icc_value)) {
    return(icc_value[1])
  }

  NA_real_
}

primary_icc <- tibble(
  population = c("ITT", "ITT", "PP", "PP"),
  model = c(
    "Gamma-log primary",
    "Linear mixed sensitivity",
    "Gamma-log primary",
    "Linear mixed sensitivity"
  ),
  icc = c(
    extract_icc(itt$models$primary_gamma),
    extract_icc(itt$models$primary_lmm),
    extract_icc(per_protocol$models$primary_gamma),
    extract_icc(per_protocol$models$primary_lmm)
  ),
  n = c(
    stats::nobs(itt$models$primary_gamma),
    stats::nobs(itt$models$primary_lmm),
    stats::nobs(per_protocol$models$primary_gamma),
    stats::nobs(per_protocol$models$primary_lmm)
  )
)

write_csv_safe(primary_icc, file.path(paths$tables, "table_06_primary_icc.csv"))

# Model objects are saved as intermediate results. This makes it possible to
# inspect or reuse fitted models without re-fitting everything.
saveRDS(itt$models, file.path(paths$models, "models_itt.rds"))
saveRDS(per_protocol$models, file.path(paths$models, "models_per_protocol.rds"))
