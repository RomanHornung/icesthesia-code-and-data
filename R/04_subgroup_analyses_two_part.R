# Prespecified exploratory subgroup analyses.
#
# For each prespecified subgroup, this script fits a treatment-by-subgroup
# interaction model and reports both the global interaction p-value and the
# treatment effect within each subgroup level.

source(file.path("R", "00_setup.R"))

analysis_data <- readRDS(file.path(paths$derived, "analysis_data.rds")) %>%
  mutate(
    piritramide_mg_any = as.integer(piritramide_mg > 0),
    dexketoprofen_mg_any = as.integer(dexketoprofen_mg > 0),
    ibuprofen_mg_any = as.integer(ibuprofen_mg > 0),
    paracetamol_mg_any = as.integer(paracetamol_mg > 0),
    metamizole_g_any = as.integer(metamizole_g > 0)
  )

subgroups <- c("age_group", "sex", "asa", "surgery_type", "anesthesia_type")

endpoint_specs <- tibble::tribble(
  ~outcome, ~reported_outcome, ~model_type, ~extra, ~component, ~effect_scale, ~data_scope, ~positive_source,
  "pacu_minutes_gamma", "pacu_minutes", "gamma", NA_character_,
  "primary PACU gamma-log model", "ratio", "all", NA_character_,
  "nrs_admission", "nrs_admission", "ordinal", "pacu_hours",
  "NRS admission cumulative logit mixed model", "proportional odds ratio", "all", NA_character_,
  "nrs_discharge", "nrs_discharge", "ordinal", "pacu_hours",
  "NRS discharge cumulative logit mixed model", "proportional odds ratio", "all", NA_character_,
  "nausea", "nausea", "binary", "pacu_hours",
  "nausea logistic mixed model", "odds ratio", "all", NA_character_,
  "vomiting", "vomiting", "binary", "pacu_hours",
  "vomiting logistic mixed model", "odds ratio", "all", NA_character_,
  "antiemetic", "antiemetic", "binary", "pacu_hours",
  "antiemetic logistic mixed model", "odds ratio", "all", NA_character_,
  "delirium", "delirium", "ordinal_delirium", NA_character_,
  "delirium cumulative logit mixed model", "proportional odds ratio", "all", NA_character_,
  "piritramide_mg_any", "piritramide_mg", "binary", "pacu_hours",
  "any medication: logistic mixed model", "odds ratio", "all", "piritramide_mg",
  "piritramide_positive_binary", "piritramide_mg", "binary", "pacu_hours",
  "positive dose: binary fallback model", "odds ratio", "positive", "piritramide_mg",
  "dexketoprofen_mg_any", "dexketoprofen_mg", "binary", "pacu_hours",
  "any medication: logistic mixed model", "odds ratio", "all", "dexketoprofen_mg",
  "dexketoprofen_positive_binary", "dexketoprofen_mg", "binary", "pacu_hours",
  "positive dose: binary fallback model", "odds ratio", "positive", "dexketoprofen_mg",
  "ibuprofen_mg_any", "ibuprofen_mg", "binary", "pacu_hours",
  "any medication: logistic mixed model", "odds ratio", "all", "ibuprofen_mg",
  "ibuprofen_positive_binary", "ibuprofen_mg", "binary", "pacu_hours",
  "positive dose: binary fallback model", "odds ratio", "positive", "ibuprofen_mg",
  "paracetamol_mg_any", "paracetamol_mg", "binary", "pacu_hours",
  "any medication: logistic mixed model", "odds ratio", "all", "paracetamol_mg",
  "paracetamol_positive_binary", "paracetamol_mg", "binary", "pacu_hours",
  "positive dose: binary fallback model", "odds ratio", "positive", "paracetamol_mg",
  "metamizole_g_any", "metamizole_g", "binary", "pacu_hours",
  "any medication: logistic mixed model", "odds ratio", "all", "metamizole_g",
  "metamizole_positive_binary", "metamizole_g", "binary", "pacu_hours",
  "positive dose: binary fallback model", "odds ratio", "positive", "metamizole_g"
)

fit_by_type <- function(data, outcome, model_type, extra = NULL, interaction = NULL) {
  if (model_type == "gamma") {
    return(fit_gamma(data, outcome, extra = extra, interaction = interaction))
  }

  if (model_type == "binary") {
    return(tryCatch(
      fit_binary(data, outcome, extra = extra, interaction = interaction),
      error = function(e) fit_binary_lme4(data, outcome, extra = extra, interaction = interaction)
    ))
  }

  if (model_type == "ordinal") {
    return(fit_ordinal(data, outcome, extra = extra, interaction = interaction))
  }

  if (model_type == "ordinal_delirium") {
    return(fit_ordinal(
      data,
      outcome,
      extra = extra,
      interaction = interaction,
      collapse_sparse_surgery = TRUE
    ))
  }

  stop("Unknown model type: ", model_type)
}

interaction_p_value <- function(null_model, interaction_model) {
  test <- tryCatch(anova(null_model, interaction_model), error = function(e) NULL)

  if (is.null(test) || nrow(test) < 2) {
    return(NA_real_)
  }

  p_column <- intersect(c("Pr(>Chisq)", "Pr(>Chi)"), names(test))[1]

  if (is.na(p_column)) {
    return(NA_real_)
  }

  test[[p_column]][2]
}

subgroup_rows <- list()
subgroup_models <- list()

for (i in seq_len(nrow(endpoint_specs))) {
  spec <- endpoint_specs[i, ]
  extra <- if (is.na(spec$extra)) NULL else spec$extra

  model_data <- analysis_data

  if (spec$data_scope == "positive") {
    model_data <- model_data %>%
      filter(.data[[spec$positive_source]] > 0, !is.na(.data[[spec$outcome]]))
  }

  for (subgroup in subgroups) {
    fit_result <- tryCatch({
      null_model <- fit_by_type(model_data, spec$outcome, spec$model_type, extra = extra)
      interaction_model <- fit_by_type(
        model_data,
        spec$outcome,
        spec$model_type,
        extra = extra,
        interaction = subgroup
      )

      rows <- subgroup_contrast_rows(
        model = interaction_model,
        data = model_data,
        subgroup = subgroup,
        outcome = spec$reported_outcome,
        component = spec$component,
        population = "ITT",
        effect_scale = spec$effect_scale
      ) %>%
        mutate(
          p_value_interaction = interaction_p_value(null_model, interaction_model),
          note = case_when(
            n_control < 5 | n_ice_lolly < 5 ~ "very sparse subgroup level",
            !is.finite(subgroup_effect) | !is.finite(conf.low) | !is.finite(conf.high) ~
              "stratum-specific effect not estimable",
            TRUE ~ ""
          )
        )

      list(model = interaction_model, rows = rows)
    }, error = function(e) {
      counts <- model_data %>%
        count(level = .data[[subgroup]], treatment, name = "n") %>%
        tidyr::pivot_wider(names_from = treatment, values_from = n, values_fill = 0)

      rows <- tibble(level = as.character(levels(droplevels(model_data[[subgroup]])))) %>%
        left_join(counts, by = "level") %>%
        mutate(
          population = "ITT",
          outcome = spec$reported_outcome,
          component = spec$component,
          subgroup = subgroup,
          n_control = Control,
          n_ice_lolly = `Ice lolly`,
          effect_scale = spec$effect_scale,
          subgroup_effect = NA_real_,
          conf.low = NA_real_,
          conf.high = NA_real_,
          p.value = NA_real_,
          p_value_interaction = NA_real_,
          note = conditionMessage(e)
        ) %>%
        select(
          population, outcome, component, subgroup, level,
          n_control, n_ice_lolly, effect_scale,
          subgroup_effect, conf.low, conf.high, p.value,
          p_value_interaction, note
        )

      list(model = NULL, rows = rows)
    })

    model_name <- paste(spec$reported_outcome, spec$component, subgroup, sep = "__")
    subgroup_models[[model_name]] <- fit_result$model
    subgroup_rows[[length(subgroup_rows) + 1]] <- fit_result$rows
  }
}

subgroup_results <- bind_rows(subgroup_rows)

write_csv_safe(subgroup_results, file.path(paths$tables, "table_07_exploratory_subgroup_interactions.csv"))
saveRDS(subgroup_models, file.path(paths$models, "models_subgroup_interactions.rds"))
