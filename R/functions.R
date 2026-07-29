# Helper functions for the Icesthesia analysis.
#
# The functions in this file are intentionally separated from the analysis
# scripts. This keeps the numbered scripts focused on the actual data analysis
# while avoiding repeated code.

# Convert Excel-imported clock times to minutes after midnight.
clock_minutes <- function(x) {
  x_time <- as.POSIXlt(x, tz = "UTC")
  x_time$hour * 60 + x_time$min + x_time$sec / 60
}

# Save a data frame as CSV with empty fields for missing values.
#
# CSV files are written in an international, publication-friendly format:
# comma as column separator and dot as decimal mark. For easy viewing in Excel,
# the pipeline additionally creates an .xlsx workbook with the same tables.
write_csv_safe <- function(x, path) {
  readr::write_csv(as.data.frame(x), path, na = "")
}

# Export all public result CSV tables into one Excel workbook.
#
# The CSV files remain the machine-readable/publication-friendly source tables.
# The workbook is provided only as a convenient way to inspect the same tables
# in Excel without fighting locale-specific CSV import settings.
write_results_workbook <- function(table_folder, workbook_path) {
  table_files <- list.files(
    table_folder,
    pattern = "^table_[0-9]{2}_.+\\.csv$",
    full.names = TRUE
  )

  table_files <- sort(table_files)

  tables <- lapply(table_files, readr::read_csv, show_col_types = FALSE)

  sheet_names <- c(
    "participant_flow",
    "baseline",
    "outcome_descriptives",
    "main_itt",
    "main_per_protocol",
    "primary_icc",
    "subgroups"
  )

  names(tables) <- sheet_names

  writexl::write_xlsx(tables, workbook_path)
}

# Calculate a normal-approximation confidence interval for a mean.
mean_ci <- function(x, level = 0.95) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 2) {
    return(c(mean = mean(x), lower = NA_real_, upper = NA_real_))
  }

  alpha <- 1 - level
  se <- stats::sd(x) / sqrt(n)
  t_value <- stats::qt(1 - alpha / 2, df = n - 1)

  c(
    mean = mean(x),
    lower = mean(x) - t_value * se,
    upper = mean(x) + t_value * se
  )
}

# Baseline covariates prespecified in the SAP.
fixed_covariates <- c(
  "age_group",
  "sex",
  "asa",
  "surgery_type",
  "anesthesia_hours",
  "anesthesia_type"
)

# Build the common mixed-model formula.
#
# outcome: name of the outcome variable.
# extra: optional additional covariates, e.g. PACU duration for pain outcomes.
# interaction: optional subgroup variable for treatment-by-subgroup analyses.
model_formula <- function(outcome, extra = NULL, interaction = NULL) {
  if (is.null(interaction)) {
    rhs <- c("treatment", fixed_covariates, extra)
  } else {
    rhs <- c(
      paste0("treatment * ", interaction),
      setdiff(fixed_covariates, interaction),
      extra
    )
  }

  stats::as.formula(
    paste(outcome, "~", paste(unique(rhs), collapse = " + "), "+ (1 | study_day)")
  )
}

# Fit a linear mixed model.
fit_lmm <- function(data, outcome, extra = NULL, interaction = NULL) {
  lme4::lmer(
    model_formula(outcome, extra = extra, interaction = interaction),
    data = data,
    REML = FALSE,
    na.action = na.omit,
    control = lme4::lmerControl(optimizer = "bobyqa")
  )
}

# Fit a logistic mixed model.
fit_binary <- function(data, outcome, extra = NULL, interaction = NULL) {
  glmmTMB::glmmTMB(
    model_formula(outcome, extra = extra, interaction = interaction),
    data = data,
    family = binomial(),
    na.action = na.omit,
    control = glmmTMB::glmmTMBControl(
      optCtrl = list(iter.max = 10000, eval.max = 10000)
    )
  )
}

# Fit a gamma mixed model with log link.
fit_gamma <- function(data, outcome, extra = NULL, interaction = NULL) {
  glmmTMB::glmmTMB(
    model_formula(outcome, extra = extra, interaction = interaction),
    data = data,
    family = Gamma(link = "log"),
    na.action = na.omit,
    control = glmmTMB::glmmTMBControl(
      optCtrl = list(iter.max = 10000, eval.max = 10000)
    )
  )
}

# Convert numeric or character values to an ordered factor.
#
# Numeric values are ordered by their numeric value. Character values keep the
# level order supplied by the factor, if present. This is used for ordinal NRS
# scores and medication-dose categories.
as_ordered_outcome <- function(x) {
  if (is.numeric(x)) {
    levels_numeric <- sort(unique(x[!is.na(x)]))
    return(ordered(as.character(x), levels = as.character(levels_numeric)))
  }

  if (is.factor(x)) {
    return(ordered(as.character(x), levels = levels(x)))
  }

  ordered(x)
}

# Fit a cumulative logit mixed model with a single proportional-odds treatment
# effect. This gives one treatment estimate and one p-value for the ordered
# outcome, instead of separate tests for every adjacent dose/score category.
fit_ordinal <- function(data,
                        outcome,
                        extra = NULL,
                        interaction = NULL,
                        collapse_sparse_surgery = FALSE) {
  data[[outcome]] <- as_ordered_outcome(data[[outcome]])

  if (collapse_sparse_surgery) {
    data <- data %>%
      mutate(
        surgery_type = factor(
          if_else(
            surgery_type == "Other / plastic",
            "Orthopedic / trauma",
            as.character(surgery_type)
          ),
          levels = c("Orthopedic / trauma", "Neurosurgery", "ENT")
        )
      )
  }

  ordinal::clmm(
    model_formula(outcome, extra = extra, interaction = interaction),
    data = data,
    link = "logit",
    Hess = TRUE,
    nAGQ = 1,
    na.action = na.omit
  )
}

# Fit a logistic mixed model with lme4 as a fallback for difficult sparse data.
fit_binary_lme4 <- function(data, outcome, extra = NULL, interaction = NULL) {
  lme4::glmer(
    model_formula(outcome, extra = extra, interaction = interaction),
    data = data,
    family = binomial(),
    na.action = na.omit,
    control = lme4::glmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 200000)
    )
  )
}

# Fit a reduced logistic mixed model for sparse fallback analyses.
#
# This is used only when the full SAP-adjusted positive-dose model is not
# estimable. It retains the treatment effect, PACU duration, and study-day
# clustering, but avoids unstable sparse covariate strata.
fit_binary_reduced <- function(data, outcome, extra = "pacu_hours", interaction = NULL) {
  rhs <- if (is.null(interaction)) {
    c("treatment", extra)
  } else {
    c(paste0("treatment * ", interaction), extra)
  }

  formula <- stats::as.formula(
    paste(outcome, "~", paste(unique(rhs), collapse = " + "), "+ (1 | study_day)")
  )

  lme4::glmer(
    formula,
    data = data,
    family = binomial(),
    na.action = na.omit,
    control = lme4::glmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 200000)
    )
  )
}

# Extract the treatment effect from a model.
#
# exponentiate = TRUE is used for odds ratios and mean ratios.
treatment_row <- function(model,
                          exponentiate = FALSE,
                          outcome = NA_character_,
                          component = NA_character_,
                          population = "ITT") {
  term <- "treatmentIce lolly"

  if (inherits(model, "clmm")) {
    coefficient_table <- coef(summary(model))

    if (!term %in% rownames(coefficient_table)) {
      return(tibble())
    }

    estimate <- coefficient_table[term, "Estimate"]
    se <- coefficient_table[term, "Std. Error"]
    ci <- c(estimate - 1.96 * se, estimate + 1.96 * se)

    if (exponentiate) {
      estimate <- exp(estimate)
      ci <- exp(ci)
    }

    return(
      tibble(
        population = population,
        outcome = outcome,
        component = component,
        effect_scale = if_else(exponentiate, "proportional odds ratio", "log odds"),
        estimate = estimate,
        conf.low = ci[1],
        conf.high = ci[2],
        p.value = coefficient_table[term, "Pr(>|z|)"],
        n = stats::nobs(model)
      )
    )
  }

  tidy_model <- broom.mixed::tidy(
    model,
    effects = "fixed",
    conf.int = TRUE,
    exponentiate = exponentiate
  )

  if (!"p.value" %in% names(tidy_model)) {
    tidy_model$p.value <- 2 * stats::pnorm(abs(tidy_model$statistic), lower.tail = FALSE)
  }

  tidy_model <- tidy_model[tidy_model$term == term, , drop = FALSE]

  if (nrow(tidy_model) == 0) {
    return(tibble())
  }

  tidy_model %>%
    transmute(
      population = .env$population,
      outcome = .env$outcome,
      component = .env$component,
      effect_scale = if_else(exponentiate, "ratio", "difference"),
      estimate = estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      p.value = p.value,
      n = stats::nobs(model)
    )
}

# Convert a fitted gamma model to an adjusted response-scale mean difference.
gamma_mean_difference <- function(model, outcome, component, population) {
  marginal_means <- emmeans::emmeans(model, ~ treatment)
  contrast_table <- as.data.frame(
    emmeans::contrast(
      emmeans::regrid(marginal_means, transform = "response"),
      method = "revpairwise",
      infer = TRUE
    )
  )

  tibble(
    population = population,
    outcome = outcome,
    component = component,
    effect_scale = "difference",
    estimate = contrast_table$estimate,
    conf.low = contrast_table$asymp.LCL,
    conf.high = contrast_table$asymp.UCL,
    p.value = contrast_table$p.value,
    n = stats::nobs(model)
  )
}

# Fit an ordinal medication-dose model including zero as the lowest category.
#
# This replaces the initially considered two-part gamma approach for medication
# dose variables, because the observed doses are highly discrete and often
# dominated by one standard dose. A cumulative logit model treats the complete
# ordered dose scale, including no administration, in one model.
fit_ordinal_dose <- function(data, outcome, extra = "pacu_hours", interaction = NULL) {
  fit_ordinal(
    data = data,
    outcome = outcome,
    extra = extra,
    interaction = interaction,
    collapse_sparse_surgery = FALSE
  )
}

# Check whether an extracted result row contains a usable estimate, confidence
# interval, and p-value.
is_result_estimable <- function(result_row) {
  nrow(result_row) > 0 &&
    is.finite(result_row$estimate[1]) &&
    is.finite(result_row$conf.low[1]) &&
    is.finite(result_row$conf.high[1]) &&
    is.finite(result_row$p.value[1])
}

# Fit the medication analysis as a two-part model.
#
# Part 1 models whether the medication was administered at all.
# Part 2 models dose intensity among patients with a positive dose. The primary
# positive-dose model is ordinal. If the ordinal model is not estimable, a
# clinically collapsed binary positive-dose model is used as a fallback.
fit_medication_two_part <- function(data,
                                    outcome,
                                    positive_ordinal,
                                    positive_binary,
                                    population = "ITT",
                                    interaction = NULL) {
  any_name <- paste0(outcome, "_any")
  data[[any_name]] <- as.integer(data[[outcome]] > 0)

  any_model <- fit_binary(
    data = data,
    outcome = any_name,
    extra = "pacu_hours",
    interaction = interaction
  )

  any_result <- treatment_row(
    any_model,
    exponentiate = TRUE,
    outcome = outcome,
    component = "any medication: logistic mixed model",
    population = population
  )

  positive_data <- data %>%
    filter(.data[[outcome]] > 0, !is.na(.data[[positive_ordinal]]))

  positive_model <- NULL
  positive_result <- tibble()
  positive_model_type <- "not fitted"

  if (nrow(positive_data) >= 20 && n_distinct(positive_data[[positive_ordinal]]) >= 2) {
    ordinal_fit <- tryCatch(
      fit_ordinal_dose(
        data = positive_data,
        outcome = positive_ordinal,
        extra = "pacu_hours",
        interaction = interaction
      ),
      error = function(e) NULL
    )

    if (!is.null(ordinal_fit)) {
      ordinal_result <- treatment_row(
        ordinal_fit,
        exponentiate = TRUE,
        outcome = outcome,
        component = "positive dose: cumulative logit mixed model",
        population = population
      )

      if (is_result_estimable(ordinal_result)) {
        positive_model <- ordinal_fit
        positive_result <- ordinal_result
        positive_model_type <- "ordinal"
      }
    }
  }

  if (!is_result_estimable(positive_result)) {
    positive_data <- positive_data %>%
      filter(!is.na(.data[[positive_binary]]))

    if (nrow(positive_data) >= 20 && n_distinct(positive_data[[positive_binary]]) == 2) {
      binary_fit <- tryCatch(
        fit_binary(
          data = positive_data,
          outcome = positive_binary,
          extra = "pacu_hours",
          interaction = interaction
        ),
        error = function(e) tryCatch(
          fit_binary_lme4(
            data = positive_data,
            outcome = positive_binary,
            extra = "pacu_hours",
            interaction = interaction
          ),
          error = function(e2) NULL
        )
      )

      if (!is.null(binary_fit)) {
        binary_result <- treatment_row(
          binary_fit,
          exponentiate = TRUE,
          outcome = outcome,
          component = "positive dose: binary fallback model",
          population = population
        )

        if (is_result_estimable(binary_result)) {
          positive_model <- binary_fit
          positive_result <- binary_result
          positive_model_type <- "binary fallback"
        }
      }
    }
  }

  if (!is_result_estimable(positive_result)) {
    positive_data <- positive_data %>%
      filter(!is.na(.data[[positive_binary]]))

    if (nrow(positive_data) >= 20 && n_distinct(positive_data[[positive_binary]]) == 2) {
      reduced_fit <- tryCatch(
        fit_binary_reduced(
          data = positive_data,
          outcome = positive_binary,
          extra = "pacu_hours",
          interaction = interaction
        ),
        error = function(e) NULL
      )

      if (!is.null(reduced_fit)) {
        reduced_result <- treatment_row(
          reduced_fit,
          exponentiate = TRUE,
          outcome = outcome,
          component = "positive dose: binary fallback model with reduced adjustment",
          population = population
        )

        if (is_result_estimable(reduced_result)) {
          positive_model <- reduced_fit
          positive_result <- reduced_result
          positive_model_type <- "binary fallback reduced"
        }
      }
    }
  }

  if (!is_result_estimable(positive_result)) {
    positive_result <- tibble(
      population = population,
      outcome = outcome,
      component = "positive dose: not estimable",
      effect_scale = "odds ratio",
      estimate = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_,
      n = nrow(positive_data)
    )
  }

  list(
    any_model = any_model,
    positive_model = positive_model,
    positive_model_type = positive_model_type,
    results = bind_rows(any_result, positive_result)
  )
}

# Extract model-based treatment contrasts within subgroup levels.
#
# This is used for the subgroup table so that interaction p-values can be read
# together with the estimated treatment effect in each subgroup level.
subgroup_contrast_rows <- function(model,
                                   data,
                                   subgroup,
                                   outcome,
                                   component,
                                   population,
                                   effect_scale) {
  counts <- data %>%
    count(level = .data[[subgroup]], treatment, name = "n") %>%
    tidyr::pivot_wider(names_from = treatment, values_from = n, values_fill = 0)

  contrasts <- tryCatch({
    emm <- emmeans::emmeans(model, specs = "treatment", by = subgroup)
    ctr <- as.data.frame(summary(
      emmeans::contrast(emm, method = "revpairwise"),
      infer = TRUE,
      type = "response"
    ))

    names(ctr)[names(ctr) == subgroup] <- "level"

    estimate_column <- intersect(c("estimate", "odds.ratio", "ratio"), names(ctr))[1]
    lower_column <- intersect(c("lower.CL", "asymp.LCL"), names(ctr))[1]
    upper_column <- intersect(c("upper.CL", "asymp.UCL"), names(ctr))[1]

    out <- ctr %>%
      transmute(
        level = as.character(level),
        subgroup_effect = .data[[estimate_column]],
        conf.low = .data[[lower_column]],
        conf.high = .data[[upper_column]],
        p.value = p.value
      )

    if (inherits(model, "clmm")) {
      out <- out %>%
        mutate(
          subgroup_effect = exp(subgroup_effect),
          conf.low = exp(conf.low),
          conf.high = exp(conf.high)
        )
    }

    out
  }, error = function(e) {
    tibble(
      level = as.character(levels(droplevels(data[[subgroup]]))),
      subgroup_effect = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_
    )
  })

  contrasts %>%
    left_join(counts, by = "level") %>%
    mutate(
      population = population,
      outcome = outcome,
      component = component,
      subgroup = subgroup,
      effect_scale = effect_scale,
      n_control = Control,
      n_ice_lolly = `Ice lolly`
    ) %>%
    select(
      population, outcome, component, subgroup, level,
      n_control, n_ice_lolly, effect_scale,
      subgroup_effect, conf.low, conf.high, p.value
    )
}

# Return TRUE/FALSE/NA for model singularity without stopping the analysis.
model_singularity <- function(model) {
  if (inherits(model, "merMod")) {
    return(lme4::isSingular(model, tol = 1e-4))
  }

  if (inherits(model, "glmmTMB")) {
    return(performance::check_singularity(model, tolerance = 1e-4))
  }

  NA
}

# Create a simple model-audit row.
model_audit_row <- function(model_object, model_name) {
  if (inherits(model_object, "merMod")) {
    convergence_message <- model_object@optinfo$conv$lme4$messages
    convergence_message <- if (is.null(convergence_message)) "" else paste(convergence_message, collapse = " | ")
    n <- nrow(model_object@frame)
  } else if (inherits(model_object, "glmmTMB")) {
    convergence_message <- if (isTRUE(model_object$sdr$pdHess) && model_object$fit$convergence == 0) {
      ""
    } else {
      paste(
        "convergence code", model_object$fit$convergence,
        "; positive-definite Hessian:", model_object$sdr$pdHess
      )
    }
    n <- stats::nobs(model_object)
  } else if (inherits(model_object, "clmm")) {
    convergence_message <- paste(model_object$info$messages %||% "", collapse = " | ")
    n <- model_object$dims$n
  } else {
    convergence_message <- "model class not audited"
    n <- NA_integer_
  }

  tibble(
    model = model_name,
    model_class = class(model_object)[1],
    n = n,
    singular = model_singularity(model_object),
    convergence_message = convergence_message
  )
}

# Null coalescing helper used in the model audit.
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Flatten nested lists of models, preserving informative names.
flatten_models <- function(x, prefix = "") {
  out <- list()

  if (inherits(x, "merMod") || inherits(x, "glmmTMB") || inherits(x, "clmm")) {
    out[[prefix]] <- x
    return(out)
  }

  if (is.list(x)) {
    for (nm in names(x)) {
      child_name <- if (nzchar(prefix)) paste(prefix, nm, sep = "/") else nm
      out <- c(out, flatten_models(x[[nm]], child_name))
    }
  }

  out
}

# Save histogram and normal Q-Q plot for an observed continuous variable.
save_distribution_plots <- function(data, variable, label, folder) {
  plot_data <- data %>%
    select(treatment, value = all_of(variable)) %>%
    filter(!is.na(value))

  histogram <- ggplot(plot_data, aes(x = value, fill = treatment)) +
    geom_histogram(bins = 50, alpha = 0.60, position = "identity") +
    facet_wrap(~ treatment, ncol = 1, scales = "free_y") +
    theme_minimal(base_size = 11) +
    labs(x = label, y = "Patients", fill = "Group")

  qq_plot <- ggplot(plot_data, aes(sample = value)) +
    stat_qq(alpha = 0.50) +
    stat_qq_line() +
    facet_wrap(~ treatment) +
    theme_minimal(base_size = 11) +
    labs(x = "Theoretical quantiles", y = label)

  ggsave(file.path(folder, paste0(variable, "_distribution.png")), histogram,
         width = 8, height = 6, dpi = 180)
  ggsave(file.path(folder, paste0(variable, "_qq.png")), qq_plot,
         width = 8, height = 4.5, dpi = 180)
}

# Save a category-frequency plot for an ordinal outcome.
#
# This is more meaningful than a normal Q-Q plot for outcomes such as NRS pain
# scores or medication doses, where only a limited set of ordered categories is
# observed.
save_ordinal_distribution_plot <- function(data, variable, label, folder) {
  plot_data <- data %>%
    mutate(category = as_ordered_outcome(.data[[variable]])) %>%
    filter(!is.na(category)) %>%
    count(treatment, category, name = "n") %>%
    group_by(treatment) %>%
    mutate(percent = 100 * n / sum(n)) %>%
    ungroup()

  bar_plot <- ggplot(plot_data, aes(x = category, y = percent, fill = treatment)) +
    geom_col(position = "dodge") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = label, y = "Patients (%)", fill = "Group")

  ggsave(file.path(folder, paste0(variable, "_ordinal_distribution.png")), bar_plot,
         width = 9, height = 5.5, dpi = 180)
}

# Save residual-vs-fitted and residual Q-Q plots for fitted continuous models.
save_residual_plots <- function(model, name, folder) {
  if (!(inherits(model, "merMod") || inherits(model, "glmmTMB"))) {
    return(invisible(NULL))
  }

  residual_data <- tibble(
    fitted = as.numeric(stats::fitted(model)),
    residual = as.numeric(stats::resid(model, type = "pearson"))
  ) %>%
    mutate(standardized_residual = as.numeric(scale(residual)))

  residual_plot <- ggplot(residual_data, aes(x = fitted, y = residual)) +
    geom_point(alpha = 0.30) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_smooth(se = FALSE) +
    theme_minimal(base_size = 11) +
    labs(x = "Fitted values", y = "Pearson residuals")

  qq_plot <- ggplot(residual_data, aes(sample = standardized_residual)) +
    stat_qq(alpha = 0.50) +
    stat_qq_line() +
    theme_minimal(base_size = 11) +
    labs(x = "Theoretical quantiles", y = "Standardized residuals")

  ggsave(file.path(folder, paste0(name, "_residuals_vs_fitted.png")), residual_plot,
         width = 7, height = 5, dpi = 180)
  ggsave(file.path(folder, paste0(name, "_residual_qq.png")), qq_plot,
         width = 6, height = 5, dpi = 180)
}

# Count events by treatment for binary and ordinal outcomes.
event_table <- function(data, outcome) {
  data %>%
    mutate(level = as.character(.data[[outcome]])) %>%
    count(treatment, level, name = "n") %>%
    group_by(treatment) %>%
    mutate(percent = 100 * n / sum(n)) %>%
    ungroup() %>%
    mutate(outcome = outcome, .before = 1)
}
