# Export a reader-friendly subgroup interaction workbook.
#
# The raw subgroup CSV remains the complete machine-readable result table.
# This script creates an additional rounded Excel workbook that is easier to
# share as supplementary raw data or in an online repository.

source(file.path("R", "00_setup.R"))

subgroup_raw <- readr::read_csv(
  file.path(paths$tables, "table_07_exploratory_subgroup_interactions.csv"),
  show_col_types = FALSE
)

# Translate internal variable names to labels that can be read without checking
# the R code. The statistical values are rounded for presentation.
subgroup_clean <- subgroup_raw %>%
  mutate(
    Outcome = dplyr::recode(
      outcome,
      pacu_minutes = "PACU duration",
      nrs_admission = "NRS pain at PACU admission",
      nrs_discharge = "NRS pain at PACU discharge",
      nausea = "Nausea",
      vomiting = "Vomiting",
      antiemetic = "Antiemetic administration",
      delirium = "Delirium score",
      piritramide_mg = "Piritramide dose",
      dexketoprofen_mg = "Dexketoprofen dose",
      ibuprofen_mg = "Ibuprofen dose",
      paracetamol_mg = "Paracetamol dose",
      metamizole_g = "Metamizole dose"
    ),
    Subgroup = dplyr::recode(
      subgroup,
      age_group = "Age group",
      sex = "Sex",
      asa = "ASA class",
      surgery_type = "Surgery type",
      anesthesia_type = "Anaesthesia type"
    ),
    `Model component` = dplyr::recode(
      component,
      `primary PACU gamma-log model` = "PACU gamma-log mixed model",
      `NRS admission cumulative logit mixed model` = "NRS admission cumulative logit mixed model",
      `NRS discharge cumulative logit mixed model` = "NRS discharge cumulative logit mixed model",
      `nausea logistic mixed model` = "Nausea logistic mixed model",
      `vomiting logistic mixed model` = "Vomiting logistic mixed model",
      `antiemetic logistic mixed model` = "Antiemetic logistic mixed model",
      `delirium cumulative logit mixed model` = "Delirium cumulative logit mixed model",
      `any medication: logistic mixed model` = "Two-part, part 1: any medication",
      `positive dose: binary fallback model` = "Two-part, part 2: positive dose, binary fallback"
    ),
    `Effect measure` = dplyr::recode(
      effect_scale,
      ratio = "Ratio / odds ratio",
      `odds ratio` = "Odds ratio",
      `proportional odds ratio` = "Proportional odds ratio"
    ),
    `Flag / note` = dplyr::if_else(is.na(note), "", note),

    # The raw CSV intentionally keeps all numerical model output. In this
    # reader-friendly workbook, stratum-specific estimates that were flagged as
    # not reliably estimable are left blank rather than showing unstable large
    # or infinite values.
    not_estimable_note = !is.na(note) & note == "stratum-specific effect not estimable",
    `Treatment effect` = dplyr::if_else(
      not_estimable_note,
      NA_real_,
      round(subgroup_effect, 3)
    ),
    `95% CI lower` = dplyr::if_else(
      not_estimable_note,
      NA_real_,
      round(conf.low, 3)
    ),
    `95% CI upper` = dplyr::if_else(
      not_estimable_note,
      NA_real_,
      round(conf.high, 3)
    ),
    `Stratum-specific p-value` = dplyr::if_else(
      not_estimable_note,
      NA_real_,
      round(p.value, 3)
    ),
    `Interaction p-value` = round(p_value_interaction, 3)
  ) %>%
  transmute(
    Population = population,
    Outcome,
    `Model component`,
    Subgroup,
    `Subgroup level` = level,
    `Control n` = n_control,
    `Ice-lolly n` = n_ice_lolly,
    `Effect measure`,
    `Treatment effect`,
    `95% CI lower`,
    `95% CI upper`,
    `Stratum-specific p-value`,
    `Interaction p-value`,
    `Flag / note`
  )

codebook <- tibble::tribble(
  ~Column, ~Description,
  "Population", "Analysis population. Current subgroup table contains intention-to-treat results.",
  "Outcome", "Outcome analysed in the treatment-by-subgroup model.",
  "Model component", "Model component used for the corresponding outcome.",
  "Subgroup", "Prespecified subgroup variable.",
  "Subgroup level", "Level/stratum of the subgroup variable.",
  "Control n", "Number of control patients contributing to this stratum-specific row.",
  "Ice-lolly n", "Number of ice-lolly patients contributing to this stratum-specific row.",
  "Effect measure", "Scale of the treatment effect estimate.",
  "Treatment effect", "Stratum-specific treatment effect. Values below 1 generally indicate lower outcome odds, lower proportional odds, or shorter PACU duration in the ice-lolly group, depending on the model.",
  "95% CI lower", "Lower limit of the 95% confidence interval for the stratum-specific treatment effect.",
  "95% CI upper", "Upper limit of the 95% confidence interval for the stratum-specific treatment effect.",
  "Stratum-specific p-value", "P-value for the treatment effect within this subgroup level.",
  "Interaction p-value", "Global treatment-by-subgroup interaction p-value for this outcome and subgroup. Repeated for all levels of the same interaction.",
  "Flag / note", "Blank if no special issue was flagged. 'stratum-specific effect not estimable' means the row did not yield a reliable stratum-specific estimate. 'very sparse subgroup level' means fewer than five patients contributed to at least one treatment arm in that subgroup stratum."
)

writexl::write_xlsx(
  list(
    subgroup_interactions = subgroup_clean,
    codebook = codebook
  ),
  file.path(paths$tables, "full_subgroup_interaction_table.xlsx")
)
