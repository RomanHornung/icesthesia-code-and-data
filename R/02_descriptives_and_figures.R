# Participant counts and baseline characteristics for the revised report.
#
# The historical filename is retained to keep the execution order stable. This
# script now writes the participant-flow summary and Table 1 source values.
# The complete ITT and PP outcome tables are produced once, by script 08.

source(file.path("R", "00_setup.R"))
analysis_data <- readRDS(file.path(paths$derived, "analysis_data.rds"))

# The input dataset contains retained analysis records only. Exclusions before
# this dataset was assembled cannot be inferred from it; see the README for
# the distinction between this summary and the clinical trial flow diagram.
participant_flow <- analysis_data %>%
  summarise(
    population = "ITT", n = n(), days = n_distinct(study_day),
    control = sum(treatment == "Control"),
    ice_lolly = sum(treatment == "Ice lolly"),
    ice_consumed = sum(ice_consumed == 1), per_protocol = sum(pp_eligible)
  )
write_csv_safe(participant_flow, file.path(paths$tables, "table_01_participant_flow.csv"))

# Weight and height are supplied in bands, not as individual measurements.
# Group whole bands without inventing patient-level values at a boundary.
band_upper <- function(x) {
  vapply(strsplit(as.character(x), "-", fixed = TRUE), function(bounds) {
    if (anyNA(bounds)) return(NA_real_)
    max(as.numeric(bounds))
  }, numeric(1))
}
weight_upper <- band_upper(analysis_data$weight_group)
height_upper <- band_upper(analysis_data$height_group)
baseline_data <- analysis_data %>%
  mutate(
    table_weight = factor(case_when(
      weight_upper <= 40 ~ "<=40", weight_upper <= 90 ~ "41-90",
      weight_upper > 90 ~ ">90", TRUE ~ NA_character_
    ), levels = c("<=40", "41-90", ">90")),
    table_height = factor(case_when(
      height_upper < 150 ~ "<150", height_upper < 170 ~ "150-169",
      height_upper >= 170 ~ ">=170", TRUE ~ NA_character_
    ), levels = c("<150", "150-169", ">=170"))
  )

# Preserve numerical source values separately from the one-decimal percentage
# display. The combined workbook also provides a ready-to-read Table1 sheet.
format_count <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
format_number <- function(x) {
  sub("\\.?0+$", "", formatC(x, digits = 2, format = "f", decimal.mark = "."))
}
rows <- list()
groups <- c("Overall", "Control", "Ice lolly")

add_category <- function(id, variable, label, level, display_level = level) {
  for (g in groups) {
    data <- if (g == "Overall") baseline_data else filter(baseline_data, treatment == g)
    x <- data[[variable]]
    denominator <- sum(!is.na(x))
    count <- sum(as.character(x) == level, na.rm = TRUE)
    percent <- if (denominator) 100 * count / denominator else NA_real_
    rows[[length(rows) + 1L]] <<- tibble(
      row_id = id, row_label = paste0(label, ": ", display_level), group = g,
      group_n = nrow(data), n = count, denominator = denominator, percent = percent,
      median = NA_real_, q1 = NA_real_, q3 = NA_real_,
      display_value = paste0(format_count(count), " (",
                             formatC(percent, digits = 1, format = "f", decimal.mark = "."), "%)")
    )
  }
}

add_continuous <- function(id, variable, label, ordinal = FALSE) {
  for (g in groups) {
    data <- if (g == "Overall") baseline_data else filter(baseline_data, treatment == g)
    x <- data[[variable]]
    x <- x[!is.na(x)]
    q <- if (length(x)) as.numeric(quantile(x, c(0.25, 0.5, 0.75),
                                            type = if (ordinal) 1L else 7L)) else rep(NA_real_, 3)
    rows[[length(rows) + 1L]] <<- tibble(
      row_id = id, row_label = label, group = g, group_n = nrow(data),
      n = length(x), denominator = nrow(data), percent = NA_real_,
      median = q[2], q1 = q[1], q3 = q[3],
      display_value = paste0(format_number(q[2]), " (", format_number(q[1]), ", ",
                             format_number(q[3]), ")")
    )
  }
}

add_category("sex_male", "sex", "Sex", "Male")
add_category("sex_female", "sex", "Sex", "Female")
for (level in c("<=12", "13-75", ">=76")) {
  add_category(paste0("age_", level), "age_group", "Age group (years)", level)
}
for (level in levels(baseline_data$table_weight)) {
  add_category(paste0("weight_", level), "table_weight", "Weight (kg)", level)
}
for (level in levels(baseline_data$table_height)) {
  add_category(paste0("height_", level), "table_height", "Height (cm)", level)
}
for (level in levels(baseline_data$asa_original)) {
  add_category(paste0("asa_", level), "asa_original", "ASA classification", level)
}
for (level in levels(baseline_data$surgery_type)) {
  add_category(paste0("surgery_", level), "surgery_type", "Surgical specialty", level)
}

# The fitted anaesthesia covariate combines spinal and peripheral nerve-block
# anaesthesia (raw AN_Mode 5 and 6). Regional add-on blocks are a separate field.
anaesthesia_labels <- c(TIVA = "TIVA", CIVA = "CIVA", Balanced = "Volatile anaesthesia",
                       IVA = "MAC", Regional = "Spinal / peripheral nerve block")
for (level in names(anaesthesia_labels)) {
  add_category(paste0("anaesthesia_", level), "anesthesia_type", "Anaesthesia type",
               level, anaesthesia_labels[[level]])
}
add_category("epidural", "epidural", "Anaesthesia", "1", "Epidural catheter")
add_category("regional_add_on", "regional_add_on", "Anaesthesia", "1", "Regional add-on block")
add_continuous("duration", "anesthesia_minutes", "Duration of anaesthesia (min), median (Q1, Q3)")
add_continuous("nrs_admission", "nrs_admission", "NRS at PACU admission, median (Q1, Q3)", ordinal = TRUE)

baseline_table <- bind_rows(rows)
stopifnot(nrow(baseline_table) == 84L, n_distinct(baseline_table$row_id) == 28L)
write_csv_safe(baseline_table, file.path(paths$tables, "table_02_baseline_characteristics.csv"))
