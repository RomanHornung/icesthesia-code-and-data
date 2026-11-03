# Create matching manuscript outcome-summary tables for ITT and PP.
#
# The same script is used for real and synthetic data. Input and output paths
# are defined by 00_setup.R; this script never reads another analysis folder.
# It can be run on its own after 01_prepare_data.R, without re-fitting models.

source(file.path("R", "00_setup.R"))

input_file <- file.path(paths$derived, "analysis_data.rds")

if (!file.exists(input_file)) {
  stop("Run R/01_prepare_data.R before creating the outcome-summary table.")
}

analysis_data <- readRDS(input_file)

# Keep counts separate from presentation strings so the exported results retain
# their numerical precision and can also be used for other table layouts.
format_count <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

format_quantile <- function(x) {
  if (is.na(x)) return("Not available")
  value <- formatC(x, digits = 2, format = "f", decimal.mark = ".")
  sub("\\.?0+$", "", value)
}

# Use one summary function for both populations, so the outcomes, quantiles,
# denominators and formatting are identical apart from the patient selection.
summarise_population <- function(data) {
  groups <- c("Control", "Ice lolly")
  group_n <- vapply(groups, function(g) sum(data$treatment == g), integer(1))
  stopifnot(all(group_n > 0L), sum(group_n) == nrow(data))
  summary_rows <- list()

  add_median_rows <- function(id, outcome, label, recipients_only = FALSE,
                              ordinal = FALSE) {
    for (g in groups) {
      x <- data[[outcome]][data$treatment == g]
      x <- x[!is.na(x)]

      # Medication-dose summaries describe only patients who received the drug.
      # The immediately preceding administration row gives the recipient count.
      if (recipients_only) x <- x[x > 0]

      # Empirical category quantiles avoid interpolated, unobserved NRS scores.
      # Duration and dose summaries use R's usual type-7 numeric quantiles.
      quantile_type <- if (ordinal) 1L else 7L
      q <- if (length(x)) {
        as.numeric(stats::quantile(x, c(0.25, 0.5, 0.75), type = quantile_type))
      } else {
        rep(NA_real_, 3)
      }

      value <- if (length(x)) {
        paste0(format_quantile(q[2]), " [", format_quantile(q[1]), ", ",
               format_quantile(q[3]), "]")
      } else {
        "Not available"
      }

      # Show actual NRS sample sizes in the cells when observations are missing.
      if (!recipients_only && length(x) < group_n[[g]]) {
        value <- paste0(value, "; n = ", format_count(length(x)))
      }

      summary_rows[[length(summary_rows) + 1L]] <<- tibble::tibble(
        row_id = id, outcome = outcome, summary = "median [Q1, Q3]",
        scope = if (recipients_only) "recipients" else "all available patients",
        row_label = label, treatment = g, group_n = group_n[[g]],
        n_observed = length(x), events = NA_integer_, percent = NA_real_,
        median = q[2], q1 = q[1], q3 = q[3], display_value = value
      )
    }
  }

  add_event_rows <- function(id, outcome, label, category = NULL) {
    for (g in groups) {
      x <- data[[outcome]][data$treatment == g]
      x <- x[!is.na(x)]

      # The denominator is available observations, not the number of events.
      # Delirium categories are counted directly from the ordered factor.
      n <- length(x)
      events <- if (is.null(category)) sum(x > 0) else sum(as.character(x) == category)
      percent <- if (n > 0L) 100 * events / n else NA_real_

      value <- if (n > 0L) {
        paste0(format_count(events), "/", format_count(n), " (",
               formatC(percent, digits = 1, format = "f", decimal.mark = "."), "%)")
      } else {
        "Not available"
      }

      summary_rows[[length(summary_rows) + 1L]] <<- tibble::tibble(
        row_id = id, outcome = outcome, summary = "n/N (%)",
        scope = "all available patients", row_label = label,
        treatment = g, group_n = group_n[[g]], n_observed = n,
        events = as.integer(events), percent = percent,
        median = NA_real_, q1 = NA_real_, q3 = NA_real_, display_value = value
      )
    }
  }

  # Primary outcome and pain scores: report the original observations, including
  # the original zero-minute PACU stay, rather than the gamma-model correction.
  add_median_rows("pacu", "pacu_minutes",
                  "Time to discharge decision (min), median [Q1, Q3]")
  add_median_rows("nrs_admission", "nrs_admission",
                  "NRS at PACU admission, median [Q1, Q3]", ordinal = TRUE)
  add_median_rows("nrs_discharge", "nrs_discharge",
                  "NRS at PACU discharge, median [Q1, Q3]", ordinal = TRUE)

  add_event_rows("nausea", "nausea", "Nausea, n/N (%)")
  add_event_rows("vomiting", "vomiting", "Vomiting, n/N (%)")
  add_event_rows("antiemetic", "antiemetic", "Antiemetic administration, n/N (%)")

  # Present all three analysed Nu-DESC categories, not just the >=2 category.
  add_event_rows("delirium_0", "delirium", "Nu-DESC 0, n/N (%)", category = "0")
  add_event_rows("delirium_1", "delirium", "Nu-DESC 1, n/N (%)", category = "1")
  add_event_rows("delirium_2", "delirium", "Nu-DESC \u22652, n/N (%)", category = ">=2")

  medications <- tibble::tribble(
    ~variable, ~label, ~unit,
    "piritramide_mg", "Piritramide", "mg",
    "dexketoprofen_mg", "Dexketoprofen", "mg",
    "ibuprofen_mg", "Ibuprofen", "mg",
    "paracetamol_mg", "Paracetamol", "mg",
    "metamizole_g", "Metamizole", "g"
  )

  # Pair administration frequency with the distribution of positive doses. This
  # mirrors the two-part analysis while keeping zero doses out of dose summaries.
  for (i in seq_len(nrow(medications))) {
    drug <- medications[i, ]
    add_event_rows(paste0(drug$variable, "_any"), drug$variable,
                   paste0(drug$label, " administration, n/N (%)"))
    add_median_rows(paste0(drug$variable, "_positive"), drug$variable,
                    paste0(drug$label, " dose (", drug$unit,
                           "), median [Q1, Q3]"), recipients_only = TRUE)
  }

  numeric_results <- dplyr::bind_rows(summary_rows)
  row_ids <- unique(numeric_results$row_id)

  # Check the category totals and two-part sample sizes before writing files.
  stopifnot(nrow(numeric_results) == 38L, length(row_ids) == 19L)
  for (g in groups) {
    categories <- numeric_results %>%
      filter(treatment == g, outcome == "delirium")
    stopifnot(sum(categories$events) == unique(categories$n_observed))
    for (v in medications$variable) {
      administration <- numeric_results %>%
        filter(treatment == g, row_id == paste0(v, "_any"))
      positive_dose <- numeric_results %>%
        filter(treatment == g, row_id == paste0(v, "_positive"))
      stopifnot(administration$events == positive_dose$n_observed)
    }
  }

  # The wide worksheet is the complete manuscript table. The numeric worksheet
  # preserves counts, denominators and unrounded quantiles for reproducibility.
  manuscript_table <- tibble::tibble(
    Outcome = numeric_results$row_label[match(row_ids, numeric_results$row_id)]
  )
  for (g in groups) {
    values <- numeric_results %>% filter(treatment == g)
    heading <- paste0(if (g == "Control") "Control" else "Ice-lolly",
                      " (N = ", format_count(group_n[[g]]), ")")
    manuscript_table[[heading]] <- values$display_value[match(row_ids, values$row_id)]
  }

  list(numeric = numeric_results, manuscript = manuscript_table, group_n = group_n)
}

# Reuse exactly the PP eligibility flag used by the main treatment models.
# Controls must neither have been offered nor consumed an ice lolly; patients
# allocated to ice-lolly days must have consumed one. Missing eligibility is
# excluded by filter(), consistently with 03_main_models.R.
per_protocol_data <- analysis_data %>% filter(pp_eligible)
itt <- summarise_population(analysis_data)
pp <- summarise_population(per_protocol_data)

write_csv_safe(itt$numeric, file.path(paths$tables, "table_08_outcome_summary.csv"))
write_csv_safe(pp$numeric, file.path(paths$tables, "table_09_outcome_summary_per_protocol.csv"))
writexl::write_xlsx(
  list(ITT = itt$manuscript, PP = pp$manuscript,
       numeric_summaries = itt$numeric, numeric_summaries_PP = pp$numeric),
  file.path(paths$tables, "outcome_summary_table.xlsx")
)

# Record this export separately so it can be rerun without the model pipeline.
writeLines(
  c(paste("Outcome-summary table generated:", format(Sys.time())),
    paste("ITT patients:", nrow(analysis_data)),
    paste("PP patients:", nrow(per_protocol_data)),
    "Rows in each manuscript table: 19",
    "Medication doses: positive doses among recipients only.",
    "NRS quantiles: empirical categories (R quantile type 1).",
    "Duration/dose quantiles: R quantile type 7.", "", capture.output(sessionInfo())),
  file.path(paths$logs, "outcome_summary_run.log")
)

message("Outcome-summary tables written: 19 rows each; ITT Control n = ",
        itt$group_n[[1]], ", Ice-lolly n = ", itt$group_n[[2]],
        "; PP Control n = ", pp$group_n[[1]],
        ", Ice-lolly n = ", pp$group_n[[2]], ". No models were re-fitted.")
