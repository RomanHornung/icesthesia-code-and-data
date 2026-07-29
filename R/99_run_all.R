# Master script for the complete Icesthesia analysis.
#
# Execute this file from the code_and_data folder:
#   Rscript R/99_run_all.R
#
# All pseudo-data tables, figures, model objects, and logs are written to
# results_pseudodata/.
# Existing generated results are removed first so that obsolete files cannot
# remain from earlier runs.

if (dir.exists("results_pseudodata")) {
  result_subfolders <- file.path(
    "results_pseudodata",
    c("derived_data", "figures", "models", "tables", "logs")
  )

  # On some Windows systems, files copied from another location may be read-only.
  # Make generated result files writable before deleting them.
  existing_files <- unlist(
    lapply(result_subfolders, list.files, recursive = TRUE, full.names = TRUE, all.files = TRUE),
    use.names = FALSE
  )

  if (length(existing_files) > 0) {
    Sys.chmod(existing_files, mode = "0666", use_umask = FALSE)
  }

  unlink(result_subfolders, recursive = TRUE, force = TRUE)
}

scripts <- c(
  "R/00_generate_pseudodata.R",
  "R/01_prepare_data.R",
  "R/02_descriptives_and_figures.R",
  "R/03_main_models.R",
  "R/04_subgroup_analyses_two_part.R",
  "R/05_model_checks.R",
  "R/06_export_excel_tables.R",
  "R/07_export_repository_subgroup_table.R"
)

dir.create(file.path("results_pseudodata", "logs"), recursive = TRUE, showWarnings = FALSE)

log_file <- file.path("results_pseudodata", "logs", "analysis_run.log")

cat("Icesthesia analysis started: ", format(Sys.time()), "\n", file = log_file)

for (script in scripts) {
  cat("Running ", script, "\n", file = log_file, append = TRUE)
  source(script, echo = FALSE)
  cat("Completed ", script, " at ", format(Sys.time()), "\n", file = log_file, append = TRUE)
}

writeLines(capture.output(sessionInfo()), file.path("results_pseudodata", "logs", "session_info.txt"))

cat("Icesthesia analysis completed: ", format(Sys.time()), "\n", file = log_file, append = TRUE)
