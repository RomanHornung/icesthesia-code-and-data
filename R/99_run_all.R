# Master script for the complete Icesthesia analysis.
#
# Execute this file from the code_and_data folder:
#   Rscript R/99_run_all.R
#
# All pseudo-data tables, figures, model objects, and logs are written to
# results_pseudodata/.
# Existing generated results are removed first so that obsolete files cannot
# remain from earlier runs.

# Validate the package and dependencies before removing any generated output.
source(file.path("R", "00_setup.R"))
results_root <- normalizePath(paths$results, winslash = "/", mustWork = TRUE)

if (dir.exists(results_root)) {
  result_subfolders <- file.path(
    results_root,
    c("derived_data", "figures", "models", "tables", "logs")
  )

  # Refuse cleanup if a resolved path (including a linked directory) leaves
  # this package's generated-results directory.
  resolved <- normalizePath(result_subfolders, winslash = "/", mustWork = TRUE)
  stopifnot(all(startsWith(resolved, paste0(results_root, "/"))))

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
  "R/05_model_checks.R",
  "R/08_outcome_summary_table.R",
  "R/09_antiemetic_absolute_effects.R",
  "R/06_export_excel_tables.R"
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
