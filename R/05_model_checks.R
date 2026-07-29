# Model checks and diagnostic plots.
#
# The public output is deliberately concise. This script saves diagnostic
# figures but does not create extra diagnostic CSV tables. Compact model-check
# summaries are printed to the console/log instead.

source(file.path("R", "00_setup.R"))

analysis_data <- readRDS(file.path(paths$derived, "analysis_data.rds"))

model_files <- list.files(paths$models, pattern = "\\.rds$", full.names = TRUE)

audit_rows <- list()

for (model_file in model_files) {
  model_objects <- flatten_models(readRDS(model_file))

  for (model_name in names(model_objects)) {
    model <- model_objects[[model_name]]

    if (is.null(model)) {
      next
    }

    audit_rows[[length(audit_rows) + 1]] <- model_audit_row(
      model_object = model,
      model_name = paste0(basename(model_file), "::", model_name)
    )
  }
}

model_audit <- bind_rows(audit_rows)

# Print only the models that deserve attention. This avoids filling the public
# results folder with audit tables while still making warnings visible.
flagged_models <- model_audit %>%
  filter(singular %in% TRUE | convergence_message != "")

if (nrow(flagged_models) > 0) {
  message("Model-check note: the following models had singularity/convergence flags:")
  print(flagged_models)
} else {
  message("Model-check note: no singularity/convergence flags detected.")
}

itt_models <- readRDS(file.path(paths$models, "models_itt.rds"))
pp_models <- readRDS(file.path(paths$models, "models_per_protocol.rds"))

# Residual plots are saved only for the continuous PACU models. The NRS and
# medication-dose outcomes are now ordinal models, for which the category
# distribution plots from 02_descriptives_and_figures.R are the relevant visual
# checks.
save_residual_plots(itt_models$primary_gamma, "itt_primary_gamma", paths$figures)
save_residual_plots(itt_models$primary_lmm, "itt_primary_lmm_sensitivity", paths$figures)

save_residual_plots(pp_models$primary_gamma, "pp_primary_gamma", paths$figures)
save_residual_plots(pp_models$primary_lmm, "pp_primary_lmm_sensitivity", paths$figures)
