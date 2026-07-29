# Icesthesia code and data supplement

This folder contains the R code, pseudo-data, and aggregate real-data results
for the statistical analysis of the Icesthesia study.

The analysis follows the statistical analysis plan saved as
`docs/statistical_analysis_plan.pdf`. Individual-level real study data are not
included in this public supplement. Instead, the folder contains a generated
pseudo-dataset that allows readers to run the complete analysis workflow without
access to confidential patient-level data.

## How to run the pseudo-data analysis

Open R or a terminal in this folder (`code_and_data`) and run:

```r
source("R/99_run_all.R")
```

Alternatively, from a command line:

```bash
Rscript R/99_run_all.R
```

The master script first regenerates `data/EIS_R_pseudodata.xlsx` and then
recreates all pseudo-data outputs in `results_pseudodata/`. Existing files in
`results_pseudodata/` are removed at the start of each run to avoid mixing
outputs from different runs.

The pseudo-data results are intended to verify that the code runs and that the
analysis workflow is reproducible. They are not intended for clinical
interpretation and will not exactly match the real study results.

## Required R packages

The script `R/00_setup.R` checks that the following packages are installed:
`readxl`, `dplyr`, `tidyr`, `ggplot2`, `lme4`, `glmmTMB`, `ordinal`,
`broom.mixed`, `emmeans`, `performance`, `readr`, and `writexl`.

The exact package versions used for a pseudo-data run are saved in
`results_pseudodata/logs/session_info.txt`.

## Folder and file contents

### `data/`

- `EIS_R_pseudodata.xlsx`: generated pseudo-dataset with the same general
  structure as the real study dataset. It is created by
  `R/00_generate_pseudodata.R` and contains no real patient-level records.
- `variable_coding_ice_r.xlsx`: variable coding workbook supplied with the
  study materials.

### `docs/`

- `statistical_analysis_plan.pdf`: statistical analysis plan used as the
  authoritative analysis document.

### `R/`

- `00_setup.R`: defines relative paths, checks required packages, creates output
  folders, loads packages, and sources helper functions.
- `00_generate_pseudodata.R`: creates the public pseudo-dataset with the raw
  variables expected by the preparation script.
- `functions.R`: helper functions for data derivation, descriptive summaries,
  mixed models, extraction of treatment effects, and diagnostic plots.
- `01_prepare_data.R`: imports the pseudo-data Excel file, derives the analysis
  variables, performs basic consistency checks, and saves the analysis dataset.
- `02_descriptives_and_figures.R`: creates descriptive tables and distribution
  plots.
- `03_main_models.R`: fits the main ITT and per-protocol mixed-effects models.
- `04_subgroup_analyses_two_part.R`: fits exploratory treatment-by-subgroup
  interaction models and reports stratum-specific treatment effects.
- `05_model_checks.R`: creates residual diagnostic plots and prints compact
  model-check notes.
- `06_export_excel_tables.R`: writes the public pseudo-data CSV tables into one
  Excel workbook for convenient viewing.
- `07_export_repository_subgroup_table.R`: creates a rounded, reader-friendly
  Excel version of the full exploratory subgroup interaction table.
- `99_run_all.R`: master script that runs the full pseudo-data workflow in the
  correct order.

### `results_real/`

This folder contains the aggregate results obtained from the real study data.
It is included so that the numerical results reported in the manuscript and
supplement can be checked. It does not contain the real individual-level
analysis dataset or fitted real-data model objects.

#### `results_real/tables/`

- `table_01_participant_flow.csv`: number of patients, study days, treatment
  groups, ice-lolly consumption, and per-protocol population size.
- `table_02_baseline_characteristics.csv`: baseline characteristics by treatment
  group. Categorical variables are shown as counts and percentages; anaesthesia
  duration is shown with continuous summaries.
- `table_03_outcome_descriptives.csv`: unadjusted descriptive summaries of the
  primary and secondary outcomes by treatment group.
- `table_04_main_results_itt.csv`: adjusted ITT treatment effects from the main
  mixed-effects models.
- `table_05_main_results_per_protocol.csv`: adjusted per-protocol treatment
  effects from the same model framework.
- `table_06_primary_icc.csv`: intraclass correlation coefficients for the
  primary endpoint models.
- `table_07_exploratory_subgroup_interactions.csv`: exploratory
  treatment-by-subgroup analyses, including the global interaction p-value and
  stratum-specific treatment effects with confidence intervals where estimable.
- `results_tables.xlsx`: Excel workbook containing the seven central real-data
  result tables as separate worksheets.
- `full_subgroup_interaction_table.xlsx`: rounded, reader-friendly Excel
  workbook of the full exploratory subgroup interaction table, with a codebook
  sheet. This file is intended for sharing as supplementary raw data or in an
  online repository.

#### `results_real/figures/`

These are the real-data diagnostic Q-Q plots corresponding to Supplementary
Figures S1-S4:

- `itt_primary_gamma_residual_qq.png`
- `itt_primary_lmm_sensitivity_residual_qq.png`
- `pp_primary_gamma_residual_qq.png`
- `pp_primary_lmm_sensitivity_residual_qq.png`

### `results_pseudodata/`

This folder is created by `R/99_run_all.R`. It contains outputs from running the
full workflow on the pseudo-dataset.

#### `results_pseudodata/derived_data/`

- `analysis_data.rds`: derived pseudo-analysis dataset used by all analysis
  scripts. This is an intermediate reproducibility file, not a manuscript table.

#### `results_pseudodata/tables/`

- `table_01_participant_flow.csv`: participant-flow summary from the pseudo-data.
- `table_02_baseline_characteristics.csv`: baseline characteristics from the
  pseudo-data.
- `table_03_outcome_descriptives.csv`: outcome descriptives from the pseudo-data.
- `table_04_main_results_itt.csv`: pseudo-data ITT model results.
- `table_05_main_results_per_protocol.csv`: pseudo-data per-protocol model
  results.
- `table_06_primary_icc.csv`: pseudo-data intraclass correlation estimates for
  the primary endpoint models.
- `table_07_exploratory_subgroup_interactions.csv`: pseudo-data exploratory
  subgroup interaction results.
- `results_tables.xlsx`: Excel workbook containing the seven pseudo-data result
  tables as separate worksheets.
- `full_subgroup_interaction_table.xlsx`: rounded pseudo-data version of the
  full exploratory subgroup interaction table.

#### `results_pseudodata/figures/`

These files are pseudo-data diagnostic figures. They are saved to make the
visual distribution and residual checks reproducible.

- `pacu_minutes_distribution.png`: histogram by treatment group for the primary
  endpoint.
- `pacu_minutes_qq.png`: normal Q-Q plot by treatment group for PACU minutes.
- `<outcome>_ordinal_distribution.png`: category-frequency plot by treatment
  group for ordinal outcomes such as NRS scores and ordered medication-dose
  categories.
- `itt_<model>_residuals_vs_fitted.png`: residual-vs-fitted plot for an ITT
  continuous PACU model.
- `itt_<model>_residual_qq.png`: residual Q-Q plot for the same ITT model.
- `pp_<model>_residuals_vs_fitted.png`: residual-vs-fitted plot for a
  per-protocol continuous PACU model.
- `pp_<model>_residual_qq.png`: residual Q-Q plot for the same per-protocol
  model.

#### `results_pseudodata/models/`

- `models_itt.rds`: fitted pseudo-data model objects for the ITT analyses.
- `models_per_protocol.rds`: fitted pseudo-data model objects for the
  per-protocol analyses.
- `models_subgroup_interactions.rds`: fitted pseudo-data model objects for the
  exploratory subgroup interaction analyses.

These files are intermediate reproducibility files. They are not manuscript
tables, but they make it possible to inspect fitted pseudo-data models without
re-fitting the full analysis.

#### `results_pseudodata/logs/`

- `analysis_run.log`: records which scripts were run and when they completed.
- `session_info.txt`: output of R's `sessionInfo()` after loading the required
  packages.

## Mapping to manuscript and supplement

The real-data aggregate outputs in `results_real/` reproduce the central
numerical results reported in the manuscript and supplement. The pseudo-data
outputs in `results_pseudodata/` are for code reproducibility only.

Some polished manuscript/supplement figures were prepared outside R from the
statistical outputs using point-and-click graphics software.

- Manuscript Figure 1 (participant flow) is not generated by the R scripts. It
  was prepared outside R using the participant-flow numbers reproduced in
  `results_real/tables/table_01_participant_flow.csv`.
- Manuscript Figure 2 (forest plot of primary and key secondary outcomes) is
  not generated by the R scripts. It was prepared outside R using the adjusted
  treatment-effect estimates reproduced in
  `results_real/tables/table_04_main_results_itt.csv` and
  `results_real/tables/table_05_main_results_per_protocol.csv`.
- Supplementary Figures S1-S4 are generated by `R/05_model_checks.R`; the
  corresponding real-data versions are stored in `results_real/figures/`.
- Supplementary Figure S5 (forest plot of pain-medication models) is not
  generated by the R scripts. It was prepared outside R using the medication
  model estimates reproduced in
  `results_real/tables/table_04_main_results_itt.csv` and
  `results_real/tables/table_05_main_results_per_protocol.csv`.
- Supplementary Figure S6 (subgroup distribution plots) is not generated by the
  R scripts. It was prepared outside R from subgroup/descriptive outputs,
  including
  `results_real/tables/table_07_exploratory_subgroup_interactions.csv`.
- Supplementary Tables S1-S2 are clinical discharge-readiness checklist tables
  and are not generated by the statistical R scripts.
- Supplementary Table S3 corresponds to
  `results_real/tables/table_06_primary_icc.csv`.
- Supplementary Table S4 corresponds to the non-estimable-model row in
  `results_real/tables/table_05_main_results_per_protocol.csv`.
- Supplementary Table S5 corresponds to the nominally significant interaction
  tests in
  `results_real/tables/table_07_exploratory_subgroup_interactions.csv`.
- The full exploratory subgroup interaction table for repository sharing is
  `results_real/tables/full_subgroup_interaction_table.xlsx`; the unrounded
  source table remains
  `results_real/tables/table_07_exploratory_subgroup_interactions.csv`.

The code also writes additional pseudo-data diagnostic/descriptive files that
are not displayed in the manuscript or supplement, such as distribution plots
and residual-vs-fitted plots. These files are retained as diagnostics rather 
than manuscript figures.

## Main analysis choices

- The intention-to-treat population contains all records and uses the randomized
  day allocation.
- The per-protocol population contains patients on intervention days who
  consumed an ice lolly and patients on control days who neither received nor
  consumed one.
- Study day is included as a random intercept in the mixed-effects models.
- The primary endpoint, PACU length of stay, is right-skewed and is therefore
  analysed with a gamma-log mixed model. A linear mixed model sensitivity
  analysis is also provided.
- Pain scores are analysed as ordered outcomes using cumulative logit mixed
  models. This avoids imposing a continuous distribution on bounded integer
  pain scores with many zeros.
- Medication outcomes use a two-part approach. First, medication administration
  is analysed as yes/no using a logistic mixed model. Second, among patients
  who received the medication, positive dose intensity is analysed using an
  ordinal cumulative logit mixed model where possible. If the ordinal
  positive-dose model is not estimable, a clinically collapsed binary
  positive-dose model is used. This keeps the two-part logic from the SAP but
  avoids treating highly discrete medication doses as gamma-distributed
  continuous outcomes.
- Secondary and subgroup analyses are exploratory and are not adjusted for
  multiplicity.
