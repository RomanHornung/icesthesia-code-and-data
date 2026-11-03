# Icesthesia code and data supplement

This folder contains the R code, pseudo-data, and aggregate real-data results
for the statistical analysis of the Icesthesia study.

Individual-level real study data are not included in this public supplement.
Instead, the folder contains a generated pseudo-dataset that allows readers to
run the complete analysis workflow without access to confidential patient-level
data. The aggregate results from the real study are included separately so that
readers can check the numbers reported in the manuscript and supplement.

The original statistical analysis plan is saved as
`docs/statistical_analysis_plan.pdf`. The code includes the ordinal pain model
and medication-dose fallbacks described in the revised report. Subgroup analyses
were removed following peer review and are not part of this supplement.

## How to run the pseudo-data analysis

Open R in this folder (`code_and_data`) and run:

```r
source("R/99_run_all.R")
```

Alternatively, from a terminal in the same folder:

```bash
Rscript R/99_run_all.R
```

The master script checks the required packages, regenerates
`data/EIS_R_pseudodata.xlsx`, and then recreates all pseudo-data outputs in
`results_pseudodata/`. Existing files in that results folder are removed at the
start of each run to avoid mixing outputs from different runs. The aggregate
real-data results in `results_real/` are not changed or overwritten.

The pseudo-data results are intended to verify that the code runs and that the
analysis workflow is reproducible. They are not intended for clinical
interpretation and will not exactly match the real study results. Sparse
pseudo-data categories can also lead to different fallback models from those
used with the real data.

## Required R packages

The script `R/00_setup.R` checks that the following packages are installed:
`readxl`, `dplyr`, `tidyr`, `ggplot2`, `lme4`, `glmmTMB`, `ordinal`,
`broom.mixed`, `emmeans`, `performance`, `readr`, and `writexl`.

Install any missing packages with `install.packages()` before running the
analysis. The exact R and package versions used for the completed pseudo-data
run are saved in `results_pseudodata/logs/session_info.txt`.

## Folder and file contents

`README.md` is this guide; `README.html` is a browser-readable version of it.
The `.gitignore` file excludes local R session files and helps prevent accidental
addition of confidential real-data inputs or model objects.

### `data/`

- `EIS_R_pseudodata.xlsx`: generated pseudo-dataset with the same general
  structure as the real study dataset. It is created by
  `R/00_generate_pseudodata.R` and contains no real patient-level records.
- `variable_coding_ice_r.xlsx`: variable coding workbook supplied with the
  study materials.

The pseudo-data generator uses rounded marginal probabilities and plausible
relationships among clinical variables, rather than copying individual study
records. Its seed is fixed in the script, so the input workbook can be
regenerated without access to the confidential data.

### `docs/`

- `statistical_analysis_plan.pdf`: original statistical analysis plan, retained
  as a record of the planned analyses. Changes made for the revised report are
  described below under Main analysis choices and in the manuscript.

### `R/`

- `00_setup.R`: defines relative paths, checks required packages, creates output
  folders, loads packages, and sources helper functions.
- `00_generate_pseudodata.R`: creates the public pseudo-dataset with the raw
  variables expected by the preparation script.
- `functions.R`: helper functions for fitting mixed models, extracting treatment
  effects, applying medication-dose fallbacks, exporting tables, and making
  residual Q-Q plots.
- `01_prepare_data.R`: imports the pseudo-data Excel file, derives the analysis
  variables, performs consistency checks, and saves the analysis dataset.
- `02_descriptives_and_figures.R`: creates the retained-patient flow summary and
  complete baseline summaries for Table 1. Despite its original filename, this
  script no longer creates additional distribution plots.
- `03_main_models.R`: fits the ITT and per-protocol mixed-effects models and
  estimates intracluster correlations for the primary endpoint. The PP linear
  model is fitted for its reported Q-Q plot and ICC; an additional treatment
  contrast from that model is not exported.
- `05_model_checks.R`: creates the four reported residual Q-Q plots and prints
  compact model-check notes to the console.
- `08_outcome_summary_table.R`: creates the complete descriptive outcome tables
  for the ITT and PP populations, including readable Excel versions.
- `09_antiemetic_absolute_effects.R`: calculates the reported ITT absolute
  antiemetic risk difference and a confidence interval using 50,000 bootstrap
  resamples of whole study days within treatment arms. PP event counts and the
  PP odds ratio remain in the outcome and main-model tables; an additional PP
  absolute risk difference is not calculated.
- `06_export_excel_tables.R`: combines the numerical CSV result tables in an
  Excel workbook and adds a formatted baseline table on the `Table1` sheet.
- `99_run_all.R`: master script that runs the full pseudo-data workflow in the
  correct order, with the combined Excel export last.

The script numbers are retained from the earlier workflow; use `99_run_all.R`
to run them in the correct order. If only descriptive tables need to be
recreated, scripts 02 and 08 can be rerun on the existing derived data without
fitting the models again. Script 09 also runs independently of model fitting.
Run script 06 afterwards to update the combined Excel workbook.

### `results_real/`

This folder contains the aggregate results obtained from the real study data.
It is included so that the numerical results reported in the manuscript and
supplement can be checked. It does not contain the real individual-level
analysis dataset or fitted real-data model objects, and it cannot be regenerated
from the public pseudo-data.

#### `results_real/tables/`

- `table_01_participant_flow.csv`: retained patients, study days, allocation,
  ice-lolly consumption and PP population size.
- `table_02_baseline_characteristics.csv`: all Table 1 categories and continuous
  summaries, for the overall sample and each allocation; numerical counts,
  percentages, quartiles and display strings are provided.
- `table_04_main_results_itt.csv`: adjusted ITT treatment effects, confidence
  intervals, nominal p-values and model components.
- `table_05_main_results_per_protocol.csv`: corresponding reported PP effects;
  unreliable results are identified by `estimable = FALSE`.
- `table_06_primary_icc.csv`: ICC estimates and sample sizes for the four PACU
  model/population combinations in Supplementary Table 4.
- `table_08_outcome_summary.csv`: numerical sources and display strings for
  the complete ITT outcome table (manuscript Table 2).
- `table_09_outcome_summary_per_protocol.csv`: corresponding PP outcome table
  (Supplementary Table 6).
- `table_10_antiemetic_absolute_effects.csv`: ITT event counts, denominators,
  proportions, risk difference and 95% cluster-bootstrap confidence interval.
- `results_tables.xlsx`: the eight numerical result tables on named sheets,
  plus the formatted baseline table on `Table1`.
- `outcome_summary_table.xlsx`: complete display tables on `ITT` and `PP`, and
  their numerical sources on `numeric_summaries` and `numeric_summaries_PP`.

NRS summaries use empirical category quantiles (R type 1); duration and positive
medication doses use numeric quantiles (R type 7). Medication doses are
summarised among recipients only. Percentages are displayed to one decimal place.

CSV files use commas as separators and decimal points. The Excel workbooks
contain the same values and can be opened directly without choosing CSV import
settings. Use the `Table1`, `ITT`, and `PP` sheets for the display-ready tables;
the numerical sheets and CSV files provide the underlying values.

#### `results_real/figures/`

These are the real-data residual Q-Q plots corresponding to Supplementary
Figures 1-4:

- `itt_primary_gamma_residual_qq.png`: ITT PACU gamma-log model
  (Supplementary Figure 1).
- `itt_primary_lmm_sensitivity_residual_qq.png`: ITT PACU linear sensitivity
  model (Supplementary Figure 2).
- `pp_primary_gamma_residual_qq.png`: PP PACU gamma-log model
  (Supplementary Figure 3).
- `pp_primary_lmm_sensitivity_residual_qq.png`: PP PACU linear model
  (Supplementary Figure 4).

Here, `primary` refers to the primary endpoint, PACU time to discharge decision;
`sensitivity` identifies the alternative linear model rather than the main
gamma-log model. ITT and PP identify the analysis population.

### `results_pseudodata/`

This folder is recreated by `R/99_run_all.R`. It contains the corresponding
outputs from running the workflow on the pseudo-dataset, together with the
intermediate files needed to inspect the analysis.

#### `results_pseudodata/derived_data/`

- `analysis_data.rds`: derived pseudo-analysis dataset used by the analysis
  scripts. This is an intermediate reproducibility file, not a manuscript table.

#### `results_pseudodata/tables/`

- `table_01_participant_flow.csv`: participant-flow summary from the pseudo-data.
- `table_02_baseline_characteristics.csv`: complete baseline characteristics
  from the pseudo-data, overall and by allocation.
- `table_04_main_results_itt.csv`: pseudo-data ITT model results.
- `table_05_main_results_per_protocol.csv`: pseudo-data PP model results.
- `table_06_primary_icc.csv`: pseudo-data intracluster correlation estimates for
  the primary endpoint models.
- `table_08_outcome_summary.csv`: numerical sources and display strings for the
  complete pseudo-data ITT outcome table.
- `table_09_outcome_summary_per_protocol.csv`: corresponding PP outcome table.
- `table_10_antiemetic_absolute_effects.csv`: pseudo-data ITT antiemetic event
  counts, proportions, absolute risk difference, and bootstrap interval.
- `results_tables.xlsx`: the eight pseudo-data numerical tables on separate
  sheets, plus the formatted baseline table on `Table1`.
- `outcome_summary_table.xlsx`: pseudo-data display tables on `ITT` and `PP`,
  with their numerical sources on `numeric_summaries` and `numeric_summaries_PP`.

#### `results_pseudodata/figures/`

These are the same four diagnostic plot types as in `results_real/figures/`,
but based on the pseudo-data:

- `itt_primary_gamma_residual_qq.png`: ITT PACU gamma-log model.
- `itt_primary_lmm_sensitivity_residual_qq.png`: ITT PACU linear model.
- `pp_primary_gamma_residual_qq.png`: PP PACU gamma-log model.
- `pp_primary_lmm_sensitivity_residual_qq.png`: PP PACU linear model.

Only these four diagnostic plots are exported. Additional distribution plots
and residual-vs-fitted plots from earlier working versions are not produced.

#### `results_pseudodata/models/`

- `models_itt.rds`: fitted pseudo-data model objects for the ITT analyses,
  including both components of the medication models.
- `models_per_protocol.rds`: fitted pseudo-data model objects for the PP analyses.

These files are intermediate reproducibility files, not manuscript tables.
They make it possible to inspect fitted pseudo-data models without re-fitting
the full analysis. Real-data model objects are not included in the public folder.

#### `results_pseudodata/logs/`

- `analysis_run.log`: records which scripts were run and when they completed.
- `outcome_summary_run.log`: records the outcome-table export settings and R
  session used to create those tables.
- `antiemetic_absolute_run.log`: records the ITT bootstrap settings, seed, and
  R session used for the absolute antiemetic comparison.
- `session_info.txt`: R and package versions for the completed analysis run.

## Mapping to manuscript and supplement

The real-data aggregate outputs in `results_real/` reproduce the numerical
results reported in the manuscript and supplement. The pseudo-data outputs in
`results_pseudodata/` are for code reproducibility only.

Some polished manuscript and supplement figures were prepared outside R from
the statistical outputs using point-and-click graphics software. Their layout
is not generated by the R scripts.

- Manuscript Table 1 is reproduced in
  `results_real/tables/table_02_baseline_characteristics.csv`, with a readable
  version on the `Table1` sheet of `results_real/tables/results_tables.xlsx`.
- Manuscript Table 2 is on the `ITT` sheet of
  `results_real/tables/outcome_summary_table.xlsx`; its numerical source is
  `results_real/tables/table_08_outcome_summary.csv`.
- Manuscript Figure 1 (participant flow) was assembled and edited outside R.
  `results_real/tables/table_01_participant_flow.csv` reproduces the retained
  analysis counts. The 3,231 initially recorded patients and 11 exclusions come
  from separate clinical aggregate information; the excluded individual records
  are not in the analysis input.
- Manuscript Figure 2 (forest plot, ITT population) was prepared outside R using
  the estimates in `results_real/tables/table_04_main_results_itt.csv`.
- Supplementary Tables 1-3 are clinical discharge-readiness checklists and the
  Nu-DESC instrument. They are not generated by the statistical R scripts.
- Supplementary Table 4 corresponds to
  `results_real/tables/table_06_primary_icc.csv`.
- Supplementary Table 5 corresponds to the non-estimable PP delirium row in
  `results_real/tables/table_05_main_results_per_protocol.csv`.
- Supplementary Table 6 is on the `PP` sheet of
  `results_real/tables/outcome_summary_table.xlsx`; its numerical source is
  `results_real/tables/table_09_outcome_summary_per_protocol.csv`.
- Supplementary Figures 1-4 are generated by `R/05_model_checks.R`; the
  corresponding real-data versions are stored in `results_real/figures/`.
- Supplementary Figure 5 (forest plot of pain-medication models) was prepared
  outside R using the medication rows in
  `results_real/tables/table_04_main_results_itt.csv` and
  `results_real/tables/table_05_main_results_per_protocol.csv`.
- Supplementary Figure 6 (forest plot, PP population) was prepared outside R
  using `results_real/tables/table_05_main_results_per_protocol.csv`.

## Main analysis choices

- The intention-to-treat (ITT) analysis uses randomised study-day allocation,
  irrespective of actual ice-lolly consumption. In this code, ITT denotes the
  retained analysis dataset, not every patient initially recorded before the
  exclusions described in the manuscript.
- The per-protocol (PP) population contains patients on intervention days who
  consumed an ice lolly and patients on control days who neither received nor
  consumed one. All retained control patients meet this rule in the real data.
  PP comparisons involve post-randomisation selection and are not randomised
  comparisons of actual consumption.
- Study day is included as a random intercept in the mixed-effects models.
  Full models adjust for age group, sex, ASA, surgery type, anaesthesia duration,
  and anaesthesia type. Models for discharge pain, nausea, vomiting, antiemetic
  use, and medication use/dose additionally adjust for PACU duration.
- The primary endpoint, PACU time to discharge decision, is analysed with a
  gamma-log mixed model. An ITT linear mixed-model sensitivity analysis is also
  reported. The PP linear model is retained for its Q-Q plot and ICC.
- Pain at discharge is analysed as an ordered outcome using a cumulative-logit
  mixed model. This avoids imposing a continuous distribution on a bounded
  integer score with many zeros. Pain at admission is summarised descriptively,
  not tested as a treatment outcome. Nu-DESC severity is also analysed using
  a cumulative-logit mixed model, with categories 0, 1, and >=2. Its effect is
  a proportional odds ratio across these categories, not a binary >=2 contrast.
- Medication outcomes use a two-part approach. First, medication administration
  is analysed as yes/no using a logistic mixed model. Second, among patients who
  received the medication, positive dose intensity is analysed using an ordinal
  cumulative-logit mixed model where possible. If that model is not estimable,
  a clinically collapsed binary positive-dose model is used. This keeps the
  two-part approach from the SAP but avoids treating highly discrete medication
  doses as gamma-distributed continuous outcomes. Where full adjustment remains
  unstable, a reduced binary fallback retains treatment, PACU duration, and the
  study-day random intercept. The `component` column identifies the model used.
- Secondary analyses are exploratory and are not adjusted for multiplicity.
  The recorded antiemetic endpoint combines intraoperative and PACU
  administration; it does not identify rescue treatment separately. The ITT
  absolute risk difference is calculated from pooled patient counts within each
  arm. Its percentile interval uses 50,000 bootstrap resamples of whole days
  within arms (seed 20261023), preserving day-level clustering.

For the positive-dose models, the ordered categories are piritramide >0-3.75,
>3.75-7.5, >7.5-15, and >15 mg; dexketoprofen <=50 or >50 mg; ibuprofen <400 or
>=400 mg; paracetamol <1000, 1000, and >1000 mg; and metamizole <1, 1, and >1 g.
Zero doses are excluded from all of these positive-dose categories. The binary
fallback thresholds are >7.5 mg, >50 mg, >=400 mg, >=1000 mg, and >=1 g,
respectively. Rows marked `estimable = FALSE` should not be interpreted as
reliable treatment estimates.

Table 1 keeps ASA I-IV separate, whereas model adjustment combines III/IV.
Spinal and peripheral nerve-block anaesthesia are combined for model adjustment
and for their descriptive category; regional add-on blocks are recorded in a
separate field. The available age bands give <=12, 13-75, and >=76 years; exact
age 75 cannot be separated from the 66-75 band. Baseline weight bands are <=40,
41-90, and >90 kg; height bands are <150, 150-169, and >=170 cm. These labels
describe the grouped input data rather than implying unavailable exact cutoffs.
