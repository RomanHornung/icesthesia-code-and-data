# Import and prepare the analysis dataset.
#
# This script:
# - imports the raw Excel file,
# - checks the expected column structure,
# - derives the variables used in the SAP,
# - performs basic data-integrity checks,
# - saves one analysis-ready R dataset for later scripts.

source(file.path("R", "00_setup.R"))

raw_data <- readxl::read_excel(paths$raw_data, sheet = 1)

# The raw workbook is expected to have this exact structure. If a column name
# changes, the script stops here instead of silently analysing the wrong data.
expected_columns <- c(
  "ID", "Date", "ICEDAY", "Sex", "Age", "Weight", "Height", "ASA",
  "Surg_Type", "AN_Mode", "PDA", "RA", "DurAn_hh:mm:ss",
  "DurSurg_hh:mm:ss", "PACU_Start_time", "PACU_Discharge_time",
  "Dur_AWR_hh:mm:ss", "NRS_Start", "NRS_DC", "Nudesc_DC", "Piri_mg",
  "PIRI_kg", "Dexketo_mg", "Ibu_mg", "Parac_mg", "Metamizol_g",
  "AE_y/n", "NAUS_y/n", "VOM_y/n", "Capri_offered", "Capri_consumed"
)

stopifnot(identical(names(raw_data), expected_columns))
stopifnot(!anyDuplicated(raw_data$ID))

# Age is available only in categories. The SAP subgroup cutoffs are therefore
# approximated by the available categories.
age_group <- dplyr::case_when(
  raw_data$Age %in% c("2-3", "4-5", "6-8", "10-12") ~ "<=12",
  raw_data$Age %in% c("13-17", "18-25", "26-35", "36-45", "46-55", "56-65", "66-75") ~ "13-75",
  raw_data$Age %in% c("76-85", "86-95") ~ ">=76",
  TRUE ~ NA_character_
)

analysis_data <- raw_data %>%
  transmute(
    # Randomisation and patient identifiers.
    id = ID,
    study_day = factor(as.Date(Date, origin = "1899-12-30")),
    treatment = factor(ICEDAY, levels = c(0, 1), labels = c("Control", "Ice lolly")),

    # Baseline covariates used for adjustment.
    sex = factor(Sex, levels = c(0, 1), labels = c("Male", "Female")),
    age_original = factor(Age),
    age_group = factor(age_group, levels = c("13-75", "<=12", ">=76")),
    weight_group = factor(Weight),
    height_group = factor(Height),
    asa = factor(if_else(ASA %in% c("III", "IV"), "III-IV", ASA), levels = c("I", "II", "III-IV")),
    surgery_type = factor(
      Surg_Type,
      levels = 1:4,
      labels = c("Orthopedic / trauma", "Neurosurgery", "ENT", "Other / plastic")
    ),
    anesthesia_type = factor(
      if_else(AN_Mode %in% c(5, 6), 5, AN_Mode),
      levels = 1:5,
      labels = c("TIVA", "CIVA", "Balanced", "IVA", "Regional")
    ),
    epidural = as.integer(PDA),
    regional_add_on = as.integer(RA),

    # Durations are imported as clock-time objects and converted to minutes.
    anesthesia_minutes = clock_minutes(`DurAn_hh:mm:ss`),
    anesthesia_hours = anesthesia_minutes / 60,
    surgery_minutes = clock_minutes(`DurSurg_hh:mm:ss`),
    pacu_start_minutes = clock_minutes(PACU_Start_time),
    pacu_discharge_minutes = clock_minutes(PACU_Discharge_time),
    pacu_minutes = pacu_discharge_minutes - pacu_start_minutes,
    supplied_pacu_minutes = clock_minutes(`Dur_AWR_hh:mm:ss`),

    # Prespecified clinical outcomes.
    nrs_admission = as.numeric(NRS_Start),
    nrs_discharge = as.numeric(NRS_DC),
    delirium = ordered(
      case_when(
        Nudesc_DC == 0 ~ "0",
        Nudesc_DC == 1 ~ "1",
        Nudesc_DC >= 2 ~ ">=2",
        TRUE ~ NA_character_
      ),
      levels = c("0", "1", ">=2")
    ),
    piritramide_mg = tidyr::replace_na(as.numeric(Piri_mg), 0),
    dexketoprofen_mg = tidyr::replace_na(as.numeric(Dexketo_mg), 0),
    ibuprofen_mg = tidyr::replace_na(as.numeric(Ibu_mg), 0),
    paracetamol_mg = tidyr::replace_na(as.numeric(Parac_mg), 0),
    metamizole_g = tidyr::replace_na(as.numeric(Metamizol_g), 0),
    antiemetic = as.integer(`AE_y/n`),
    nausea = as.integer(`NAUS_y/n`),
    vomiting = as.integer(`VOM_y/n`),
    ice_offered = as.integer(Capri_offered),
    ice_consumed = as.integer(Capri_consumed)
  ) %>%
  mutate(
    # Correct rare overnight stays if discharge time passes midnight.
    pacu_minutes = if_else(pacu_minutes < 0, pacu_minutes + 24 * 60, pacu_minutes),
    pacu_hours = pacu_minutes / 60,

    # Gamma models require strictly positive values. The original PACU duration
    # is kept unchanged; this corrected variable is used only for gamma models.
    pacu_minutes_gamma = pmax(pacu_minutes, 0.5),

    # This variable checks that the derived duration matches the supplied field.
    pacu_duration_difference = pacu_minutes - supplied_pacu_minutes,

    # Per-protocol definition from the SAP.
    pp_eligible = (treatment == "Ice lolly" & ice_consumed == 1L) |
      (treatment == "Control" & ice_offered == 0L & ice_consumed == 0L)
  ) %>%
  mutate(
    # Ordered dose categories used for the medication-dose models. These avoid
    # treating strongly discrete dose variables as continuous gamma outcomes.
    piritramide_dose_ord = ordered(
      case_when(
        piritramide_mg == 0 ~ "0",
        piritramide_mg <= 3.75 ~ ">0-3.75",
        piritramide_mg <= 7.5 ~ ">3.75-7.5",
        piritramide_mg <= 15 ~ ">7.5-15",
        piritramide_mg > 15 ~ ">15",
        TRUE ~ NA_character_
      ),
      levels = c("0", ">0-3.75", ">3.75-7.5", ">7.5-15", ">15")
    ),
    dexketoprofen_dose_ord = ordered(
      case_when(
        dexketoprofen_mg == 0 ~ "0",
        dexketoprofen_mg <= 50 ~ ">0-50",
        dexketoprofen_mg > 50 ~ ">50",
        TRUE ~ NA_character_
      ),
      levels = c("0", ">0-50", ">50")
    ),
    ibuprofen_dose_ord = ordered(
      case_when(
        ibuprofen_mg == 0 ~ "0",
        ibuprofen_mg < 400 ~ ">0-<400",
        ibuprofen_mg >= 400 ~ ">=400",
        TRUE ~ NA_character_
      ),
      levels = c("0", ">0-<400", ">=400")
    ),
    paracetamol_dose_ord = ordered(
      case_when(
        paracetamol_mg == 0 ~ "0",
        paracetamol_mg < 1000 ~ ">0-<1000",
        paracetamol_mg == 1000 ~ "1000",
        paracetamol_mg > 1000 ~ ">1000",
        TRUE ~ NA_character_
      ),
      levels = c("0", ">0-<1000", "1000", ">1000")
    ),
    metamizole_dose_ord = ordered(
      case_when(
        metamizole_g == 0 ~ "0",
        metamizole_g < 1 ~ ">0-<1",
        metamizole_g == 1 ~ "1",
        metamizole_g > 1 ~ ">1",
        TRUE ~ NA_character_
      ),
      levels = c("0", ">0-<1", "1", ">1")
    ),
    piritramide_positive_ord = ordered(
      case_when(
        piritramide_mg > 0 & piritramide_mg <= 3.75 ~ ">0-3.75",
        piritramide_mg > 3.75 & piritramide_mg <= 7.5 ~ ">3.75-7.5",
        piritramide_mg > 7.5 & piritramide_mg <= 15 ~ ">7.5-15",
        piritramide_mg > 15 ~ ">15",
        TRUE ~ NA_character_
      ),
      levels = c(">0-3.75", ">3.75-7.5", ">7.5-15", ">15")
    ),
    dexketoprofen_positive_ord = ordered(
      case_when(
        dexketoprofen_mg > 0 & dexketoprofen_mg <= 50 ~ ">0-50",
        dexketoprofen_mg > 50 ~ ">50",
        TRUE ~ NA_character_
      ),
      levels = c(">0-50", ">50")
    ),
    ibuprofen_positive_ord = ordered(
      case_when(
        ibuprofen_mg > 0 & ibuprofen_mg < 400 ~ ">0-<400",
        ibuprofen_mg >= 400 ~ ">=400",
        TRUE ~ NA_character_
      ),
      levels = c(">0-<400", ">=400")
    ),
    paracetamol_positive_ord = ordered(
      case_when(
        paracetamol_mg > 0 & paracetamol_mg < 1000 ~ ">0-<1000",
        paracetamol_mg == 1000 ~ "1000",
        paracetamol_mg > 1000 ~ ">1000",
        TRUE ~ NA_character_
      ),
      levels = c(">0-<1000", "1000", ">1000")
    ),
    metamizole_positive_ord = ordered(
      case_when(
        metamizole_g > 0 & metamizole_g < 1 ~ ">0-<1",
        metamizole_g == 1 ~ "1",
        metamizole_g > 1 ~ ">1",
        TRUE ~ NA_character_
      ),
      levels = c(">0-<1", "1", ">1")
    ),
    piritramide_positive_binary = as.integer(piritramide_mg > 7.5),
    dexketoprofen_positive_binary = as.integer(dexketoprofen_mg > 50),
    ibuprofen_positive_binary = as.integer(ibuprofen_mg >= 400),
    paracetamol_positive_binary = as.integer(paracetamol_mg >= 1000),
    metamizole_positive_binary = as.integer(metamizole_g >= 1)
  )

# Check that all patients from a given study day have the same randomised
# treatment allocation.
allocation_check <- analysis_data %>%
  group_by(study_day) %>%
  summarise(n = n(), allocations = n_distinct(treatment), .groups = "drop")

stopifnot(all(allocation_check$allocations == 1))
stopifnot(all(analysis_data$pacu_minutes >= 0))
stopifnot(all(analysis_data$nrs_admission %in% 0:10 | is.na(analysis_data$nrs_admission)))
stopifnot(all(analysis_data$nrs_discharge %in% 0:10 | is.na(analysis_data$nrs_discharge)))

# Print compact QC information to the console/log. These are checks rather than
# manuscript tables, so they are not saved as public result CSV files.
message("QC: patients = ", nrow(analysis_data))
message("QC: randomised days = ", nlevels(analysis_data$study_day))
message("QC: per-protocol patients = ", sum(analysis_data$pp_eligible))
message("QC: duplicate IDs = ", sum(duplicated(analysis_data$id)))
message("QC: days with inconsistent allocation = ", sum(allocation_check$allocations != 1))
message(
  "QC: maximum absolute PACU duration difference = ",
  max(abs(analysis_data$pacu_duration_difference), na.rm = TRUE)
)

# Save the single derived dataset used by all following scripts.
saveRDS(analysis_data, file.path(paths$derived, "analysis_data.rds"))
