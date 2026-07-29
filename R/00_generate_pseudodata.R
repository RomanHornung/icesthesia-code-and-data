# Generate the public pseudo-dataset.
#
# The real individual-level study data cannot be shared publicly. This script
# therefore creates a synthetic raw-data workbook with the same column names and
# broadly similar distributions. The pseudo-data are not intended to reproduce
# the published numerical results. They are provided so that readers can run the
# analysis code end-to-end and inspect the workflow, model fitting, tables, and
# figures.

source(file.path("R", "00_setup.R"))

set.seed(20260729)

n_patients <- 3220
n_days <- 108
n_ice_days <- 55
start_date <- as.Date("2025-07-28")

# Helper for categorical sampling with explicit probabilities.
sample_cat <- function(values, probabilities, n) {
  sample(values, size = n, replace = TRUE, prob = probabilities / sum(probabilities))
}

# Helper for Bernoulli variables.
rbern <- function(probability, n) {
  stats::rbinom(n, size = 1, prob = probability)
}

# Helper for Excel-compatible clock-time columns. The original workbook stores
# durations and times as time-like Excel cells; writing POSIXct values keeps the
# same import behaviour for readxl.
clock_time <- function(minutes) {
  as.POSIXct("1899-12-31", tz = "UTC") + round(minutes * 60)
}

# Create study days and cluster sizes. All patients on the same day receive the
# same randomized treatment allocation.
study_dates <- start_date + seq_len(n_days) - 1
day_treatment <- sample(c(rep(1L, n_ice_days), rep(0L, n_days - n_ice_days)))

day_sizes <- as.vector(rmultinom(1, size = n_patients - n_days * 20, prob = rep(1, n_days))) + 20
day_id <- rep(seq_len(n_days), times = day_sizes)

treatment <- day_treatment[day_id]

# Baseline and procedural variables are generated from empirical-like marginal
# distributions. These probabilities are rounded and are not patient-level data.
sex <- sample_cat(c(0L, 1L), c(0.544, 0.456), n_patients)

age <- sample_cat(
  c("2-3", "4-5", "6-8", "10-12", "13-17", "18-25", "26-35",
    "36-45", "46-55", "56-65", "66-75", "76-85", "86-95"),
  c(0.006, 0.007, 0.021, 0.018, 0.038, 0.080, 0.106,
    0.110, 0.143, 0.199, 0.138, 0.105, 0.029),
  n_patients
)

weight <- sample_cat(
  c("6-8", "11-15", "16-20", "21-25", "25-30", "31-40", "41-50",
    "51-60", "61-70", "71-80", "81-90", "91-100", "101-110",
    "111-120", "121-130", "131-140", "141-150", "151-160", "171-180"),
  c(0.001, 0.005, 0.008, 0.008, 0.009, 0.017, 0.044,
    0.127, 0.197, 0.195, 0.179, 0.108, 0.049,
    0.034, 0.011, 0.005, 0.001, 0.002, 0.001),
  n_patients
)

height <- sample_cat(
  c("84-80", "89-85", "99-90", "109-100", "119-110", "129-120",
    "139-130", "149-140", "159-150", "169-160", "179-170",
    "189-180", "199-190", "220-200"),
  c(0.002, 0.0003, 0.003, 0.004, 0.006, 0.010,
    0.009, 0.013, 0.083, 0.271, 0.324,
    0.229, 0.042, 0.004),
  n_patients
)

asa <- sample_cat(c("I", "II", "III", "IV"), c(0.194, 0.471, 0.323, 0.012), n_patients)
surgery_type <- sample_cat(1:4, c(0.417, 0.148, 0.402, 0.032), n_patients)
an_mode <- sample_cat(1:6, c(0.430, 0.402, 0.119, 0.033, 0.015, 0.002), n_patients)
pda <- rbern(0.012, n_patients)
ra <- rbern(0.100, n_patients)

# Generate procedure and PACU durations. The treatment effect is deliberately
# small so that pseudo-data behave like a neutral trial while remaining random.
anesthesia_minutes <- pmax(20, round(stats::rgamma(n_patients, shape = 5, scale = 22)))
surgery_minutes <- pmax(10, round(anesthesia_minutes * stats::runif(n_patients, 0.45, 0.85)))

age_tail <- ifelse(age %in% c("76-85", "86-95"), 12, 0)
complexity <- ifelse(asa %in% c("III", "IV"), 10, 0) + ifelse(surgery_type == 2, 10, 0)
pacu_minutes <- round(stats::rgamma(n_patients, shape = 4.5, scale = 20) + age_tail + complexity + treatment * 1.5)
pacu_minutes <- pmin(pmax(pacu_minutes, 5), 420)

pacu_start <- sample(seq(7 * 60, 18 * 60, by = 5), n_patients, replace = TRUE)
pacu_discharge <- (pacu_start + pacu_minutes) %% (24 * 60)

# Ice-lolly exposure variables. On intervention days, about two thirds consume
# the offered ice lolly; on control days neither offering nor consumption occurs.
capri_offered <- treatment
capri_consumed <- ifelse(treatment == 1L, rbern(0.685, n_patients), 0L)

# Symptom outcomes and pain scores. Probabilities are chosen to resemble the
# observed rarity of nausea, vomiting, and delirium while preserving plausible
# relationships with age, ASA, surgery type, and PACU time.
nrs_lambda_start <- 1.55 + 0.25 * (sex == 1L) + 0.15 * (asa %in% c("III", "IV"))
nrs_lambda_discharge <- 0.95 + 0.10 * (nrs_lambda_start > 1.6)

nrs_start <- pmin(10, stats::rpois(n_patients, lambda = nrs_lambda_start))
nrs_start[rbern(0.42, n_patients) == 1L] <- 0
nrs_dc <- pmin(5, stats::rpois(n_patients, lambda = nrs_lambda_discharge))
nrs_dc[rbern(0.38, n_patients) == 1L] <- 0

nausea_prob <- plogis(-3.20 - 0.15 * treatment + 0.30 * (sex == 1L) + 0.25 * (pacu_minutes > 120))
vomit_prob <- plogis(-4.45 - 0.25 * treatment + 0.20 * (nrs_start >= 4))
ae_prob <- plogis(1.55 - 0.18 * treatment + 0.40 * (nrs_start >= 4) + 0.55 * (nausea_prob > 0.05))

nausea <- rbern(nausea_prob, n_patients)
vomiting <- rbern(vomit_prob, n_patients)
antiemetic <- rbern(ae_prob, n_patients)

delirium_prob <- plogis(-5.20 + 0.70 * (age %in% c("76-85", "86-95")) +
                          0.50 * (asa %in% c("III", "IV")) + 0.35 * (surgery_type == 2))
delirium_event <- rbern(delirium_prob, n_patients)
nudesc <- ifelse(delirium_event == 1L, sample_cat(2:5, c(0.60, 0.30, 0.07, 0.03), n_patients), 0L)
nudesc[delirium_event == 0L & rbern(0.015, n_patients) == 1L] <- 1L

# Medication variables. No administration is stored as missing, matching the
# original raw workbook; the analysis code later converts missing doses to zero.
make_dose <- function(any_prob, dose_values, dose_probabilities) {
  any_med <- rbern(any_prob, n_patients)
  dose <- rep(NA_real_, n_patients)
  dose[any_med == 1L] <- sample_cat(dose_values, dose_probabilities, sum(any_med == 1L))
  dose
}

piri_mg <- make_dose(
  plogis(-0.25 - 0.10 * treatment + 0.25 * (nrs_start >= 4)),
  c(1, 2, 3.75, 4, 6, 7.5, 10, 15, 22.5, 30, 45),
  c(0.02, 0.04, 0.25, 0.02, 0.02, 0.30, 0.02, 0.25, 0.04, 0.03, 0.01)
)

dexketo_mg <- make_dose(
  plogis(-0.72 + 0.02 * treatment),
  c(25, 50, 100),
  c(0.01, 0.98, 0.01)
)

ibu_mg <- make_dose(
  plogis(-3.35 + 0.05 * treatment),
  c(100, 150, 200, 250, 300, 350, 400, 600),
  c(0.02, 0.04, 0.35, 0.04, 0.05, 0.02, 0.45, 0.03)
)

parac_mg <- make_dose(
  plogis(-0.70 - 0.03 * treatment),
  c(100, 125, 200, 250, 400, 500, 750, 1000, 2000),
  c(0.01, 0.01, 0.01, 0.01, 0.02, 0.03, 0.01, 0.89, 0.01)
)

metamizol_g <- make_dose(
  plogis(-1.08 - 0.05 * treatment),
  c(0.3, 0.5, 1, 1.5, 2),
  c(0.01, 0.02, 0.94, 0.01, 0.02)
)

piri_kg <- piri_mg / 75

# Add a small amount of missingness to variables that had rare missing values.
nrs_start[rbern(0.001, n_patients) == 1L] <- NA
nrs_dc[rbern(0.004, n_patients) == 1L] <- NA
nudesc[rbern(0.001, n_patients) == 1L] <- NA

pseudo_data <- data.frame(
  ID = seq_len(n_patients),
  Date = study_dates[day_id],
  ICEDAY = treatment,
  Sex = sex,
  Age = age,
  Weight = weight,
  Height = height,
  ASA = asa,
  Surg_Type = surgery_type,
  AN_Mode = an_mode,
  PDA = pda,
  RA = ra,
  `DurAn_hh:mm:ss` = clock_time(anesthesia_minutes),
  `DurSurg_hh:mm:ss` = clock_time(surgery_minutes),
  PACU_Start_time = clock_time(pacu_start),
  PACU_Discharge_time = clock_time(pacu_discharge),
  `Dur_AWR_hh:mm:ss` = clock_time(pacu_minutes),
  NRS_Start = nrs_start,
  NRS_DC = nrs_dc,
  Nudesc_DC = nudesc,
  Piri_mg = piri_mg,
  PIRI_kg = piri_kg,
  Dexketo_mg = dexketo_mg,
  Ibu_mg = ibu_mg,
  Parac_mg = parac_mg,
  Metamizol_g = metamizol_g,
  `AE_y/n` = antiemetic,
  `NAUS_y/n` = nausea,
  `VOM_y/n` = vomiting,
  Capri_offered = capri_offered,
  Capri_consumed = capri_consumed,
  check.names = FALSE
)

stopifnot(nrow(pseudo_data) == n_patients)
stopifnot(!anyDuplicated(pseudo_data$ID))
stopifnot(all(tapply(pseudo_data$ICEDAY, pseudo_data$Date, function(x) length(unique(x))) == 1))
stopifnot(all(pseudo_data$Capri_consumed[pseudo_data$ICEDAY == 0L] == 0L))

writexl::write_xlsx(
  list(Tabelle2 = pseudo_data),
  path = file.path(paths$data, "EIS_R_pseudodata.xlsx")
)

message("Pseudo-data written to data/EIS_R_pseudodata.xlsx")
