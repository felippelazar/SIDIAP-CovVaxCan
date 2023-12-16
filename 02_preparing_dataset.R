# ============================================================================ #
# 2. Processing Dataset for Analysis  #
# Author: Felippe Lazar, IDIAP Jordi Gol, 2023 #
# ============================================================================ #

# Connecting Database 
library(DBI)
library(RPostgres)
library(CDMConnector)
library(tidyverse)
library(readxl)
library(here)
library(CirceR)
library(gtsummary)
library(tidylog)
# usethis::edit_r_environ()

# Setting Connection - Credentials and Database 
db <- dbConnect(RPostgres::Postgres(),
                dbname = Sys.getenv('DB_SERVER_DATABASE'),
                port = Sys.getenv('DB_PORT'),
                host = Sys.getenv('DB_HOST'),
                user = Sys.getenv('DB_USER'),
                password = Sys.getenv('DB_PASSWORD'))

# Setting Up Connection with SIDIAP Database and Retrieving Results Tables and CDM All
cdm_database_schema <-"omop22t2_cmbd"
vocabulary_database_schema <-"omop22t2_cmbd"
results_database_schema <- "results22t2_cmbd"
tableResults <- 'covcanvac_cohorts'
# tableResults2 <- 'covcanvac_cohorts_2'

cdm <- CDMConnector::cdm_from_con(con = db,
                                  cdm_schema = cdm_database_schema,
                                  write_schema = results_database_schema,
                                  cohort_tables = c(tableResults))



cdm[[tableResults]] %>% group_by(cohort_definition_id) %>% 
  tally()

# Cohort Definitions - 'covcanvac_cohorts'
# ID01 - [CovCanVac] Cancer Excluding Non-Melanoma Skin Cancer, N = 1,551,243
# ID02 - [CovCanVac] COVID-19 Testing PCR or Antigen (Excluding Antibody), N = 7,622,831
# ID03 - [CovCanVac] COVID-19 Vaccine, N = 11.328.739
# ID04 - [CovCanVac] COVID-Diagnosis (Clinical or Laboratorial), N = 2,830,850
# ID05 - [CovCanVac] COVID-Diagnosis Hospitalized (Clinical or Laboratorial) 21d.bef - alld.aft, N = 105,587
# ID06 - [CovCanVac] Hospitalization with MV, Tracheostomy or ECMO, N = 103,696
# ID07 - [CovCanVac] Cancer Excluding Non-Melanoma Skin Cancer (Strict Definition), N = 1,473,394
# ID08 - [CovCanVac] Solid Cancer Metastatic Disease, N = 302,566
# ID09 - [CovCanVac] COVID-19 Testing PCR or Antigen (Excluding Antibody) - Negative Test, N = 6,581,644
# ID10 - [CovCanVac] COVID-19 Testing PCR or Antigen (Excluding Antibody) - Positive Test, N = 1,894,489
# ID11 - [CovCanVac] COVID-19 Hospitalization (Clinical or Laboratorial) - 21D.bef - 3D.aft, N = 104,770
# ID12 - [CovCanVac] COVID-Diagnosis (Laboratorial Only), N = 2,025,854
# ID13 - [CovCanVac] # ID11 - [CovCanVac] COVID-19 Hospitalization (Clinical or Laboratorial) - 14D.bef - 3D.aft

#-- In the following steps, we will create a main cohort and many exposure dataframes which will be left joined with the main cohort
#-- Creating main cohort and subjects demographics variables
# Cancer Excluding Non-Melanoma Skin Cancer Tidying - Cancer Exposure Cohort
# Importing Cohort (ID = 1) from Table Results in CDM
cancerCohort <- cdm[[tableResults]] %>% filter(cohort_definition_id == 1) %>% collect()

# Creating Cancer Per Patient and Only First and Last Cancer Diagnosis per Patient ID and the Time Delta in Days
cancerCohort <- cancerCohort %>%
  arrange(subject_id, cohort_start_date) %>%
  group_by(subject_id) %>%
  summarise(cancer_diag_first_date = min(cohort_start_date, na.rm = T),
            cancer_diag_last_date = max(cohort_start_date, na.rm = T)) %>%
  filter(cancer_diag_first_date > as.Date.character('27/12/2020', format = '%d/%m/%Y') - 5*365)

# Getting Observation Periods - Beginning and End of Follow-up Data
cancerCohort <- cancerCohort %>%
  left_join(cdm$observation_period %>% select(person_id, observation_period_start_date, observation_period_end_date) %>% collect(),
            by = c('subject_id' = 'person_id')) %>%
  rename(end_db_followup = observation_period_end_date,
         begin_db_followup = observation_period_start_date)

# Getting Person Attributes - Gender, Date of Birth, Location, and Death Date
cancerCohort <- cancerCohort %>%
  left_join(cdm$person %>% select(person_id, gender_concept_id, year_of_birth, month_of_birth, day_of_birth, location_id) %>% collect(),
            by = c('subject_id' = 'person_id')) %>%
  left_join(cdm$death %>% select(person_id, death_date) %>% collect(),
            by = c('subject_id' = 'person_id')) %>%
  left_join(cdm$location %>% select(location_id, location_source_value) %>% collect(),
            by = c('location_id' = 'location_id')) %>%
  mutate(dob = as.Date.character(paste(day_of_birth, month_of_birth, year_of_birth, sep = '/'), format = '%d/%m/%Y')) %>%
  rename(location = location_source_value) %>%
  select(-year_of_birth, -month_of_birth, -day_of_birth, -location_id)

# Getting ABS region and related variables
# Importing table of coding references for ABS, AGA, COMARCA and Municipality in Catalunia
absTable <- readRDS('abs_aga_comarca_municipality.rds')

# Joining with the ABS value from SIDIAP and the Cancer Cohort
cancerCohort <- cancerCohort %>%
  left_join(cdm$observation %>% 
              filter(observation_source_value == 'abs') %>% 
              select(person_id, observation_date, value_as_number) %>% collect(),
            by = c('subject_id' = 'person_id')) %>%
  rename(abs_value_as_number = value_as_number) %>%
  left_join(absTable %>% select(abs_code, aga_code, aga_name) %>% mutate(abs_code = as.numeric(abs_code)), 
            by = c('abs_value_as_number' = 'abs_code')) %>%
  rename(abs_code = abs_value_as_number)

rm(absTable)
# Creating a vector cancer subject IDS which will be used in furthers analysis (to speed-up process in the CDM)
cancerIDS <- cancerCohort %>% pull(subject_id)

# Getting Cancer Diagnosis by cancer Type
# Importing Cohort (ID = 1) from Table Results in CDM
cancerDiagnosis <- cdm[[tableResults]] %>% filter(cohort_definition_id == 1) %>% collect()

cancerDiagnosis <- cancerDiagnosis %>%
  filter(subject_id %in% cancerIDS) %>%
  left_join(cdm$condition_occurrence %>% filter(person_id %in% cancerIDS) %>%
              select(person_id, condition_start_date, condition_concept_id) %>%
              collect(),
            by = c('subject_id' = 'person_id', 'cohort_start_date' = 'condition_start_date'))

cancerDiagnosisIDS <- cancerDiagnosis %>% pull(condition_concept_id) %>% unique()
cancerDiagnosis <- cancerDiagnosis %>%
  left_join(cdm$concept %>% filter(concept_id %in% cancerDiagnosisIDS) %>%
              select(concept_id, concept_name) %>%
              collect(),
            by = c('condition_concept_id' = 'concept_id'))

rm(cancerDiagnosisIDS)
# Importing Categorized Codes
cancerDiagnosisCodes <- read_excel(here('CancerDiagnosis26042023.xlsx'))

cancerDiagnosis <- cancerDiagnosis %>%
  left_join(cancerDiagnosisCodes %>% filter(include == 1),
            by = c('condition_concept_id' = 'condition_concept_id'))

cancerDiagnosisWide <- cancerCohort %>%
  select(subject_id) %>%
  left_join(cancerDiagnosis %>% filter(include == 1),
            by = c('subject_id' = 'subject_id')) %>%
  arrange(subject_id, cohort_start_date) %>%
  distinct(subject_id, cancer_dx, .keep_all=T) %>%
  select(subject_id, cohort_start_date, cancer_dx) %>%
  pivot_wider(names_from = c('cancer_dx'), values_from = 'cohort_start_date',  names_glue = "cancer_dx_{cancer_dx}") %>%
  rename_with(~ str_replace_all(., '[- ]+', '_'), starts_with("cancer_dx_")) %>%
  mutate(across(starts_with('cancer_dx_'), ~ ifelse(is.na(.x), 0, 1))) %>%
  select(-cancer_dx_NA) %>%
  mutate(cancer_dx_other_2 = if_else(rowSums(.[2:ncol(.)]) == 0, 1, 0))

cancerDiagnosisGroupWide <- cancerCohort %>%
  select(subject_id) %>%
  left_join(cancerDiagnosis %>% filter(include == 1),
            by = c('subject_id' = 'subject_id')) %>%
  arrange(subject_id, cohort_start_date) %>%
  distinct(subject_id, cancer_group, .keep_all=T) %>%
  select(subject_id, cohort_start_date, cancer_group) %>%
  pivot_wider(names_from = c('cancer_group'), values_from = 'cohort_start_date',  names_glue = "cancer_group_{cancer_group}") %>%
  rename_with(~ str_replace_all(., '[- ]+', '_'), starts_with("cancer_group_")) %>%
  mutate(across(starts_with('cancer_group_'), ~ ifelse(is.na(.x), 0, 1))) %>%
  select(-cancer_group_NA) %>%
  mutate(cancer_group_other_2 = if_else(rowSums(.[2:ncol(.)]) == 0, 1, 0))

rm(cancerDiagnosisCodes)
rm(cancerDiagnosis)
#-- Getting Vaccination Exposure Concept IDS and dates
# ID3 - [CovCanVac] COVID-19 Vaccine, N = 11.328.739
# Pfizer-Biontech mRNA, ConceptID = 37003436
# Moderna mRNA, ConceptID = 37003518
# AZ ChAdOx1, ConceptID = 724905
# Jansen Ad26, ConceptID = 739906

# Importing Cohort from Results Table (ID = 3)
covidVaccine <- cdm[[tableResults]] %>% filter(cohort_definition_id == 3) %>% collect()

# Matching Vaccine Administered with Vaccine Type by Person and Date of Administration
covidVaccine <- covidVaccine %>%
  filter(subject_id %in% cancerIDS) %>%
  left_join(cdm$drug_exposure %>% 
              filter(drug_concept_id %in% c('37003436', '37003518', '724905', '739906')) %>%
              select(person_id, drug_concept_id, drug_exposure_start_date) %>% collect(), 
            by = c('subject_id' = 'person_id', 'cohort_start_date' = 'drug_exposure_start_date'))

# Renaming and Labeling Vaccine Products
covidVaccine <- covidVaccine %>%
  rename(drug_exposure_date = cohort_start_date) %>%
  mutate(drug_concept_id = factor(drug_concept_id, 
                                  levels = c('37003436', '37003518', '724905', '739906'),
                                  labels = c('Pfizer-mRNA-BNT162b', 'Moderna-mRNA-1273', 'AZ-ChAdOx1', 'Jansen-Ad26')))

# Excluding Duplicates and Numbering Vaccine Doses
covidVaccine <- covidVaccine %>%
  arrange(subject_id, drug_exposure_date) %>%
  distinct(subject_id, drug_exposure_date, .keep_all = T) %>%
  mutate(dose_number = unlist(mapply(
    function(len, val) if (val == 0) rep(0, len) else 1:len,
    rle(as.numeric(subject_id))$lengths, rle(as.numeric(subject_id))$values))) %>%
  mutate(dose_number = ifelse(is.na(drug_concept_id), NA, dose_number))

# Creating Lag Days between Vaccine Doses
covidVaccine <- covidVaccine %>%
  mutate(vac_lag_days = as.numeric(drug_exposure_date - lag(drug_exposure_date))) %>%
  mutate(vac_lag_days = ifelse(dose_number == 1, NA, vac_lag_days))

# Merging with Cancer Cohort
cancerVaccine <- cancerCohort %>%
  left_join(covidVaccine %>% 
              select(subject_id, drug_exposure_date, drug_concept_id, dose_number, vac_lag_days),
            by = c('subject_id' = 'subject_id'))

cancerVaccineWide <- cancerVaccine %>%
  pivot_wider(id_cols = subject_id, names_from = c('dose_number'), 
              values_from = c('drug_exposure_date', 'drug_concept_id', 'vac_lag_days'),
              names_glue = "{.value}_{dose_number}") %>%
  rename_with(~str_replace(., 'drug', 'vac')) %>%
  select(-ends_with('_NA'))

rm(cancerVaccine)
rm(covidVaccine)

#-- Getting COVID-19 diagnosis 
# ID4 - [CovCanVac] COVID-Diagnosis (Clinical or Laboratorial), N = 2,830,850
# Importing Cohort from Results Table (ID = 4)
covidDiagnosis <- cdm[[tableResults]] %>% filter(cohort_definition_id == 4) %>% collect()

# Dropping Duplicates, Renaming Date Column, Arranging and Creating Number of Infections per Patient
covidDiagnosis <- covidDiagnosis %>%
  filter(subject_id %in% cancerIDS) %>%
  distinct(subject_id, cohort_start_date, .keep_all = T) %>%
  select(subject_id, cohort_start_date) %>%
  rename(covid_date = cohort_start_date) %>%
  arrange(subject_id, covid_date) %>%
  mutate(infection_number = unlist(mapply(
    function(len, val) if (val == 0) rep(0, len) else 1:len,
    rle(as.numeric(subject_id))$lengths, rle(as.numeric(subject_id))$values))) %>%
  mutate(infection_number = ifelse(is.na(covid_date), NA, infection_number))

# Creating Lag Infection Days between Diagnosis
covidDiagnosis <- covidDiagnosis %>%
  mutate(infection_lag_days = covid_date - lag(covid_date)) %>%
  mutate(infection_lag_days = ifelse(infection_number == 1, NA, infection_lag_days))

# Joining with Cancer Diagnosis
covidCancerDiagnosis <- cancerCohort %>%
  left_join(covidDiagnosis, by = c('subject_id' = 'subject_id'))

# Widening Lag Days and COVID Date of Infection among Cancer Patients
covidCancerDiagnosisWide <- covidCancerDiagnosis %>%
  pivot_wider(id_cols = subject_id, names_from = c('infection_number'), 
              values_from = c('covid_date', 'infection_lag_days'),
              names_glue = "{.value}_{infection_number}") %>%
  select(-ends_with('_NA'))

rm(covidDiagnosis)
rm(covidCancerDiagnosis)
#-- Getting COVID-19 hospitalizations
# ID5 - [CovCanVac] COVID-Diagnosis Hospitalized (Clinical or Laboratorial), N = 103,856
# Importing Cohort from Results Table (ID = 5)
covidHosp <- cdm[[tableResults]] %>% filter(cohort_definition_id == 5) %>% collect()

# Dropping Duplicates and Couting Number of Hospitalizations
covidHosp <- covidHosp %>%
  filter(subject_id %in% cancerIDS) %>%
  distinct(subject_id, cohort_start_date, .keep_all = T) %>%
  select(subject_id, cohort_start_date, cohort_end_date) %>%
  rename(hosp_admission_date = cohort_start_date,
         hosp_discharge_date = cohort_end_date) %>%
  arrange(subject_id, hosp_admission_date) %>%
  mutate(hosp_number = unlist(mapply(
    function(len, val) if (val == 0) rep(0, len) else 1:len,
    rle(as.numeric(subject_id))$lengths, rle(as.numeric(subject_id))$values))) %>%
  mutate(hosp_number = ifelse(is.na(hosp_number), NA, hosp_number)) %>%
  mutate(hosp_duration = as.numeric(hosp_discharge_date - hosp_admission_date))

# Joining with Cancer Diagnosis
covidCancerHosp <- cancerCohort %>%
  left_join(covidHosp, by = c('subject_id' = 'subject_id'))

covidCancerHospWide <- covidCancerHosp %>%
  pivot_wider(id_cols = subject_id, names_from = c('hosp_number'), 
              values_from = c('hosp_admission_date', 'hosp_discharge_date'),
              names_glue = "{.value}_{hosp_number}") %>%
  select(-ends_with('_NA'))

# ID6 - [CovCanVac] Hospitalization with MV, Tracheostomy or ECMO, N = 103,696
# Importing Cohord from Results Table (ID = 5)
covidSevere <- cdm[[tableResults]] %>% filter(cohort_definition_id == 6) %>% collect()

covidSevere <- covidHosp %>%
  left_join(covidSevere %>% rename(severe_covid_date = cohort_start_date) %>% select(subject_id, severe_covid_date),
            by = c('subject_id' = 'subject_id')) %>%
  filter(severe_covid_date >= hosp_admission_date & severe_covid_date <= hosp_discharge_date) %>%
  distinct(subject_id, hosp_number, .keep_all = TRUE) %>%
  select(subject_id, hosp_number, hosp_admission_date, hosp_discharge_date) %>%
  arrange(subject_id, hosp_admission_date) %>%
  mutate(hosp_number = unlist(mapply(
    function(len, val) if (val == 0) rep(0, len) else 1:len,
    rle(as.numeric(subject_id))$lengths, rle(as.numeric(subject_id))$values)))

covidSevereHospWide <- cancerCohort %>%
  left_join(covidSevere, by = c('subject_id' = 'subject_id')) %>%
  rename(hosp_severe_admission_date = hosp_admission_date,
         hosp_severe_discharge_date = hosp_discharge_date) %>%
  pivot_wider(id_cols = subject_id, names_from = c('hosp_number'), 
              values_from = c('hosp_severe_admission_date', 'hosp_severe_discharge_date'),
              names_glue = "{.value}_{hosp_number}") %>%
  select(-ends_with('_NA'))

rm(covidHosp)
rm(covidCancerHosp)
rm(covidSevere)

#-- Getting MEDEA values
# Importing MEDEA Concepts
medea2001 <- cdm$observation %>% 
  filter(observation_source_value	== "medea") %>% 
  select(person_id, value_as_string) %>%
  collect() %>% 
  rename(medea_group_2001 = value_as_string)

medea2011 <- cdm$observation %>% 
  filter(observation_source_value	== "medea11") %>% 
  select(person_id, value_as_string) %>%
  collect() %>% 
  rename(medea_group_2011 = value_as_string)

#-- Getting Care Home Variable (patients living in care-home)
# Importing Care Home Concept 
careHome <- cdm$observation %>% 
  filter(observation_concept_id == "44791364") %>% 
  distinct(person_id) %>% 
  mutate(living_care_home = 1) %>%
  collect()

# Creating Charlson Comorbidity Index - Romano Adpation 
# Adpated from OHDSI Feature Extraction Package - see link below for more details.
# https://github.com/OHDSI/FeatureExtraction/blob/main/inst/sql/sql_server/CharlsonIndex.sql#L19

# Importing Table Long Charlson Descendants Concepts IDS -- see above commented code on how this was created
charlsonIndexTable <- read.table(paste0(here(), '/CharlsonIDS_17042023.csv'), sep = ';')
charlsonIndexIDs <- charlsonIndexTable %>% pull(descendant_concept_id)

# Joining ConceptIDs from Charlson with Condition Ocurrence of Cohort - Excluding Duplicated Condition Occurrences
cancerCCI <- cancerCohort %>% select(subject_id) %>%
  left_join(cdm$condition_occurrence %>% select(person_id, condition_concept_id, condition_start_date, condition_end_date) %>%
              filter(condition_concept_id %in% charlsonIndexIDs) %>% 
              collect(), 
            by = c('subject_id' = 'person_id')) %>% 
  distinct(subject_id, condition_concept_id, .keep_all = T) %>% 
  left_join(charlsonIndexTable, by = c('condition_concept_id' = 'descendant_concept_id'), relationship = "many-to-many")

# Pivot-Widening the Cohort after Filtering by Start Date of Condition Ocurrence 
cancerCCIWide <- cancerCCI %>%
  #filter(coalesce(condition_start_date >= as.Date.character('01/12/2017', format = '%d/%m/%Y'), TRUE)) %>%
  distinct(subject_id, charlson_comorbidity, .keep_all = T) %>%
  pivot_wider(id_cols = c('subject_id'), names_from = c('charlson_comorbidity'), values_from = c('charlson_weight'), names_prefix = 'CCI_')

cancerCCIWide <- cancerCCIWide %>%
  mutate(CCI_Any_Malignancy = 2) %>%
  mutate(charlson_index = rowSums(across(starts_with('CCI')), na.rm = T)) %>%
  mutate(across(starts_with('CCI'), ~ ifelse(is.na(.x), 0, 1))) %>% 
  select(-CCI_NA)

rm(cancerCCI)
rm(charlsonIndexTable)
rm(charlsonIndexIDs)
#-- Getting dates of predominant COVID Variants of Concern (VOC)
# Data exported from GISAID - pre-processed in another R Script.
covidVOC <- read.table('voc_voi_catalonia_20042023.csv', sep = ',', header = T)

# Renaming VOC 
covidVOC <- covidVOC %>%
  mutate(date = as.Date(date)) %>%
  rename(date_variant = date) %>%
  mutate(name_variant = case_when(
    grepl('Delta', variant) ~ 'Delta VOC',
    grepl('Omicron', variant) ~ 'Omicron VOC',
    TRUE ~ 'Other VOC'
  ))

# Summarizing Weekly Proportion of Tests Positive for Variant and Getting the First Date of Each VOC Predominance
covidVOC <- covidVOC %>%
  mutate(week_variant = strftime(date_variant, format = "%V-%Y")) %>%
  arrange(date_variant) %>%
  group_by(week_variant) %>%
  mutate(n_week_test = n()) %>%
  ungroup() %>%
  group_by(week_variant, name_variant) %>%
  mutate(n_week_variant = n()) %>% 
  ungroup() %>%
  distinct(date_variant, week_variant, name_variant, n_week_variant, n_week_test) %>%
  mutate(prop_week_variant = n_week_variant/n_week_test) %>%
  arrange(date_variant, week_variant, desc(prop_week_variant)) %>%
  distinct(date_variant, .keep_all=T) %>%
  mutate(lag_variant_eval = ifelse(name_variant == lag(name_variant), 1, 0)) %>%
  mutate(lag_variant_eval = ifelse(is.na(lag_variant_eval), 0, lag_variant_eval)) %>%
  filter(lag_variant_eval == 0) %>%
  select(date_variant, name_variant) %>%
  pivot_wider(names_from = 'name_variant', values_from = 'date_variant') %>%
  as.vector()

#-- Extracting Health-Seeking Behaviour from Patients List of Medical Visits in the past year
# Visit Concept ID 9201 - Hospital
# Visit Concept ID 9202 - Outpatient
# Visit Concept ID 5083 - Telehealth
# Visit Concept ID 581476 - Home
# Visit Concept ID 32037 - UTI

# Extracting cancer Patients visits
cancerVisits <- cancerCohort %>% select(subject_id) %>%
  left_join(cdm$visit_occurrence %>% 
              filter(person_id %in% cancerIDS) %>% 
              filter(year(visit_start_date) %in% c('2019', '2020')) %>%
              select(person_id, visit_start_date, visit_concept_id) %>% collect(),
            by = c('subject_id' = 'person_id'))

# Turning into Wide Format the Count number of Visits in the year prior to beginning of vaccination
visitsWide <- cancerVisits %>%
  filter(visit_start_date < as.Date.character('27/12/2020', format = '%d/%m/%Y') &
           visit_start_date >= as.Date.character('27/12/2019', format = '%d/%m/%Y')) %>%
  mutate(visit_concept_id = factor(visit_concept_id, levels = c('9202', '581476', '5083', '9201', '32037'),
                                   labels = c('outpatient', 'home', 'telehealth', 'inpatient', 'icu'))) %>%
  group_by(subject_id, visit_concept_id) %>%
  count() %>%
  pivot_wider(names_from = 'visit_concept_id', values_from = 'n',
              names_glue = "n_visits_{visit_concept_id}") %>%
  replace_na(replace = list(n_visits_outpatient = 0, n_visits_telehealth = 0, 
                            n_visits_inpatient = 0, n_visits_home = 0, n_visits_icu = 0)) %>%
  mutate(n_visits_total = sum(n_visits_outpatient + n_visits_telehealth + n_visits_inpatient + n_visits_home + n_visits_icu))

cancerVisitsWide <- cancerCohort %>% select(subject_id) %>%
  left_join(visitsWide) %>%
  replace_na(replace = list(n_visits_outpatient = 0, n_visits_telehealth = 0, n_visits_inpatient = 0, 
                            n_visits_home = 0, n_visits_icu = 0, n_visits_total = 0))

rm(cancerVisits)
rm(visitsWide)

#-- Extracting PCR COVID-19 tests 
# ID2 - [CovCanVac] COVID-19 Testing PCR or Antigen (Excluding Antibody), N = 7,622,831
cohortTests <- cdm[[tableResults]] %>% filter(cohort_definition_id == 2) %>% collect()

# Measurements IDs of COVID-tests in the database
# Concept ID 586310 - Measurement of (SARS-CoV-2) Genetic material using Molecular method
# Concept ID 37310257 - Measurement of (SARS-CoV-2) antigen
# Concept ID 37310258 - Measurement of (SARS-CoV-2) antibody
covidTests <- cdm$measurement %>% 
  filter(year(measurement_date) %in% c('2020', '2021', '2022')) %>%
  filter(measurement_concept_id %in% c('586310', '37310257')) %>%
  select(person_id, measurement_date, measurement_concept_id, value_as_concept_id) %>%
  collect()

cancerCovidTests <- cohortTests %>%
  filter(subject_id %in% cancerIDS) %>%
  left_join(covidTests, by = c('subject_id' = 'person_id', 'cohort_start_date' = 'measurement_date')) %>%
  distinct(subject_id, cohort_start_date, measurement_concept_id, value_as_concept_id)

# Values Concept IDs Results 
# Concept ID 45878583 - Negative
# Concept ID 45884084 - Positive
# Concept ID 45880649 - Undetermined
# Concept ID 45879592 - Likely
cancerCovidTests <- cancerCovidTests %>%
  # Assigning Type of COVID-19 tests
  mutate(measurement_concept_id = factor(measurement_concept_id, 
                                         levels = c('586310', '37310257'),
                                         labels = c('PCR', 'antigen'))) %>%
  # Creating Result Variable
  mutate(test_result = case_when(
    value_as_concept_id %in% c('45884084', '586310') ~ 1, 
    value_as_concept_id %in% c('45880649', '45879592') ~ 99,
    value_as_concept_id %in% c('45878583') ~ 0))

# Turning the number of tests (positive, negative, undetermined) were done for each patient during all follow-up
covidTestsWide <- cancerCovidTests %>%
  group_by(subject_id, test_result) %>% count() %>%
  pivot_wider(names_from = 'test_result', values_from = 'n', 
              names_glue = "n_covid_tests_{test_result}") %>%
  replace_na(replace = list(n_covid_tests_0 = 0, n_covid_tests_1 = 0, n_covid_tests_99 = 0)) %>%
  mutate(n_covid_tests = sum(n_covid_tests_0, n_covid_tests_1, n_covid_tests_99))

cancerCovidTestsWide <- cancerCohort  %>%
  left_join(covidTestsWide,
            by = c('subject_id' = 'subject_id')) %>%
  select(subject_id, starts_with('n_covid_tests')) %>%
  replace_na(replace = list(n_covid_tests_0 = 0, n_covid_tests_1 = 0, n_covid_tests_99 = 0, n_covid_tests = 0))

rm(covidTestsWide)
# Merging all Created Datasets with the Cancer Cohort
cancerMerged <- cancerCohort %>%
  left_join(covidCancerDiagnosisWide, by = c('subject_id')) %>%
  left_join(covidCancerHospWide, by = c('subject_id')) %>%
  left_join(covidSevereHospWide, by = c('subject_id')) %>%
  left_join(cancerVaccineWide, by = c('subject_id')) %>%
  left_join(cancerCCIWide, by = c('subject_id')) %>%
  left_join(medea2011, by = c('subject_id' = 'person_id')) %>%
  left_join(medea2001, by = c('subject_id' = 'person_id')) %>%
  left_join(careHome, by = c('subject_id' = 'person_id')) %>%
  left_join(cancerCovidTestsWide, by = c('subject_id' = 'subject_id')) %>%
  left_join(cancerVisitsWide, by = c('subject_id' = 'subject_id')) %>%
  left_join(cancerDiagnosisWide, by = c('subject_id' = 'subject_id')) %>%
  left_join(cancerDiagnosisGroupWide, by = c('subject_id' = 'subject_id'))

rm(covidCancerDiagnosisWide)
rm(covidCancerHospWide)
rm(covidSevereHospWide)
rm(cancerVaccineWide)
rm(cancerCCIWide)
rm(medea2011)
rm(medea2001)
rm(careHome)
rm(cancerCovidTestsWide)
rm(cancerVisitsWide)
rm(cancerDiagnosisWide)
rm(cancerDiagnosisGroupWide)

# CREATING DATAFRAMES FOR SUB-GROUP ANALYSIS
# Cancer Strict Definition
# ID07 - [CovCanVac] Cancer Excluding Non-Melanoma Skin Cancer (Strict Definition), N = 1,473,394
# Importing Cohort (ID = 7) from Table Results in CDM
cancerStrict <- cdm[[tableResults]] %>% filter(cohort_definition_id == 7) %>% collect()

# Creating Cancer Per Patient and Only First and Last Cancer Diagnosis per Patient ID and the Time Delta in Days
cancerStrict <- cancerStrict %>%
  arrange(subject_id, cohort_start_date) %>%
  group_by(subject_id) %>%
  summarise(cancer_diag_first_date = min(cohort_start_date, na.rm = T),
            cancer_diag_last_date = max(cohort_start_date, na.rm = T)) %>%
  filter(cancer_diag_first_date > as.Date.character('27/12/2020', format = '%d/%m/%Y') - 5*365)

cancerIDS_strict <- cancerStrict %>% pull(subject_id) %>% unique()

# ID11 - [CovCanVac] COVID-Diagnosis Hospitalized (Clinical or Laboratorial) up to 3 days after admission
# Importing Cohort from Results Table (ID = 11)
covidHosp_subgroup <- cdm[[tableResults]] %>% filter(cohort_definition_id == 11) %>% collect()

# Dropping Duplicates and Couting Number of Hospitalizations
covidHosp_subgroup <- covidHosp_subgroup %>%
  filter(subject_id %in% cancerIDS) %>%
  distinct(subject_id, cohort_start_date, .keep_all = T) %>%
  select(subject_id, cohort_start_date, cohort_end_date) %>%
  rename(hosp_admission_date = cohort_start_date,
         hosp_discharge_date = cohort_end_date) %>%
  arrange(subject_id, hosp_admission_date) %>%
  mutate(hosp_number = unlist(mapply(
    function(len, val) if (val == 0) rep(0, len) else 1:len,
    rle(as.numeric(subject_id))$lengths, rle(as.numeric(subject_id))$values))) %>%
  mutate(hosp_number = ifelse(is.na(hosp_number), NA, hosp_number)) %>%
  mutate(hosp_duration = as.numeric(hosp_discharge_date - hosp_admission_date))

# Joining with Cancer Diagnosis
covidCancerHosp_subgroup <- cancerCohort %>%
  left_join(covidHosp_subgroup, by = c('subject_id' = 'subject_id'))

covidCancerHospWide_subgroup <- covidCancerHosp_subgroup %>%
  pivot_wider(id_cols = subject_id, names_from = c('hosp_number'), 
              values_from = c('hosp_admission_date', 'hosp_discharge_date'),
              names_glue = "{.value}_{hosp_number}") %>%
  select(-ends_with('_NA'))

# ID6 - [CovCanVac] Hospitalization with MV, Tracheostomy or ECMO, N = 103,696
# Importing Cohord from Results Table (ID = 5)
covidSevere_subgroup <- cdm[[tableResults]] %>% filter(cohort_definition_id == 6) %>% collect()

covidSevere_subgroup <- covidHosp_subgroup %>%
  left_join(covidSevere_subgroup %>% rename(severe_covid_date = cohort_start_date) %>% select(subject_id, severe_covid_date),
            by = c('subject_id' = 'subject_id')) %>%
  filter(severe_covid_date >= hosp_admission_date & severe_covid_date <= hosp_discharge_date) %>%
  distinct(subject_id, hosp_number, .keep_all = TRUE) %>%
  select(subject_id, hosp_number, hosp_admission_date, hosp_discharge_date) %>%
  arrange(subject_id, hosp_admission_date) %>%
  mutate(hosp_number = unlist(mapply(
    function(len, val) if (val == 0) rep(0, len) else 1:len,
    rle(as.numeric(subject_id))$lengths, rle(as.numeric(subject_id))$values)))

covidSevereHospWide_subgroup <- cancerCohort %>%
  left_join(covidSevere_subgroup, by = c('subject_id' = 'subject_id')) %>%
  rename(hosp_severe_admission_date = hosp_admission_date,
         hosp_severe_discharge_date = hosp_discharge_date) %>%
  pivot_wider(id_cols = subject_id, names_from = c('hosp_number'), 
              values_from = c('hosp_severe_admission_date', 'hosp_severe_discharge_date'),
              names_glue = "{.value}_{hosp_number}") %>%
  select(-ends_with('_NA'))

rm(covidHosp_subgroup)
rm(covidCancerHosp_subgroup)
rm(covidSevere_subgroup)

# Subgroup COVID-19 Laboratorial Only
# ID12 - [CovCanVac] COVID-Diagnosis (Laboratorial Only), N = 2,025,854
covidDiagnosis_subgroup <- cdm[[tableResults]] %>% filter(cohort_definition_id == 12) %>% collect()

# Dropping Duplicates, Renaming Date Column, Arranging and Creating Number of Infections per Patient
covidDiagnosis_subgroup <- covidDiagnosis_subgroup %>%
  filter(subject_id %in% cancerIDS) %>%
  distinct(subject_id, cohort_start_date, .keep_all = T) %>%
  select(subject_id, cohort_start_date) %>%
  rename(covid_date = cohort_start_date) %>%
  arrange(subject_id, covid_date) %>%
  mutate(infection_number = unlist(mapply(
    function(len, val) if (val == 0) rep(0, len) else 1:len,
    rle(as.numeric(subject_id))$lengths, rle(as.numeric(subject_id))$values))) %>%
  mutate(infection_number = ifelse(is.na(covid_date), NA, infection_number))

# Creating Lag Infection Days between Diagnosis
covidDiagnosis_subgroup <- covidDiagnosis_subgroup %>%
  mutate(infection_lag_days = covid_date - lag(covid_date)) %>%
  mutate(infection_lag_days = ifelse(infection_number == 1, NA, infection_lag_days))

# Joining with Cancer Diagnosis
covidCancerDiagnosis_subgroup <- cancerCohort %>%
  left_join(covidDiagnosis_subgroup, by = c('subject_id' = 'subject_id'))

# Widening Lag Days and COVID Date of Infection among Cancer Patients
covidCancerDiagnosisWide_subgroup <- covidCancerDiagnosis_subgroup %>%
  pivot_wider(id_cols = subject_id, names_from = c('infection_number'), 
              values_from = c('covid_date', 'infection_lag_days'),
              names_glue = "{.value}_{infection_number}") %>%
  select(-ends_with('_NA')) %>%
  select(-starts_with('infection_lag_days_'))

rm(covidDiagnosis_subgroup)
rm(covidCancerDiagnosis_subgroup)

# Subgroup - 14 Days before AND up to 3 days after (Hospitalization)
# ID13 - [CovCanVac] COVID-Diagnosis Hospitalized (Clinical or Laboratorial) 14 days before AND up to 3 days after admission
# Importing Cohort from Results Table (ID = 11)
covidHosp_subgroup <- cdm[[tableResults]] %>% filter(cohort_definition_id == 13) %>% collect()

# Dropping Duplicates and Couting Number of Hospitalizations
covidHosp_subgroup <- covidHosp_subgroup %>%
      filter(subject_id %in% cancerIDS) %>%
      distinct(subject_id, cohort_start_date, .keep_all = T) %>%
      select(subject_id, cohort_start_date, cohort_end_date) %>%
      rename(hosp_admission_date = cohort_start_date,
             hosp_discharge_date = cohort_end_date) %>%
      arrange(subject_id, hosp_admission_date) %>%
      mutate(hosp_number = unlist(mapply(
            function(len, val) if (val == 0) rep(0, len) else 1:len,
            rle(as.numeric(subject_id))$lengths, rle(as.numeric(subject_id))$values))) %>%
      mutate(hosp_number = ifelse(is.na(hosp_number), NA, hosp_number)) %>%
      mutate(hosp_duration = as.numeric(hosp_discharge_date - hosp_admission_date))

# Joining with Cancer Diagnosis
covidCancerHosp_subgroup <- cancerCohort %>%
      left_join(covidHosp_subgroup, by = c('subject_id' = 'subject_id'))

covidCancerHospWide_subgroup2 <- covidCancerHosp_subgroup %>%
      pivot_wider(id_cols = subject_id, names_from = c('hosp_number'), 
                  values_from = c('hosp_admission_date', 'hosp_discharge_date'),
                  names_glue = "{.value}_{hosp_number}") %>%
      select(-ends_with('_NA'))

# ID6 - [CovCanVac] Hospitalization with MV, Tracheostomy or ECMO, N = 103,696
# Importing Cohord from Results Table (ID = 6)
covidSevere_subgroup <- cdm[[tableResults]] %>% filter(cohort_definition_id == 6) %>% collect()

covidSevere_subgroup <- covidHosp_subgroup %>%
      left_join(covidSevere_subgroup %>% rename(severe_covid_date = cohort_start_date) %>% select(subject_id, severe_covid_date),
                by = c('subject_id' = 'subject_id')) %>%
      filter(severe_covid_date >= hosp_admission_date & severe_covid_date <= hosp_discharge_date) %>%
      distinct(subject_id, hosp_number, .keep_all = TRUE) %>%
      select(subject_id, hosp_number, hosp_admission_date, hosp_discharge_date) %>%
      arrange(subject_id, hosp_admission_date) %>%
      mutate(hosp_number = unlist(mapply(
            function(len, val) if (val == 0) rep(0, len) else 1:len,
            rle(as.numeric(subject_id))$lengths, rle(as.numeric(subject_id))$values)))

covidSevereHospWide_subgroup2 <- cancerCohort %>%
      left_join(covidSevere_subgroup, by = c('subject_id' = 'subject_id')) %>%
      rename(hosp_severe_admission_date = hosp_admission_date,
             hosp_severe_discharge_date = hosp_discharge_date) %>%
      pivot_wider(id_cols = subject_id, names_from = c('hosp_number'), 
                  values_from = c('hosp_severe_admission_date', 'hosp_severe_discharge_date'),
                  names_glue = "{.value}_{hosp_number}") %>%
      select(-ends_with('_NA'))

rm(covidHosp_subgroup)
rm(covidCancerHosp_subgroup)
rm(covidSevere_subgroup)
