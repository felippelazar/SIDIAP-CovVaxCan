# ============================================================================ #
# 4. Rolling Entry Matching Cohort Analysis - COVID-19 Vaccine 1st and 2nd     #
# Author: Felippe Lazar, IDIAP Jordi Gol, 2023 #
# This code was adapted from Rachel Mulholland <rachel.mulholland@ed.ac.uk> and
#                            Chris Robertson <chrisrobertson@nhs.net>
# ============================================================================ #

library(tidyverse)
library(readxl)
library(here)
library(tidylog)
library(survival)
library(survival)
library(survminer)
source('utils.R')

# The following code creates a matched cohort of vaccinated individuals from propensity scores and matched variables
# This code was adptade from Rachel Mulholland <rachel.mulholland@ed.ac.uk> and Chris Robertson <chrisrobertson@nhs.net>

# -- Applying initial filtering (further filtering will be done in the looping below)
# Minimum date will be beggining of vaccination campaign in Spain - 27th December 2020. 
# End date will be 1 month before Omicron is the dominant variant (> 50% of tests)
# Creating Variables of Interest for Filtering Purposes of Elegibility
cancerREM_dose12 <- cancerMerged %>%
      left_join(influenzaVaccineCancerWide, by = c('subject_id')) %>%
      left_join(cancerAnyHospWide, by = c('subject_id')) %>%
  mutate(minDate = as.Date.character('27/12/2020', format = '%d/%m/%Y'), # Vaccination Beginning
         maxDate = as.Date.character('20/11/2021', format = '%d/%m/%Y')) # 1 month before Omicron

#-- Applying inclusion and exclusion criteria based on the date created above 
# Important: Further filtering will be done during the for loop below
cancerREM_dose12 <- cancerREM_dose12 %>%
  # Patient Alive on beginning of the Vaccination
  filter(coalesce(death_date >= minDate, T)) %>%
  # Patient Active on beginning of the Vaccination
  filter(coalesce(end_db_followup >= minDate, T)) %>%
  # Excluding patients with previous COVID-19 Infection 
  filter(coalesce(covid_date_1 >= minDate, T)) %>%
  # Excluding patients with previous COVID-19 hospitalization
  filter(coalesce(hosp_admission_date_1 >= minDate, T)) %>%
  # Filtering by Care Home Living
  filter(is.na(living_care_home))

#-- Tidying Some Variables for Further Analysis
cancerREM_dose12 <- cancerREM_dose12 %>%
  mutate(gender_concept_id = factor(gender_concept_id, levels = c('8507', '8532'), labels = c('M', 'F')),
         visits_outpatient_cat = fct_case_when(
           n_visits_outpatient == 0 ~ '0',
           n_visits_outpatient == 1 ~ '1',
           n_visits_outpatient == 2 ~ '2',
           n_visits_outpatient >= 3 ~ '3+'
         ))

#-- Adjusting Exposure and Outcome dates according to minimum date (date of eligibility)
# Getting column names
col_names_rem <- colnames(cancerREM_dose12)

# Finding names of the columns that are associated with exposures and/or outcomes
any_hosp_admission_vars <- col_names_rem[grepl('any_hosp_admission_date_', col_names_rem)]
flu_vac_date_vars <- col_names_rem[grepl('flu_vac_exposure_date_', col_names_rem)]
covid_date_vars <- col_names_rem[grepl('covid_date_', col_names_rem)]
hosp_admission_vars <- col_names_rem[grepl('^hosp_admission_date_', col_names_rem)]
hosp_severe_admission_vars <- col_names_rem[grepl('hosp_severe_admission_date_', col_names_rem)]

# For Outcomes, creating NA occurrences for dates before minimum date or after maximum date 
cancerREM_dose12 <- cancerREM_dose12 %>%
  # NA occurrences for outcomes before minimum Date
  mutate(across(starts_with('covid_date'), ~ if_else(.x < minDate, as.Date(NA), .x)),
         across(starts_with('hosp_admission_date'), ~ if_else(.x < minDate, as.Date(NA), .x)),
         across(starts_with('hosp_severe_admission_date'), ~ if_else(.x < minDate, as.Date(NA), .x))) %>%
  # NA occurrences for outcomes after maximum Date
  mutate(across(starts_with('covid_date'), ~ if_else(.x > maxDate, as.Date(NA), .x)),
         across(starts_with('hosp_admission_date'), ~ if_else(.x > maxDate, as.Date(NA), .x)),
         across(starts_with('hosp_severe_admission_date'), ~ if_else(.x > maxDate, as.Date(NA), .x)),
         across(starts_with('death_date'), ~ if_else(.x > maxDate, as.Date(NA), .x))) %>% 
  # NA ocurrences for vaccine exposure date greater than maximum date
  mutate(across(starts_with('vac_exposure_date'), ~ if_else(.x > maxDate, as.Date(NA), .x)),
         vac_concept_id_1 = if_else(is.na(vac_exposure_date_1), as.factor(NA), vac_concept_id_1),
         vac_concept_id_2 = if_else(is.na(vac_exposure_date_2), as.factor(NA), vac_concept_id_2),
         vac_concept_id_3 = if_else(is.na(vac_exposure_date_3), as.factor(NA), vac_concept_id_3)) %>%
  # Selecting minimum date of COVID-19 infection and hospitalization (excluding previously created NAs)
  mutate(covid_date = exec(pmin, !!!rlang::syms(covid_date_vars), na.rm = TRUE),
         hosp_admission_date = exec(pmin, !!!rlang::syms(hosp_admission_vars), na.rm = TRUE),
         hosp_severe_admission_date = exec(pmin, !!!rlang::syms(hosp_severe_admission_vars), na.rm = TRUE),
         ) %>%
  # Creating variable death due to COVID-19 (death with a positive test <= 28 days before death)
  mutate(covid_death_date = case_when(death_date - covid_date <= 28 ~ death_date)) %>%
  select(-starts_with('covid_date_'), -starts_with('hosp_admission_date_'), -starts_with('hosp_severe_admission_date_'),
         -starts_with('hosp_discharge_date_'), -starts_with('hosp_severe_discharge_date_'))

# Unselecting Columns to Make Lopping Faster
dfz <- cancerREM_dose12 %>%
  select(-aga_name, -starts_with('infection_lag_days_'), 
         -starts_with('vac_lag_days_'), -starts_with('CCI'),  CCI_Metastatic_Solid_Tumor,  
         -starts_with('n_covid_tests'), -starts_with('cancer_dx'), -starts_with('cancer_group'))

detach(package:tidylog,unload=TRUE)
#-- Matching process: Exact matching of 1% bands, individual age and other covariates to adjustment
# Ineligible matching conditions: 
# 1. If controls were vaccinated before the exposed, 
# 2. If they had a COVID-19 hospitalization or any death before the vaccination date of the exposed.

# Setting up Daily Period for Matching
# Create loop for day from all dates of available 1st dose vaccination
date_list <- cancerREM_dose12 %>% arrange(vac_exposure_date_1) %>%
  drop_na(vac_exposure_date_1) %>% pull(vac_exposure_date_1) %>% unique()

# date_list <- cancerREM_dose12 %>% arrange(vac_exposure_date_1) %>% 
#   drop_na(vac_exposure_date_1) %>% distinct(vac_exposure_date_1) %>% mutate(n = sample(1:nrow(.))) %>% 
#   filter(n <= 50) %>% pull(vac_exposure_date_1)

# Create list to assign daily datasets to posteriorly combine them
z_merge_1st2nd_sens <- list()
eligibles_1st2nd_sens <- list()

#-- Create a loop for the jth day and perform matching
# !! WARNING !!: Long run time
for(j in 1:(length(date_list))){
  
  # j = 1
  # Extract date from previously created date_list
  startDate <- date_list[j]
  # endDate <- date_list[j]+1 - End Date only useful for monthly/weekly or more than one day groupped matching 
  
  # Print to give progress on what time-period loop is on
  print(paste0("-- Time-period: ", j, "/", length(date_list), " (", startDate, ") --"))
  
  # From the main cohort, creating variables based on period dates that were set above ('startDate' +- 'EndDate')
  allCohort <- dfz %>%
    # Creating Age on the Date
    mutate(age = as.numeric(difftime(startDate, dob, units = 'days')/365)) %>%
    mutate(age_group_match = case_when(
      age >= 100 ~ 100,
      age >= 95 & age < 100  ~ 97,
      age >= 90 & age < 95 ~ 92,
      age >= 85 & age < 90 ~ 87,
      age >= 80 & age < 85 ~ 82,
      age >= 75 & age < 80 ~ 77,
      age >= 70 & age < 75 ~ 72,
      age >= 65 & age < 70 ~ 67,
      age >= 60 & age < 65 ~ 62,
      TRUE ~ floor(age/5)*5)
      ) %>%
    mutate(age_group = case_when(
      age >= 18 & age < 50 ~ '18-49',
      age >= 50 & age < 60 ~ '50-59',
      age >= 60 & age < 70 ~ '60-69',
      age >= 70 & age < 80 ~ '70-79',
      age >= 80 & age < 999 ~ '80-115',
    )) %>%
    # Checking previous COVID-19 ocurrences (Hospitalizations or Positive Tests)
    mutate(previous_covid = as.numeric(coalesce(covid_date <= startDate, FALSE)),
           previous_hosp_covid = as.numeric(coalesce(hosp_admission_date <= startDate, FALSE)),
           previous_death =  as.numeric(coalesce(death_date <= startDate, FALSE)),
           previous_vac_1 =  as.numeric(coalesce(vac_exposure_date_1 < startDate, FALSE)),
           moved_out = as.numeric(coalesce(end_db_followup <= startDate, FALSE)),
           noteligibleyet = as.numeric(coalesce(minDate > startDate, FALSE))) %>%
    # Creating Vaccination as Outcomes Dates for Control Groups - Will be used in further Analysis
    mutate(outcome_vac_date_1 = vac_exposure_date_1,
           outcome_vac_date_2 = vac_exposure_date_2,
           outcome_vac_date_3 = vac_exposure_date_3) %>%
    # Setting Vaccination Date to No if After end of period date and creating binary variable to use for GLM (PS matching) and filtering
    mutate(vac_exposure_date_1 = if_else(coalesce(vac_exposure_date_1 > startDate, T), as.Date(NA), vac_exposure_date_1),
           vac_day = if_else(!is.na(vac_exposure_date_1), 1, 0)) %>%
    # Creating Cancer Diagnosis Variables - time from current start date to cancer diagnosis (first diagnosis)
    mutate(cancer_diagnosis_time = as.numeric(difftime(startDate, cancer_diag_first_date, units = 'days')/365),
           cancer_diagnosis_time = factor(floor(cancer_diagnosis_time))) %>%
    # Setting to NA Flu Vaccine Date Greater than 360 days before baseline
    mutate(across(starts_with('flu_vac_exposure_date_'), ~ if_else(.x < (startDate - 365), as.Date(NA), .x)),
           across(starts_with('flu_vac_exposure_date_'), ~ if_else(.x > startDate, as.Date(NA), .x)),
           flu_vac_exposure_date = exec(pmax, !!!rlang::syms(flu_vac_date_vars), na.rm = TRUE),
           previous_flu_vac = if_else(!is.na(flu_vac_exposure_date), 1, 0)) %>%
    # Setting to NA Any Hosp 30 days before baseline
    left_join(dfz %>%
                mutate(across(starts_with('any_hosp_admission_'), ~ if_else(.x < (startDate - 30), as.Date(NA), .x)),
                       across(starts_with('any_hosp_admission_'), ~ if_else(.x > startDate, as.Date(NA), .x)),
                       any_hosp_admission_date = exec(pmax, !!!rlang::syms(any_hosp_admission_vars), na.rm = TRUE),
                       previous_any_hosp_admission = if_else(!is.na(any_hosp_admission_date), 1, 0)) %>%
                select(subject_id, previous_any_hosp_admission)) %>%
    # Creating Outcome Next Hospitalization
    mutate(across(starts_with('any_hosp_admission_'), ~ if_else(.x < startDate, as.Date(NA), .x)),
           any_hosp_admission_date = exec(pmin, !!!rlang::syms(any_hosp_admission_vars), na.rm = TRUE)) %>%
    # Create Enrollment Date
    mutate(enrol_date = startDate) %>%
    # Filtering by First Date of Diagnosis no More than 5 years from 27th December 2020 - Beginning of Vaccination in Catalunia
    filter(cancer_diag_first_date > (startDate - 365*5)) %>%
    filter(cancer_diag_first_date < startDate) %>%
    # Filtering by Age (>= 18 years)
    filter(age >= 18) %>%
    # Filtering Those that Had Already Been Vaccinated, Had COVID-19, Died and did not have further follow-up Before the day of Vaccination
    filter(previous_covid == 0) %>%
    filter(previous_hosp_covid == 0) %>%
    filter(previous_any_hosp_admission == 0) %>%
    filter(previous_death == 0) %>%
    filter(previous_vac_1 == 0) %>%
    filter(moved_out == 0) %>% 
    filter(noteligibleyet == 0) %>%
    select(-starts_with('flu_vac_exposure_date_')) %>%
    select(-starts_with('any_hosp_admission_date_'))
    
    eligibles_1st2nd_sens[[j]] <- allCohort %>% select(subject_id, age, age_group, cancer_diagnosis_time, vac_day, enrol_date, previous_flu_vac)
    ## Propensity score 
    # Model: Age, sex, cancer diagnosis time, charlson index, MEDEA 2001 index, AGA, metastasis, health care usage and previous vac scheme
    model.ps <- glm(vac_day ~ age + gender_concept_id + cancer_diagnosis_time + charlson_index + 
                      medea_group_2001 + aga_code + CCI_Metastatic_Solid_Tumor + n_visits_outpatient, 
                   data = allCohort, family=binomial)
  
    # Predict onto dataset and group into 1% bands
    z_ps <- predict(model.ps, newdata=allCohort, type="response")
    allCohort <- allCohort %>% 
      mutate(prop_score = z_ps) %>%
      mutate(ps_grp = cut(prop_score, breaks=seq(0,1, by=0.05)))
    
    # Creating dataset of vaccinated individuals 
    allVac <- allCohort %>%
      filter(vac_day == 1) %>%
      setNames(paste0('gv_', names(.)))
    
    # Creating dataset of un-vaccinated individuals 
    AllControl <- allCohort %>%
      filter(vac_day == 0) %>%
      setNames(paste0('gc_', names(.))) 
    
    #-- Perform the matching
    # Exact match through propensity scores (grouped), age, AGA, cancer diagnosis time and metastasis
    # Merge the vaccination cohort with the whole cohort by the matching variables
    mergedCohort <- allVac %>%
      left_join(AllControl, 
                by = c('gv_ps_grp' = 'gc_ps_grp', 
                       'gv_gender_concept_id' = 'gc_gender_concept_id',
                       'gv_age_group_match' = 'gc_age_group_match',
                       'gv_aga_code' = 'gc_aga_code',
                       'gv_cancer_diagnosis_time' = 'gc_cancer_diagnosis_time',
                       'gv_previous_flu_vac' = 'gc_previous_flu_vac'
                       ))
  
    rm(AllCohort)
    rm(allVac)
    rm(AllControl)
    # Since merging with characteristics, this will assign all study IDs matching on these 
    # characteristics per vaccinated person in mergedCohort (i.e. multiple IDs per vaccinated person)
    
    #-- Vaccinated control
    # Remove any matches where data of vaccination of control is before vaccination case
    # Note: For daily matching, there should not be any exclusion at this point as data was already filtered in up-steps
    mergedCohort <- mergedCohort %>%
      filter((gc_outcome_vac_date_1 > gv_vac_exposure_date_1 | is.na(gc_outcome_vac_date_1)))
    
    ## Event for control (COVID-19 infection) occurs before paired vaccination
    # Note: For daily matching, there should not be any exclusion at this point as data was already filtered in up-steps
    mergedCohort <- mergedCohort %>%
      filter((gc_covid_date > gv_vac_exposure_date_1 | is.na(gc_covid_date)))
    
    ## Event for control (hospitalization) occurs before paired vaccination
    # Note: For daily matching, there should not be any exclusion at this point as data was already filtered in up-steps
    mergedCohort <- mergedCohort %>%
      filter((gc_hosp_admission_date > gv_vac_exposure_date_1 | is.na(gc_hosp_admission_date)))
    
    ## Event for control (death) occurs before paired vaccination
    # Note: For daily matching, there should not be any exclusion at this point as data was already filtered in up-steps
    mergedCohort <- mergedCohort %>%
      filter((gc_death_date > gv_vac_exposure_date_1 | is.na(gc_death_date)))
    
    # Filtering Patients not Matched at All
    mergedCohort <- mergedCohort %>%
      filter(!is.na(gc_subject_id))
    
    if(j %in% c(50, 100, 150, 200, 250, 300)){gc()}
    # Randomly assign match out of leftovers
    # Assign a random ID to all rows and arranges from highest to lowest
    # Once reordered, take a unique row per vaccinated ID
    set.seed(123)
    if(nrow(mergedCohort) >= 1){
      mergedCohort <- mergedCohort %>% 
        # Setting Limit of Control Subject IDs per Pairing
        arrange(gc_subject_id, sample(1:nrow(.))) %>%
        mutate(nrep_gc_ids = unlist(mapply(
          function(len, val) if (val == 0) rep(0, len) else 1:len,
          rle(as.numeric(gc_subject_id))$lengths, rle(as.numeric(gc_subject_id))$values))) %>%
        arrange(sample(1:nrow(.))) %>%
        # Setting a Maximum of 5 matches per control subject ID per iteration - this should also limit the final number of matches
        filter(nrep_gc_ids <= 5) %>%
        select(-nrep_gc_ids) %>%
        mutate(random_id = runif(nrow(.))) %>% 
        arrange(random_id) %>% # randomly rearrange to make sure that the same controls are not always selected
        filter(!duplicated(gv_subject_id)) %>%  # get one match per vaccinated individuals
        dplyr::select(-random_id)

      z_merge_1st2nd_sens[[j]] <- mergedCohort
      rm(mergedCohort)
      
    }else{rm(mergedCohort)}
    
    if(j %in% c(50, 100, 150, 200, 250, 300)){gc()}
}
