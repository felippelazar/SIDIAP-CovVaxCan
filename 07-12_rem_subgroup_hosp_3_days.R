# ============================================================================ #
# 7. REM Analysis - COVID-19 Vaccine 1st and 2nd     #
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
library(tableone)
source('utils.R')
library(emmeans)
library(broom.helpers)
library(tidycmprsk)
library(ggsurvfit)

# Creating Folder for Exporting Files if Does Not Exist Yet
ifelse(!dir.exists(here('Results')), dir.create(here('Results')), FALSE)
ifelse(!dir.exists(here('Results', 'dose_12')), dir.create(here('Results', 'dose_12')), FALSE)
ifelse(!dir.exists(here('Results', 'dose_12', 'sub_group_hosp_3_days')), dir.create(here('Results', 'dose_12', 'sub_group_hosp_3_days')), FALSE)

# Loading Auxiliary Objects for Analayis
source('aux_objects_rem_12.R')
current_analysis <- 'sub_group_hosp_3_days'

# Creating Boolean Handlers for Analysis (goal: save time when re-running processes)
DO_DESCRIPTIVE <- FALSE
DO_INFECTION <- FALSE
DO_HOSP <- FALSE
DO_SEVERE_HOSP <- FALSE
DO_DEATH <- TRUE
DO_HOSP_DEATH <- TRUE
DO_NON_COVID_DEATH <- TRUE
DO_SUBGROUP_ANALYSIS <- FALSE
DO_COMPETING_RISK <- FALSE

## Merge batched data into the one dataframe
# Make into dataframe
dfREM <- do.call(bind_rows, z_merge_1st2nd)

# HERE WILL BE MANIPULATING THE DATA TO INCLUDE THE OUTCOME OF HOSPITALIZATION UNTIL DAY 3th
dfREMVac <- dfREM %>%
  mutate(gv_subject_pair = paste(gv_subject_id, gc_subject_id, sep  = '-')) %>%
  dplyr::select(starts_with('gv')) %>%
  mutate(tx_group = 1) %>%
  setNames(gsub('gv_', '', names(.)))
  
dfREMControl <- dfREM %>%
  mutate(gc_subject_pair = paste(gv_subject_id, gc_subject_id, sep  = '-')) %>%
  dplyr::select(starts_with('gc'), 
              gv_gender_concept_id, 
              gv_aga_code, 
              gv_cancer_diagnosis_time, 
  ) %>%
  mutate(tx_group = 0) %>%
  setNames(gsub('gc_', '', names(.))) %>%
  setNames(gsub('gv_', '', names(.)))

dfREMlong <- bind_rows(dfREMVac, dfREMControl)
rm(dfREMVac)
rm(dfREMControl)

hosp_admission_vars <- colnames(covidCancerHospWide_subgroup)[grepl('hosp_admission_date_', colnames(covidCancerHospWide_subgroup))]
hosp_severe_admission_vars <- colnames(covidSevereHospWide_subgroup)[grepl('hosp_severe_admission_date_', colnames(covidSevereHospWide_subgroup))]

dfREMlong <- dfREMlong %>%
  select(-hosp_admission_date, -hosp_severe_admission_date) %>%
  left_join(covidCancerHospWide_subgroup, by = c('subject_id')) %>%
  left_join(covidSevereHospWide_subgroup) %>%
  # NA occurrences for outcomes before minimum Date
  mutate(across(contains('hosp_admission_date'), ~ if_else(.x <= enrol_date, as.Date(NA), .x)),
         across(contains('hosp_severe_admission_date'), ~ if_else(.x <= enrol_date, as.Date(NA), .x))) %>%
  # NA occurrences for outcomes after maximum Date
  mutate(across(contains('hosp_admission_date'), ~ if_else(.x > maxDate, as.Date(NA), .x)),
         across(contains('hosp_severe_admission_date'), ~ if_else(.x > maxDate, as.Date(NA), .x))) %>% 
  # Selecting minimum date of COVID-19 infection and hospitalization (excluding previously created NAs)
  mutate(hosp_admission_date = exec(pmin, !!!rlang::syms(hosp_admission_vars), na.rm = TRUE),
         hosp_severe_admission_date = exec(pmin, !!!rlang::syms(hosp_severe_admission_vars), na.rm = TRUE),
  ) %>%
  select(-starts_with('hosp_admission_date_'))

dfVac <- dfREMlong %>%
  filter(vac_day == 1) %>%
  setNames(paste0('gv_', names(.)))

dfControl <- dfREMlong %>%
  filter(vac_day == 0) %>%
  setNames(paste0('gc_', names(.)))

dfREM <- dfVac %>%
  left_join(dfControl, by = c('gv_subject_pair' = 'gc_subject_pair'))

# Separating Again the Datatable
# Creating Outcome Variables (Dates and Outcome Variables)
# Outcome named 'covid' = COVID-19 infection
# Outcome named 'hosp' = Hospitalization for COVID-19
# Outcome named 'covid_death' = Death due to COVID-19
# Important: date of control group vaccination is included to censor both patients
dfREM <-dfREM %>%
  # Correcting for patients with COVID-19 after vaccination - affects only hospitalization and death outcomes
  mutate(gc_outcome_vac_date_1 = if_else(
    condition = coalesce(gc_outcome_vac_date_1 >= gc_covid_date, F), 
    true = as.Date(NA), 
    false = gc_outcome_vac_date_1)
  ) %>%
  # Get the minimum date of each possible outcome
  # The minimum date include the outcome proposed, 1st dose vaccination of the control group, and 3rd dose vaccinated of vaccine group
  mutate(
    # Control Group Outcomes
    gc_outcome_covid_date = pmin(gc_outcome_vac_date_1, gc_covid_date, gc_hosp_admission_date, gc_death_date, gv_vac_exposure_date_3, na.rm = T),
    gc_outcome_hosp_date = pmin(gc_outcome_vac_date_1, gc_hosp_admission_date, gc_death_date, gv_vac_exposure_date_3, na.rm = T),
    gc_outcome_hosp_severe_date = pmin(gc_outcome_vac_date_1, gc_hosp_severe_admission_date, gc_death_date, gv_vac_exposure_date_3, na.rm = T),
    gc_outcome_death_date = pmin(gc_outcome_vac_date_1, gc_death_date, gv_vac_exposure_date_3, na.rm = T),
    gc_outcome_hosp_death_date = pmin(gc_outcome_vac_date_1, gc_hosp_admission_date, gc_death_date, gv_vac_exposure_date_3, na.rm = T),
    # Vaccinated Group Outcomes
    gv_outcome_covid_date = pmin(gc_outcome_vac_date_1, gv_covid_date, gv_hosp_admission_date, gv_death_date, gv_vac_exposure_date_3, na.rm = T),
    gv_outcome_hosp_date = pmin(gc_outcome_vac_date_1, gv_hosp_admission_date, gv_death_date, gv_vac_exposure_date_3, na.rm = T),
    gv_outcome_hosp_severe_date = pmin(gc_outcome_vac_date_1, gv_hosp_severe_admission_date, gv_death_date, gv_vac_exposure_date_3, na.rm = T),
    gv_outcome_death_date = pmin(gc_outcome_vac_date_1, gv_death_date, gv_vac_exposure_date_3, na.rm = T),
    gv_outcome_hosp_death_date = pmin(gc_outcome_vac_date_1, gv_hosp_admission_date, gv_death_date, gv_vac_exposure_date_3, na.rm = T)
  ) %>%
  # Compare the chosen outcome with the outcome of interest
  mutate(
    # Control Group Outcomes
    gc_outcome_covid_status = case_when(
      gc_outcome_covid_date == gc_hosp_admission_date ~ 2, # COVID-19 Hospitalization
      gc_outcome_covid_date == gc_covid_date ~ 2, # COVID-19 Infection
      gc_outcome_covid_date == gc_death_date ~ 1, # Death
      gc_outcome_covid_date == gc_outcome_vac_date_1 ~ 0, # 1st dose Vaccinated Control Group
      gc_outcome_covid_date == gv_vac_exposure_date_3 ~ 0, # 3rd dose Vaccinated Group
      is.na(gc_outcome_covid_date) ~ 0 # No Outcome
    ),  
    gc_outcome_hosp_status = case_when(
      gc_outcome_hosp_date == gc_hosp_admission_date ~ 2, # COVID-19 Hospitalization
      gc_outcome_hosp_date == gc_death_date ~ 1, # Death
      gc_outcome_hosp_date == gc_outcome_vac_date_1 ~ 0, # 1st dose Vaccinated Control Group
      gc_outcome_hosp_date == gv_vac_exposure_date_3 ~ 0, # 3rd dose Vaccinated Group
      is.na(gc_outcome_hosp_date) ~ 0 # No Outcome
    ), 
    gc_outcome_hosp_severe_status = case_when(
      gc_outcome_hosp_severe_date == gc_hosp_severe_admission_date ~ 2, # COVID-19 Severe Hospitalization
      gc_outcome_hosp_severe_date == gc_death_date ~ 1, # Death
      gc_outcome_hosp_severe_date == gc_outcome_vac_date_1 ~ 0, # 1st dose Vaccinated Control Group
      gc_outcome_hosp_severe_date == gv_vac_exposure_date_3 ~ 0, # 3rd dose Vaccinated Group
      is.na(gc_outcome_hosp_severe_date) ~ 0 # No Outcome
    ), 
    gc_outcome_death_status = case_when(
      gc_outcome_death_date == gc_covid_death_date ~ 2, # COVID-19 Infection and Death
      gc_outcome_death_date == gc_death_date ~ 1, # Death 
      gc_outcome_death_date == gc_outcome_vac_date_1 ~ 0, # 1st dose Vaccinated Control Group
      gc_outcome_death_date == gv_vac_exposure_date_3 ~ 0, # 3rd dose Vaccinated Group
      is.na(gc_outcome_death_date) ~ 0 # No Outcome
    ),
    gc_outcome_hosp_death_status = case_when(
      gc_outcome_hosp_death_date == gc_hosp_admission_date ~ 2, # COVID-19 Hospitalization
      gc_outcome_hosp_death_date == gc_covid_death_date ~ 2, # COVID-19 Death
      gc_outcome_hosp_death_date == gc_death_date ~ 1, # Death
      gc_outcome_hosp_death_date == gc_outcome_vac_date_1 ~ 0, # 1st dose Vaccinated Control Group
      gc_outcome_hosp_death_date == gv_vac_exposure_date_3 ~ 0, # 3rd dose Vaccinated Group
      is.na(gc_outcome_hosp_death_date) ~ 0 # No Outcome
    ),
    # Vaccinated Group Outcomes
    gv_outcome_covid_status = case_when(
      gv_outcome_covid_date == gv_hosp_admission_date ~ 2, # COVID-19 Hospitalization
      gv_outcome_covid_date == gv_covid_date ~ 2, # COVID-19 Infection
      gv_outcome_covid_date == gv_death_date ~ 1, # Death
      gv_outcome_covid_date == gc_outcome_vac_date_1 ~ 0, # 1st dose Vaccinated Control Group
      gv_outcome_covid_date == gv_vac_exposure_date_3 ~ 0, # 3rd dose Vaccinated Group
      is.na(gv_outcome_covid_date) ~ 0 # No Outcome
    ),  
    gv_outcome_hosp_status = case_when(
      gv_outcome_hosp_date == gv_hosp_admission_date ~ 2, # COVID-19 Hospitalization
      gv_outcome_hosp_date == gv_death_date ~ 1, # Death
      gv_outcome_hosp_date == gc_outcome_vac_date_1 ~ 0, # 1st dose Vaccinated Control Group
      gv_outcome_hosp_date == gv_vac_exposure_date_3 ~ 0, # 3rd dose Vaccinated Group
      is.na(gv_outcome_hosp_date) ~ 0 # No Outcome
    ), 
    gv_outcome_hosp_severe_status = case_when(
      gv_outcome_hosp_severe_date == gv_hosp_severe_admission_date ~ 2, # Severe COVID-19 Hospitalization
      gv_outcome_hosp_severe_date == gv_death_date ~ 1, # Death
      gv_outcome_hosp_severe_date == gc_outcome_vac_date_1 ~ 0, # 1st dose Vaccinated Control Group
      gv_outcome_hosp_severe_date == gv_vac_exposure_date_3 ~ 0, # 3rd dose Vaccinated Group
      is.na(gv_outcome_hosp_severe_date) ~ 0 # No Outcome
    ),
    gv_outcome_death_status = case_when(
      gv_outcome_death_date == gv_covid_death_date ~ 2, # COVID-19 Infection and Death
      gv_outcome_death_date == gv_death_date ~ 1, # Death 
      gv_outcome_death_date == gc_outcome_vac_date_1 ~ 0, # 1st dose Vaccinated Control Group
      gv_outcome_death_date == gv_vac_exposure_date_3 ~ 0, # 3rd dose Vaccinated Group
      is.na(gv_outcome_death_date) ~ 0 # No Outcome
    ),
    gv_outcome_hosp_death_status = case_when(
      gv_outcome_hosp_death_date == gv_hosp_admission_date ~ 2, # COVID-19 Hospitalization
      gv_outcome_hosp_death_date == gv_covid_death_date ~ 2, # COVID-19 Death
      gv_outcome_hosp_death_date == gv_death_date ~ 1, # Death
      gv_outcome_hosp_death_date == gc_outcome_vac_date_1 ~ 0, # Vaccinated
      gv_outcome_hosp_death_date == gv_vac_exposure_date_3 ~ 0, # 3rd dose Vaccinated Group
      is.na(gv_outcome_hosp_death_date) ~ 0 # No Outcome
    )) %>%
  # If Someone did not have the outcome until the MaxDate, will be censored at maxDate or end of database followup
  # For Censoring Purposes: 
  # In the Hospitalization Outcome, If the Person had COVID-19, outcome is COVID-19 + 21
  # In Death Outcome, If the Person had Hosp. COVID-19, outcome is Hospital Admission Date + 28 days - On death only outcomes
  mutate(
    gc_outcome_hosp_date = case_when(
      is.na(gc_outcome_hosp_date) & !is.na(gc_outcome_covid_date) ~ gc_outcome_covid_date + 21,
      TRUE ~ gc_outcome_hosp_date),
    gc_outcome_hosp_severe_date = case_when(
      is.na(gc_outcome_hosp_severe_date) & !is.na(gc_outcome_covid_date) ~ gc_outcome_covid_date + 21,
      TRUE ~ gc_outcome_hosp_severe_date),
    gc_outcome_death_date = case_when(
      is.na(gc_outcome_death_date) & !is.na(gc_outcome_hosp_date) ~ gc_outcome_covid_date + 28, 
      is.na(gc_outcome_death_date) &  is.na(gc_outcome_hosp_date) & !is.na(gc_outcome_covid_date) ~ gc_outcome_covid_date + 28, 
      TRUE ~ gc_outcome_death_date),
    gc_outcome_hosp_death_date = case_when(
      is.na(gc_outcome_hosp_death_date) & !is.na(gc_outcome_covid_date) ~ gc_outcome_covid_date + 28, 
      TRUE ~ gc_outcome_hosp_death_date),
    gv_outcome_hosp_date = case_when(
      is.na(gv_outcome_hosp_date) & !is.na(gv_outcome_covid_date) ~ gv_outcome_covid_date + 21, 
      TRUE ~ gv_outcome_hosp_date),
    gv_outcome_hosp_severe_date = case_when(
      is.na(gv_outcome_hosp_severe_date) & !is.na(gv_outcome_covid_date) ~ gv_outcome_covid_date + 21, 
      TRUE ~ gv_outcome_hosp_severe_date),
    gv_outcome_death_date = case_when(
      is.na(gv_outcome_death_date) & !is.na(gv_outcome_hosp_date) ~ gv_outcome_covid_date + 28, 
      is.na(gv_outcome_death_date) &  is.na(gv_outcome_hosp_date) & !is.na(gv_outcome_covid_date) ~ gv_outcome_covid_date + 28, 
      TRUE ~ gv_outcome_death_date),
    gv_outcome_hosp_death_date = case_when(
      is.na(gv_outcome_hosp_death_date) & !is.na(gv_outcome_covid_date) ~ gv_outcome_covid_date + 28, 
      TRUE ~ gv_outcome_hosp_death_date)
  ) %>%
  mutate(
    across(matches('gc_outcome.*date$'), ~ if_else(is.na(.x), pmin(gc_maxDate, gc_end_db_followup), .x)),
    across(matches('gv_outcome.*date$'), ~ if_else(is.na(.x), pmin(gv_maxDate, gv_end_db_followup), .x)),
    across(matches('gc_outcome.*date$'), ~ pmin(.x, gc_maxDate)),
    across(matches('gv_outcome.*date$'), ~ pmin(.x, gv_maxDate))
  )

# Creating time as days for the outcome dates described before - time from eligibility date (minimum date)
dfREM <- dfREM %>%
  mutate(
    # Control Group
    gc_outcome_covid_time = as.numeric(difftime(gc_outcome_covid_date, gv_vac_exposure_date_1,units = 'days')),
    gc_outcome_hosp_time = as.numeric(difftime(gc_outcome_hosp_date, gv_vac_exposure_date_1, units = 'days')),
    gc_outcome_hosp_severe_time = as.numeric(difftime(gc_outcome_hosp_severe_date, gv_vac_exposure_date_1, units = 'days')),
    gc_outcome_death_time = as.numeric(difftime(gc_outcome_death_date, gv_vac_exposure_date_1, units = 'days')),
    gc_outcome_hosp_death_time = as.numeric(difftime(gc_outcome_hosp_death_date, gv_vac_exposure_date_1, units = 'days')),
    # Vaccinated Group
    gv_outcome_covid_time = as.numeric(difftime(gv_outcome_covid_date, gv_vac_exposure_date_1, units = 'days')),
    gv_outcome_hosp_time = as.numeric(difftime(gv_outcome_hosp_date, gv_vac_exposure_date_1, units = 'days')),
    gv_outcome_hosp_severe_time = as.numeric(difftime(gv_outcome_hosp_severe_date, gv_vac_exposure_date_1, units = 'days')),
    gv_outcome_death_time = as.numeric(difftime(gv_outcome_death_date, gv_vac_exposure_date_1, units = 'days')),
    gv_outcome_hosp_death_time = as.numeric(difftime(gv_outcome_hosp_death_date, gv_vac_exposure_date_1, units = 'days'))
  ) %>%
  mutate(
    gv_vac_exposure_time_1 = as.numeric(difftime(gv_vac_exposure_date_1, gv_vac_exposure_date_1, units = 'days')),
    gv_vac_exposure_time_2 = as.numeric(difftime(gv_vac_exposure_date_2, gv_vac_exposure_date_1, units = 'days')),
    gv_vac_exposure_time_3 = as.numeric(difftime(gv_vac_exposure_date_3, gv_vac_exposure_date_1, units = 'days'))
  ) %>%
  mutate(across(matches('.*outcome.*time'), ~ if_else(.x == 0, 0.5, .x)))

# Creating a long dataset for further analysis
dfREMVac <- dfREM %>%
  dplyr::select(starts_with('gv')) %>%
  setNames(gsub('gv_', '', names(.)))

dfREMControl <- dfREM %>%
  dplyr::select(starts_with('gc')) %>%
  setNames(gsub('gc_', '', names(.))) %>%
  setNames(gsub('gv_', '', names(.)))

dfREMlong <- bind_rows(dfREMVac, dfREMControl)
rm(dfREMVac)
rm(dfREMControl)
# Creating Unique Identifiers for Cox Analysis (as matches can be duplicated eventually)
dfREMlong <- dfREMlong %>%
  mutate(new_id = 1:nrow(.))

# Descriptive Analysis - Matched Cohort
# Merging with Characteristics previously deleted before looping 
cols_to_merge <- colnames(cancerREM_dose12)[!colnames(cancerREM_dose12) %in% colnames(dfREMlong)]
dfREMlong <- dfREMlong %>%
  left_join(cancerREM_dose12 %>%
              dplyr::select(subject_id, all_of(cols_to_merge)),
            by = c('subject_id' = 'subject_id')) %>%
  # Creating Variables for Sub-Group Analysis
      mutate(age_bin_60 = if_else(age >= 60, 'Age 60+', 'Age < 60'),
             age_bin_65 = if_else(age >= 65, 'Age 65+', 'Age < 65'),
             age_bin_70 = if_else(age >= 70, 'Age 70+', 'Age < 70'),
             age_bin_75 = if_else(age >= 75, 'Age 75+', 'Age < 75'),
             age_bin_80 = if_else(age >= 80, 'Age 80+', 'Age < 80'),
             age_bin_85 = if_else(age >= 85, 'Age 85+', 'Age < 85'),
         cancer_diagnosis_time_bin_0 = if_else(cancer_diagnosis_time == 0, '< 1y', '1-5yr'),
         cancer_diagnosis_time_bin_1 = if_else(cancer_diagnosis_time %in% c(0, 1), '< 2y', '2-5yr'),
         cancer_diagnosis_time_bin_2 = if_else(cancer_diagnosis_time %in% c(0, 1, 2), '< 3y', '3-5yr'),
         cancer_diagnosis_time_bin_3 = if_else(cancer_diagnosis_time %in% c(0, 1, 2, 3), '< 4y', '4-5yr'), 
         vac_heterologous = case_when(
           vac_concept_id_1 %in% c('Pfizer-mRNA-BNT162b', 'Moderna-mRNA-1273') &
             vac_concept_id_2 %in% c('Pfizer-mRNA-BNT162b', 'Moderna-mRNA-1273') ~ 0,
           vac_concept_id_1 %in% c('AZ-ChAdOx1') &
             vac_concept_id_2 %in% c('AZ-ChAdOx1') ~ 0,
           TRUE ~ 1),
         vac_scheme = paste(vac_concept_id_1, vac_concept_id_2, sep = '-'))


# Creating Descriptive Analysis
# 1. Descriptive Analysis
if(DO_DESCRIPTIVE){
      # Creating Descriptive Table of the Matched VS Un-matched Patients
      temp.table <- CreateTableOne(data = dfREMlong,
                                   vars = c(vars_demographics,
                                            vars_vaccine_type,
                                            vars_cancer_time,
                                            vars_cancer_dx,
                                            vars_cancer_group,
                                            'charlson_index',
                                            vars_comorbidities,
                                            vars_health_visits,
                                            'visits_outpatient_cat'),
                                   factorVars = c(vars_cancer_time,
                                                  vars_cancer_dx,
                                                  vars_cancer_group,
                                                  vars_covid_tests,
                                                  vars_comorbidities),
                                   strata = 'tx_group',
                                   includeNA = TRUE)
      
      print.temp <- print(temp.table,  showAllLevels = T, nonnormal = c('charlson_index', vars_covid_tests))
      write.table(print.temp, file = here('Results', dose_analysis, current_analysis, 'desc_tx_group_matched_rem_12_doses_all_levels.csv'), sep = ';')
      
      print.temp <- print(temp.table, showAllLevels = F, nonnormal = c('charlson_index', vars_covid_tests))
      write.table(print.temp, file = here('Results', dose_analysis, current_analysis, 'desc_tx_group_matched_rem_12_doses_clean.csv'), sep = ';')
      
      print.temp <- print(temp.table, showAllLevels = F, nonnormal = c('charlson_index', vars_covid_tests), smd = T)
      write.table(print.temp, file = here('Results', dose_analysis, current_analysis, 'desc_tx_group_matched_rem_12_doses_smd.csv'), sep = ';')
      
      # Creating Graph of SMD Between Groups
      g.temp <- data.frame(var_names = rownames(ExtractSmd(temp.table)), smd = as.vector(ExtractSmd(temp.table))) 
      write.table(g.temp, file = here('Results', dose_analysis, current_analysis, 'desc_tx_group_eligible_matched_rem_12_doses_smd.csv'), sep = ';')
      
      ggsave(here('Results', dose_analysis, current_analysis, 'graph_tx_group_matched_rem_12_doses_smd.pdf'),
             make.smd.plot(g.temp, 'SMD Treatment Matched Groups'),
             dpi=600, height = 400*0.8, width=300*0.8, units = 'mm')
      
      # Creating Table of Outcomes Percentage
      # Creating Descriptive Table of the Matched VS Un-matched Patients
      temp.table <- CreateTableOne(data = dfREMlong,
                                   vars = c(vars_outcomes_status,
                                            vars_outcomes_time,
                                            'tx_group'),
                                   factorVars = c(vars_outcomes_status),
                                   strata = 'tx_group',
                                   includeNA = TRUE)
      
      print.temp <- print(temp.table,  showAllLevels = T, nonnormal = vars_outcomes_time)
      write.table(print.temp, file = here('Results', dose_analysis, current_analysis, 'desc_outcomes_tx_group_matched_rem_12_doses_all_levels.csv'), sep = ';')
      
      print.temp <- print(temp.table, showAllLevels = F, nonnormal = vars_outcomes_time)
      write.table(print.temp, file = here('Results', dose_analysis, current_analysis, 'desc_outcomes_tx_group_matched_rem_12_doses_clean.csv'), sep = ';')
      
      # Doing Graphs of Censoring Time to Check
      ggplot(aes(x=outcome_covid_time, color=as.factor(tx_group)), data=dfREMlong) + geom_line(stat = 'bin') +
            ggtitle('Follow-up Time COVID-19 Outcome') + theme_minimal() + theme(legend.position = 'top')
      
      ggsave(here('Results', dose_analysis, current_analysis, 'graph_time_outcome_covid_matched_rem_12_doses.pdf'),
             dpi=600, height = 400*0.5, width=300*0.5, units = 'mm')
      
      ggplot(aes(x=outcome_hosp_death_time, color=as.factor(tx_group)), data=dfREMlong) + geom_line(stat = 'bin') +
            ggtitle('Follow-up Time Hosp-Death Outcome') + theme_minimal() + theme(legend.position = 'top')
      
      ggsave(here('Results', dose_analysis, current_analysis, 'graph_time_outcome_hosp_death_matched_rem_12_doses.pdf'),
             dpi=600, height = 400*0.5, width=300*0.5, units = 'mm')
}
rm(cancerElegible)

# Creating Outcomes Analysis Pipeline
# 2.1 Outcome = COVID-19 Infection
if(DO_INFECTION){
      # Main Analysis
      fit <- survfit2(Surv(outcome_covid_time, outcome_covid_status == 2) ~ tx_group, 
                      data = dfREMlong)
      
      saveRDS(fit, here('Results', dose_analysis, current_analysis, 'survfit2_outcome_covid.RDS'))
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 180),
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_covid_rem_12_.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 180), conf.int = T,
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_covid_rem_12_confint.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      dfREM_covid <- tmerge_all_periods(dfREMlong, 'outcome_covid_time', 'outcome_covid_status')
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period, 
            data = dfREM_covid) %>% broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>% 
            broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
            write.table(here('Results', dose_analysis, current_analysis, 'outcome_covid_period_all.csv'), sep = ';', row.names = F)
      
      
      dfREM_covid <- tmerge_three_periods(dfREMlong, 'outcome_covid_time', 'outcome_covid_status')
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period, 
            data = dfREM_covid) %>% broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>%
            broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
            write.table(here('Results', dose_analysis, current_analysis, 'outcome_covid_period_three.csv'), sep = ';', row.names = F)
      
      if(DO_SUBGROUP_ANALYSIS){
            # Sub-group Analysis 
            temp.results <- lapply(vars_subgroup_analysis, tidyInteractionCox, df = dfREM_covid, outcome = 'outcome_covid')
            subgroup.temp.results <- do.call(bind_rows, temp.results)
            subgroup.temp.results <- apply(subgroup.temp.results, 2, as.character)
            
            write.table(subgroup.temp.results,
                        here('Results', dose_analysis, current_analysis, 'subgroup_outcome_covid_three_periods.csv'), sep = ';', row.names = F)
      }      
}
rm(dfREM_covid)

# 2.2 Outcome = COVID-19 Hospitalization
if(DO_HOSP){
      # Main Analysis
      fit <- survfit2(Surv(outcome_hosp_time, outcome_hosp_status == 2) ~ tx_group, 
                      data = dfREMlong)
      
      saveRDS(fit, here('Results', dose_analysis, current_analysis, 'survfit2_outcome_hosp.RDS'))
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 180),
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_hosp_rem_12_.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 180), conf.int = T,
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_hosp_rem_12_confint.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 30),
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_hosp_rem_12_subset_0_30.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      dfREM_hosp <- tmerge_all_periods(dfREMlong, 'outcome_hosp_time', 'outcome_hosp_status')
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period, 
            data = dfREM_hosp) %>% broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>%
            broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
            write.table(here('Results', dose_analysis, current_analysis, 'outcome_hosp_period_all.csv'), sep = ';', row.names = F)
      
      dfREM_hosp <- tmerge_three_periods(dfREMlong, 'outcome_hosp_time', 'outcome_hosp_status')
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period, 
            data = dfREM_hosp) %>% broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>% 
            broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
            write.table(here('Results', dose_analysis, current_analysis, 'outcome_hosp_period_three.csv'), sep = ';', row.names = F)
      
      if(DO_SUBGROUP_ANALYSIS){
            # Subgroup Analysis
            temp.results <- lapply(vars_subgroup_analysis, tidyInteractionCox, df = dfREM_hosp, outcome = 'outcome_hosp')
            subgroup.temp.results <- do.call(bind_rows, temp.results)
            subgroup.temp.results <- apply(subgroup.temp.results, 2, as.character)
            
            write.table(subgroup.temp.results,
                        here('Results', dose_analysis, current_analysis, 'subgroup_outcome_hosp_three_periods.csv'), sep = ';', row.names = F)
      }}

# 2.3 Outcome = COVID-19 Severe Hospitalization
if(DO_SEVERE_HOSP){
      # Main Analysis
      fit <- survfit2(Surv(outcome_hosp_severe_time, outcome_hosp_severe_status == 2) ~ tx_group, 
                      data = dfREMlong)
      
      saveRDS(fit, here('Results', dose_analysis, current_analysis, 'survfit2_outcome_hosp_severe.RDS'))
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 180),
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_hosp_severe_rem_12_.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 180), conf.int = T,
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_hosp_severe_rem_12_confint.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      
      dfREM_hosp_severe <- tmerge_all_periods(dfREMlong, 'outcome_hosp_severe_time', 'outcome_hosp_severe_status')
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period, data = dfREM_hosp_severe) %>% 
            broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>% 
            broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
            write.table(here('Results', dose_analysis, current_analysis, 'outcome_hosp_severe_period_all.csv'), sep = ';', row.names = F)
      
      dfREM_hosp_severe <- tmerge_three_periods(dfREMlong, 'outcome_hosp_severe_time', 'outcome_hosp_severe_status')
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period, 
            data = dfREM_hosp_severe) %>% broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>% 
            broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
            write.table(here('Results', dose_analysis, current_analysis, 'outcome_hosp_severe_period_three.csv'), sep = ';', row.names = F)
      
}
rm(dfREM_hosp_severe)

# 2.4 Outcome = COVID-19 Death
if(DO_DEATH){
      # Main Analysis
      fit <- survfit2(Surv(outcome_death_time, outcome_death_status == 2) ~ tx_group, 
                      data = dfREMlong)
      
      saveRDS(fit, here('Results', dose_analysis, current_analysis, 'survfit2_outcome_death.RDS'))
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 180),
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_death_rem_12_.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 180), conf.int = T,
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_death_rem_12_confint.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      dfREM_death <- tmerge_all_periods(dfREMlong, 'outcome_death_time', 'outcome_death_status')
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period, data = dfREM_death) %>% 
            broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>% 
            broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
            write.table(here('Results', dose_analysis, current_analysis, 'outcome_death_period_all.csv'), sep = ';', row.names = F)
      
      dfREM_death <- tmerge_three_periods(dfREMlong, 'outcome_death_time', 'outcome_death_status')
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period, data = dfREM_death) %>% 
            broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>% 
            broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
            write.table(here('Results', dose_analysis, current_analysis, 'outcome_death_period_three.csv'), sep = ';', row.names = F)
      
      if(DO_SUBGROUP_ANALYSIS){
            # Subgroup Analysis
            temp.results <- lapply(vars_subgroup_analysis, tidyInteractionCox, df = dfREM_death, outcome = 'outcome_death')
            subgroup.temp.results <- do.call(bind_rows, temp.results)
            subgroup.temp.results <- apply(subgroup.temp.results, 2, as.character)
            
            write.table(subgroup.temp.results,
                        here('Results', dose_analysis, current_analysis, 'subgroup_outcome_death_three_periods.csv'), sep = ';', row.names = F)
            
      }}

# 2.5 Outcome = COVID-19 Hospitalization OR Death
if(DO_HOSP_DEATH){
      # Main Analysis
      fit <- survfit2(Surv(outcome_hosp_death_time, outcome_hosp_death_status == 2) ~ tx_group, 
                      data = dfREMlong)
      
      saveRDS(fit, here('Results', dose_analysis, current_analysis, 'survfit2_outcome_hosp_death.RDS'))
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 180),
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_hosp_death_rem_12_.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 180), conf.int = T,
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_hosp_death_rem_12_confint.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 30),
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_hosp_death_rem_12_subset_0_30.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 14),
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_hosp_death_rem_12_subset_0_14.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      dfREM_hosp_death <- tmerge_all_periods(dfREMlong, 'outcome_hosp_death_time', 'outcome_hosp_death_status')
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period, data = dfREM_hosp_death) %>% 
            broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>%
            broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
            write.table(here('Results', dose_analysis, current_analysis, 'outcome_hosp_death_period_all.csv'), sep = ';', row.names = F)
      
      dfREM_hosp_death <- tmerge_three_periods(dfREMlong, 'outcome_hosp_death_time', 'outcome_hosp_death_status')
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period, 
            data = dfREM_hosp_death) %>% broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>% 
            broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
            write.table(here('Results', dose_analysis, current_analysis, 'outcome_hosp_death_period_three.csv'), sep = ';', row.names = F)
      
      if(DO_SUBGROUP_ANALYSIS){
            # Subgroup Analysis
            temp.results <- lapply(vars_subgroup_analysis, tidyInteractionCox, df = dfREM_hosp_death, outcome = 'outcome_hosp_death')
            subgroup.temp.results <- do.call(bind_rows, temp.results)
            subgroup.temp.results <- apply(subgroup.temp.results, 2, as.character)
            
            write.table(subgroup.temp.results,
                        here('Results', dose_analysis, current_analysis, 'subgroup_outcome_hosp_death_three_periods.csv'), sep = ';', row.names = F)
      }
}

# Creating Additional Analysis
# 3.1 Outcome = Non-COVID-19 Death Analysis
if(DO_NON_COVID_DEATH){
      # Main Analysis
      fit <- survfit2(Surv(outcome_death_time, outcome_death_status == 1) ~ tx_group, 
                      data = dfREMlong)
      
      saveRDS(fit, here('Results', dose_analysis, current_analysis, 'survfit2_outcome_noncovid_death.RDS'))
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 180),
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_noncovid_death_rem_12_.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      dfREM_death <- tmerge_all_periods(dfREMlong, 'outcome_death_time', 'outcome_death_status')
      
      coxph(Surv(tstart, tstop, outcome == 1) ~ period, data = dfREM_death) %>% 
            broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>% 
            broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
            write.table(here('Results', dose_analysis, current_analysis, 'outcome_noncovid_death_period_all.csv'), sep = ';', row.names = F)
      
      dfREM_death <- tmerge_three_periods(dfREMlong, 'outcome_death_time', 'outcome_death_status')
      
      coxph(Surv(tstart, tstop, outcome == 1) ~ period, data = dfREM_death) %>% 
            broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>% 
            broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
            write.table(here('Results', dose_analysis, current_analysis, 'outcome_noncovid_death_period_three.csv'), sep = ';', row.names = F)
      
      if(DO_SUBGROUP_ANALYSIS){      
            # Subgroup Analysis
            temp.results <- lapply(vars_subgroup_analysis, tidyInteractionCox, df = dfREM_death, outcome = 'outcome_death', outcome_number = 1)
            subgroup.temp.results <- do.call(bind_rows, temp.results)
            subgroup.temp.results <- apply(subgroup.temp.results, 2, as.character)
            
            write.table(subgroup.temp.results,
                        here('Results', dose_analysis, current_analysis, 'subgroup_outcome_noncovid_death_three_periods.csv'), sep = ';', row.names = F)
      }}

# 3.2 Competing Risk Analysis
if(DO_COMPETING_RISK){
      # Cumulative Incidence - Pseudohazards (Cuminc)
      # Outcome COVID Hospitalization or COVID Death vs Non-COVID Death
      dfREM_hosp_death_cuminc <- dfREM_hosp_death %>% mutate(outcome_hosp_death_status = factor(outcome_hosp_death_status, levels = 0:2,
                                                                                                labels = c('censor', 'noncovid_death', 'covid_hosp_death')))
      
      crr(Surv(outcome_hosp_death_time, outcome_hosp_death_status) ~ period, data = dfREM_hosp_death_cuminc, id = new_id, failcode = 'noncovid_death') %>%
            broom::tidy() %>% 
            write.table(here('Results', dose_analysis, current_analysis, 'cuminc_outcome_hosp_death_three_periods_failcode_noncovid_death.csv'), sep = ';', row.names = F)
      
      crr(Surv(outcome_hosp_death_time, outcome_hosp_death_status) ~ period, data = dfREM_hosp_death_cuminc, id = new_id, failcode = 'covid_hosp_death') %>%
            broom::tidy() %>% 
            write.table(here('Results', dose_analysis, current_analysis, 'cuminc_outcome_hosp_death_three_periods_failcode_covid_hosp_death.csv'), sep = ';', row.names = F)
      
      
      # Outcome COVID Hospitalization vs Non-COVID Death
      dfREM_hosp_cuminc <- dfREM_hosp %>% mutate(outcome_hosp_status = factor(outcome_hosp_status, levels = 0:2,
                                                                              labels = c('censor', 'noncovid_death', 'covid_hosp')))
      
      crr(Surv(outcome_hosp_time, outcome_hosp_status) ~ period, data = dfREM_hosp_cuminc, id = new_id, failcode = 'noncovid_death') %>%
            broom::tidy() %>% 
            write.table(here('Results', dose_analysis, current_analysis, 'cuminc_outcome_hosp_three_periods_failcode_noncovid_death.csv'), sep = ';', row.names = F)
      
      crr(Surv(outcome_hosp_time, outcome_hosp_status) ~ period, data = dfREM_hosp_cuminc, id = new_id, failcode = 'covid_hosp') %>%
            broom::tidy() %>% 
            write.table(here('Results', dose_analysis, current_analysis, 'cuminc_outcome_hosp_three_periods_failcode_covid_hosp.csv'), sep = ';', row.names = F)
      
      
      # Outcome COVID Death vs Non-COVID Death
      dfREM_death_cuminc <- dfREM_death %>% mutate(outcome_death_status = factor(outcome_death_status, levels = 0:2,
                                                                                 labels = c('censor', 'noncovid_death', 'covid_death')))
      
      cuminc_fit <- cuminc(Surv(outcome_death_time, outcome_death_status) ~ tx_group, 
                           data = dfREM_death_cuminc, id = new_id)
      
      saveRDS(cuminc_fit, here('Results', dose_analysis, current_analysis, 'cuminc_outcome_death.RDS'))
      
      crr(Surv(outcome_death_time, outcome_death_status) ~ period, data = dfREM_death_cuminc, id = new_id, failcode = 'noncovid_death') %>%
            broom::tidy() %>% 
            write.table(here('Results', dose_analysis, current_analysis, 'cuminc_outcome_death_three_periods_failcode_noncovid_death.csv'), sep = ';', row.names = F)
      
      crr(Surv(outcome_death_time, outcome_death_status) ~ period, data = dfREM_death_cuminc, id = new_id, failcode = 'covid_death') %>%
            broom::tidy() %>% 
            write.table(here('Results', dose_analysis, current_analysis, 'cuminc_outcome_death_three_periods_failcode_covid_death.csv'), sep = ';', row.names = F)
      
}
rm(dfREM_death)
rm(dfREM_hosp)
rm(dfREM_hosp_death)
