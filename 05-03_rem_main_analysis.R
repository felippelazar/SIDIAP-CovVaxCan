# ============================================================================ #
# 5. REM Analysis - COVID-19 Vaccine 3rd dose  #
# Author: Felippe Lazar, IDIAP Jordi Gol, 2023 #
# ============================================================================ #

library(tidyverse)
library(readxl)
library(here)
library(tidylog)
library(survival)
library(survival)
library(survminer)
source('utils.R')
library(broom.helpers)
library(emmeans)
library(tidycmprsk)
library(ggsurvfit)
library(tableone)

# Creating Folder for Exporting Files if Does Not Exist Yet
ifelse(!dir.exists(here('Results')), dir.create(here('Results')), FALSE)
ifelse(!dir.exists(here('Results', 'dose_3')), dir.create(here('Results', 'dose_3')), FALSE)
ifelse(!dir.exists(here('Results', 'dose_3', 'rem_main_analysis')), dir.create(here('Results', 'dose_3', 'rem_main_analysis')), FALSE)

# Loading Auxiliary Objects for Analayis
current_analysis <- 'rem_main_analysis'
source('aux_objects_rem_3.R')

# Creating Boolean Handlers for Analysis (goal: save time when re-running processes)
DO_DESCRIPTIVE <- TRUE
DO_INFECTION <- TRUE
DO_HOSP <- TRUE
DO_ANY_HOSP <- TRUE
DO_SEVERE_HOSP <- TRUE
DO_DEATH <- TRUE
DO_HOSP_DEATH <- TRUE
DO_NON_COVID_DEATH <- TRUE
DO_SUBGROUP_ANALYSIS <- TRUE
DO_COMPETING_RISK <- TRUE

#-- Merge batched data into the one dataframe
dfREM <- do.call(bind_rows, z_merge_3rd)

# Creating Outcome Variables (Dates and Outcome Variables)
# Outcome named 'covid' = COVID-19 infection
# Outcome named 'hosp' = Hospitalization for COVID-19
# Outcome named 'covid_death' = Death due to COVID-19
# Important: date of control group vaccination is included to censor both patients
dfREM <- dfREM %>%
  # Correcting for patients with COVID-19 after vaccination - affects only hospitalization and death outcomes
  mutate(gc_outcome_vac_date_3 = if_else(
    coalesce(gc_outcome_vac_date_3 >= gc_covid_date, F), 
    as.Date(NA), 
    gc_outcome_vac_date_3)
  ) %>%
  # Get the minimum date of each possible outcome
  mutate(
    # Control Group
    gc_outcome_covid_date = pmin(gc_outcome_vac_date_3, gc_covid_date, gc_hosp_admission_date, gc_death_date, gv_vac_exposure_date_4, na.rm = T),
    gc_outcome_hosp_date = pmin(gc_outcome_vac_date_3, gc_hosp_admission_date, gc_death_date, gv_vac_exposure_date_4, na.rm = T),
    gc_outcome_any_hosp_date = pmin(gc_outcome_vac_date_3, gc_any_hosp_admission_date, gc_death_date, gv_vac_exposure_date_4, na.rm = T),
    gc_outcome_hosp_severe_date = pmin(gc_outcome_vac_date_3, gc_hosp_severe_admission_date, gc_death_date, gv_vac_exposure_date_4, na.rm = T),
    gc_outcome_death_date = pmin(gc_outcome_vac_date_3, gc_death_date, gv_vac_exposure_date_4, na.rm = T),
    gc_outcome_hosp_death_date = pmin(gc_outcome_vac_date_3, gc_hosp_admission_date, gc_death_date, gv_vac_exposure_date_4, na.rm = T),
    # Vaccinated Group
    gv_outcome_covid_date = pmin(gc_outcome_vac_date_3, gv_covid_date, gv_hosp_admission_date, gv_death_date, gv_vac_exposure_date_4, na.rm = T),
    gv_outcome_hosp_date = pmin(gc_outcome_vac_date_3, gv_hosp_admission_date, gv_death_date, gv_vac_exposure_date_4, na.rm = T),
    gv_outcome_any_hosp_date = pmin(gc_outcome_vac_date_3, gv_any_hosp_admission_date, gv_death_date, gv_vac_exposure_date_4, na.rm = T),
    gv_outcome_hosp_severe_date = pmin(gc_outcome_vac_date_3, gv_hosp_severe_admission_date, gv_death_date, gv_vac_exposure_date_4, na.rm = T),
    gv_outcome_death_date = pmin(gc_outcome_vac_date_3, gv_death_date, gv_vac_exposure_date_4, na.rm = T),
    gv_outcome_hosp_death_date = pmin(gc_outcome_vac_date_3, gv_hosp_admission_date, gv_death_date, gv_vac_exposure_date_4, na.rm = T)
  ) %>%
  # Compare the chosen outcome with the outcome of interest
  mutate(
    # Control Group Outcomes
    gc_outcome_covid_status = case_when(
      gc_outcome_covid_date == gc_hosp_admission_date ~ 2, # COVID-19 Hospitalization
      gc_outcome_covid_date == gc_covid_date ~ 2, # COVID-19 Infection
      gc_outcome_covid_date == gc_death_date ~ 1, # Death
      gc_outcome_covid_date == gc_outcome_vac_date_3 ~ 0, # 3rd dose Control Group
      gc_outcome_covid_date == gv_vac_exposure_date_4 ~ 0, # 4th dose Vaccinated Group
      is.na(gc_outcome_covid_date) ~ 0 # No Outcome
    ),  
    gc_outcome_hosp_status = case_when(
      gc_outcome_hosp_date == gc_hosp_admission_date ~ 2, # COVID-19 Hospitalization
      gc_outcome_hosp_date == gc_death_date ~ 1, # Death
      gc_outcome_hosp_date == gc_outcome_vac_date_3 ~ 0, # 3rd dose Control Group
      gc_outcome_hosp_date == gv_vac_exposure_date_4 ~ 0, # 4th dose Vaccinated Group
      is.na(gc_outcome_hosp_date) ~ 0 # No Outcome
    ), 
    gc_outcome_any_hosp_status = case_when(
      gc_outcome_any_hosp_date == gc_any_hosp_admission_date ~ 2, # Any Hospitalization
      gc_outcome_any_hosp_date == gc_death_date ~ 1, # Death
      gc_outcome_any_hosp_date == gc_outcome_vac_date_3 ~ 0, # 3rd dose Control Group
      gc_outcome_any_hosp_date == gv_vac_exposure_date_4 ~ 0, # 4th dose Vaccinated Group
      is.na(gc_outcome_any_hosp_date) ~ 0 # No Outcome
    ), 
    gc_outcome_hosp_severe_status = case_when(
      gc_outcome_hosp_severe_date == gc_hosp_severe_admission_date ~ 2, # COVID-19 Severe Hospitalization
      gc_outcome_hosp_severe_date == gc_death_date ~ 1, # Death
      gc_outcome_hosp_severe_date == gc_outcome_vac_date_3 ~ 0, # 3rd dose Control Group
      gc_outcome_hosp_severe_date == gv_vac_exposure_date_4 ~ 0, # 4th dose Vaccinated Group
      is.na(gc_outcome_hosp_severe_date) ~ 0 # No Outcome
    ), 
    gc_outcome_death_status = case_when(
      gc_outcome_death_date == gc_covid_death_date ~ 2, # COVID-19 Infection and Death
      gc_outcome_death_date == gc_death_date ~ 1, # Death 
      gc_outcome_death_date == gc_outcome_vac_date_3 ~ 0, # 3rd dose Control Group
      gc_outcome_death_date == gv_vac_exposure_date_4 ~ 0, # 4th dose Vaccinated Group
      is.na(gc_outcome_death_date) ~ 0 # No Outcome
    ),
    gc_outcome_hosp_death_status = case_when(
      gc_outcome_hosp_death_date == gc_hosp_admission_date ~ 2, # COVID-19 Hospitalization
      gc_outcome_hosp_death_date == gc_covid_death_date ~ 2, # COVID-19 Death
      gc_outcome_hosp_death_date == gc_death_date ~ 1, # Death
      gc_outcome_hosp_death_date == gc_outcome_vac_date_3 ~ 0, # 3rd dose Control Group
      gc_outcome_hosp_death_date == gv_vac_exposure_date_4 ~ 0, # 4th dose Vaccinated Group
      is.na(gc_outcome_hosp_death_date) ~ 0 # No Outcome
    ),
    # Vaccinated Group Outcomes
    gv_outcome_covid_status = case_when(
      gv_outcome_covid_date == gv_hosp_admission_date ~ 2, # COVID-19 Hospitalization
      gv_outcome_covid_date == gv_covid_date ~ 2, # COVID-19 Infection
      gv_outcome_covid_date == gv_death_date ~ 1, # Death
      gv_outcome_covid_date == gc_outcome_vac_date_3 ~ 0, # 3rd dose Control Group
      gv_outcome_covid_date == gv_vac_exposure_date_4 ~ 0, # 4th dose Vaccinated Group
      is.na(gv_outcome_covid_date) ~ 0 # No Outcome
    ),  
    gv_outcome_hosp_status = case_when(
      gv_outcome_hosp_date == gv_hosp_admission_date ~ 2, # COVID-19 Hospitalization
      gv_outcome_hosp_date == gv_death_date ~ 1, # Death
      gv_outcome_hosp_date == gc_outcome_vac_date_3 ~ 0, # 3rd dose Control Group
      gv_outcome_hosp_date == gv_vac_exposure_date_4 ~ 0, # 4th dose Vaccinated Group
      is.na(gv_outcome_hosp_date) ~ 0 # No Outcome
    ), 
    gv_outcome_any_hosp_status = case_when(
      gv_outcome_any_hosp_date == gv_any_hosp_admission_date ~ 2, # Any Hospitalization
      gv_outcome_any_hosp_date == gv_death_date ~ 1, # Death
      gv_outcome_any_hosp_date == gc_outcome_vac_date_3 ~ 0, # 3rd dose Control Group
      gv_outcome_any_hosp_date == gv_vac_exposure_date_4 ~ 0, # 4th dose Vaccinated Group
      is.na(gv_outcome_any_hosp_date) ~ 0 # No Outcome
    ), 
    gv_outcome_hosp_severe_status = case_when(
      gv_outcome_hosp_severe_date == gv_hosp_severe_admission_date ~ 2, # Severe COVID-19 Hospitalization
      gv_outcome_hosp_severe_date == gv_death_date ~ 1, # Death
      gv_outcome_hosp_severe_date == gc_outcome_vac_date_3 ~ 0, # 3rd dose Control Group
      gv_outcome_hosp_severe_date == gv_vac_exposure_date_4 ~ 0, # 4th dose Vaccinated Group
      is.na(gv_outcome_hosp_severe_date) ~ 0 # No Outcome
    ),
    gv_outcome_death_status = case_when(
      gv_outcome_death_date == gv_covid_death_date ~ 2, # COVID-19 Infection and Death
      gv_outcome_death_date == gv_death_date ~ 1, # Death 
      gv_outcome_death_date == gc_outcome_vac_date_3 ~ 0, # 3rd dose Control Group
      gv_outcome_death_date == gv_vac_exposure_date_4 ~ 0, # 4th dose Vaccinated Group
      is.na(gv_outcome_death_date) ~ 0 # No Outcome
    ),
    gv_outcome_hosp_death_status = case_when(
      gv_outcome_hosp_death_date == gv_hosp_admission_date ~ 2, # COVID-19 Hospitalization
      gv_outcome_hosp_death_date == gv_covid_death_date ~ 2, # COVID-19 Death
      gv_outcome_hosp_death_date == gv_death_date ~ 1, # Death
      gv_outcome_hosp_death_date == gc_outcome_vac_date_3 ~ 0, # 3rd dose Control Group
      gv_outcome_hosp_death_date == gv_vac_exposure_date_4 ~ 0, # 4th dose Vaccinated Group
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
    gc_outcome_covid_time = as.numeric(difftime(gc_outcome_covid_date, gv_vac_exposure_date_3,units = 'days')),
    gc_outcome_hosp_time = as.numeric(difftime(gc_outcome_hosp_date, gv_vac_exposure_date_3, units = 'days')),
    gc_outcome_any_hosp_time = as.numeric(difftime(gc_outcome_any_hosp_date, gv_vac_exposure_date_3, units = 'days')),
    gc_outcome_hosp_severe_time = as.numeric(difftime(gc_outcome_hosp_severe_date, gv_vac_exposure_date_3, units = 'days')),
    gc_outcome_death_time = as.numeric(difftime(gc_outcome_death_date, gv_vac_exposure_date_3, units = 'days')),
    gc_outcome_hosp_death_time = as.numeric(difftime(gc_outcome_hosp_death_date, gv_vac_exposure_date_3, units = 'days')),
    # Vaccinated Group
    gv_outcome_covid_time = as.numeric(difftime(gv_outcome_covid_date, gv_vac_exposure_date_3, units = 'days')),
    gv_outcome_hosp_time = as.numeric(difftime(gv_outcome_hosp_date, gv_vac_exposure_date_3, units = 'days')),
    gv_outcome_any_hosp_time = as.numeric(difftime(gv_outcome_any_hosp_date, gv_vac_exposure_date_3, units = 'days')),
    gv_outcome_hosp_severe_time = as.numeric(difftime(gv_outcome_hosp_severe_date, gv_vac_exposure_date_3, units = 'days')),
    gv_outcome_death_time = as.numeric(difftime(gv_outcome_death_date, gv_vac_exposure_date_3, units = 'days')),
    gv_outcome_hosp_death_time = as.numeric(difftime(gv_outcome_hosp_death_date, gv_vac_exposure_date_3, units = 'days'))
  ) %>%
  mutate(
    gv_vac_exposure_time_3 = as.numeric(difftime(gv_vac_exposure_date_3, gv_vac_exposure_date_3, units = 'days'))
  ) %>%
  mutate(across(matches('.*outcome.*time'), ~ if_else(.x == 0, 0.5, .x)))

# Creating a long dataset for further analysis
dfREMVac <- dfREM %>%
  mutate(gv_subject_id_pair = paste(gv_subject_id, gc_subject_id, sep = '-')) %>%
  dplyr::select(starts_with('gv')) %>%
  mutate(tx_group = 1) %>%
  setNames(gsub('gv_', '', names(.)))

dfREMControl <- dfREM %>%
  mutate(gc_subject_id_pair = paste(gv_subject_id, gc_subject_id, sep = '-')) %>%
  dplyr::select(starts_with('gc'), 
                gv_gender_concept_id, 
                gv_aga_code, 
                gv_cancer_diagnosis_time, 
                gv_vac_scheme
  ) %>%
  mutate(tx_group = 0) %>%
  setNames(gsub('gc_', '', names(.))) %>%
  setNames(gsub('gv_', '', names(.)))

dfREMlong <- bind_rows(dfREMVac, dfREMControl)

# Creating Unique Identifiers for Cox Analysis (as matches can be duplicated eventually)
dfREMlong <- dfREMlong %>%
  mutate(new_id = 1:nrow(.))

# Descriptive Analysis Unmatched and Matched Cohort
matchedVacIDS <- dfREMlong %>% filter(vac_day == 1) %>% pull(subject_id) %>% unique()
matchedControlIDS <- dfREMlong %>% filter(vac_day == 0) %>% pull(subject_id) %>% unique()
cancerElegible <- do.call(bind_rows, eligibles_3rd) %>% distinct(subject_id, vac_day, .keep_all=T)

# Eligible Patients Descriptive Analysis
cancerElegible <- cancerElegible %>%
  left_join(cancerREM_dose3, by = c('subject_id' = 'subject_id')) %>%
  group_by(subject_id) %>%
  mutate(vac_day = max(vac_day)) %>%
  ungroup() %>%
  mutate(matched_vac = if_else(subject_id %in% matchedVacIDS, 1, 0)) %>%
  mutate(matched_control = if_else(subject_id %in% matchedControlIDS, 1, 0)) %>%
  mutate(matched_any = if_else(matched_vac == 1 | matched_vac == 1, 1, 0)) %>%
  distinct(subject_id, .keep_all = T)

# Descriptive Analysis - Matched Cohort
cols_to_merge <- colnames(cancerREM_dose3)[!colnames(cancerREM_dose3) %in% colnames(dfREMlong)]
dfREMlong <- dfREMlong %>%
      left_join(cancerREM_dose3 %>%
                      select(subject_id, all_of(cols_to_merge)),
                by = c('subject_id' = 'subject_id')) %>%
      # Creating Variables for Sub-Group Analysis
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
             vac_mRNA_12 = if_else(vac_concept_id_1 != 'AZ-ChAdOx1', 1, 0),
             vac_diff_vac = if_else(vac_concept_id_2 != vac_concept_id_3, 1, 0),
             vac_heterologous = case_when(
                   vac_concept_id_2 %in% c('Pfizer-mRNA-BNT162b', 'Moderna-mRNA-1273') &
                         vac_concept_id_3 %in% c('Pfizer-mRNA-BNT162b', 'Moderna-mRNA-1273') ~ 0,
                   vac_concept_id_2 %in% c('AZ-ChAdOx1') &
                         vac_concept_id_3 %in% c('AZ-ChAdOx1') ~ 0,
                   TRUE ~ 1
             ))

# Creating Descriptive Analysis
# 1. Descriptive Analysis
if(DO_DESCRIPTIVE){
      # Creating Descriptive Table of the Final Eligible Patients (Only One Group)
      temp.table <- CreateTableOne(data = cancerElegible,
                                   vars = c(vars_matching,
                                            vars_demographics,
                                            'vac_day',
                                            vars_cancer_time,
                                            vars_cancer_dx,
                                            vars_cancer_group,
                                            'charlson_index',
                                            vars_comorbidities,
                                            vars_health_visits,
                                            'visits_outpatient_cat'),
                                   factorVars = c('vac_day',
                                                  vars_matching,
                                                  vars_cancer_time,
                                                  vars_cancer_dx,
                                                  vars_cancer_group,
                                                  vars_covid_tests,
                                                  vars_comorbidities),
                                   includeNA = TRUE)
      
      print.temp <- print(temp.table,  showAllLevels = T, nonnormal = c('charlson_index', vars_covid_tests))
      write.table(print.temp, file = here('Results', dose_analysis, current_analysis, 'desc_eligible_rem_3_doses_all_levels.csv'), sep = ';')
      
      print.temp <- print(temp.table, showAllLevels = F, nonnormal = c('charlson_index', vars_covid_tests))
      write.table(print.temp, file = here('Results', dose_analysis, current_analysis, 'desc_eligible_rem_3_doses_clean.csv'), sep = ';')
      
      # Creating Descriptive Table of the Matched VS Un-matched Patients
      temp.table <- CreateTableOne(data = cancerElegible,
                                   vars = c('matched_any',
                                            vars_demographics,
                                            'vac_day',
                                            vars_cancer_time,
                                            vars_cancer_dx,
                                            vars_cancer_group,
                                            'charlson_index',
                                            vars_comorbidities,
                                            vars_health_visits,
                                            'visits_outpatient_cat'),
                                   factorVars = c('vac_day',
                                                  vars_cancer_time,
                                                  vars_cancer_dx,
                                                  vars_cancer_group,
                                                  vars_covid_tests,
                                                  vars_comorbidities),
                                   strata = 'matched_any',
                                   includeNA = TRUE)
      
      print.temp <- print(temp.table,  showAllLevels = T, nonnormal = c('charlson_index', vars_covid_tests))
      write.table(print.temp, file = here('Results', dose_analysis, current_analysis, 'desc_eligible_matched_rem_3_doses_all_levels.csv'), sep = ';')
      
      print.temp <- print(temp.table, showAllLevels = F, nonnormal = c('charlson_index', vars_covid_tests))
      write.table(print.temp, file = here('Results', dose_analysis, current_analysis, 'desc_eligible_matched_rem_3_doses_clean.csv'), sep = ';')
      
      print.temp <- print(temp.table, showAllLevels = F, nonnormal = c('charlson_index', vars_covid_tests), smd = T)
      write.table(print.temp, file = here('Results', dose_analysis, current_analysis, 'desc_eligible_matched_rem_3_doses_smd.csv'), sep = ';')
      
      # Creating Graph of SMD Between Groups
      g.temp <- data.frame(var_names = rownames(ExtractSmd(temp.table)), smd = as.vector(ExtractSmd(temp.table))) 
      write.table(g.temp, file = here('Results', dose_analysis, current_analysis, 'graph_desc_eligible_matched_rem_3_doses_smd.csv'), sep = ';')
      
      make.smd.plot <- function(table, title = ''){
        ggplot(aes(x = fct_reorder(var_names, smd), y = smd), data = g.temp) + 
          geom_point(aes(color = as.factor(ifelse(smd > 0.1, 1, 0)))) + 
          scale_y_log10(limits = c(0.0001, 10)) + theme_minimal() + coord_flip() +
          geom_hline(yintercept = 0.1, linetype = 2) + 
          geom_hline(yintercept = 0.05, linetype = 2) +
          scale_color_manual(values = c('lightgray', 'black')) +
          labs(x = 'variables', y = 'SMD \n (Standardized Mean Differences)', title = title) + 
          theme(legend.position = 'none',
                plot.title = element_text(hjust = 0.5))
      }
      
      ggsave(here('Results', dose_analysis, current_analysis, 'graph_desc_eligible_matched_rem_3_doses_smd.pdf'),
             make.smd.plot(g.temp, 'SMD Matched vs Unmatched Patients'),
             dpi=600, height = 400*0.8, width=300*0.8, units = 'mm')
      
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
      write.table(print.temp, file = here('Results', dose_analysis, current_analysis, 'desc_tx_group_matched_rem_3_doses_all_levels.csv'), sep = ';')
      
      print.temp <- print(temp.table, showAllLevels = F, nonnormal = c('charlson_index', vars_covid_tests))
      write.table(print.temp, file = here('Results', dose_analysis, current_analysis, 'desc_tx_group_matched_rem_3_doses_clean.csv'), sep = ';')
      
      print.temp <- print(temp.table, showAllLevels = F, nonnormal = c('charlson_index', vars_covid_tests), smd = T)
      write.table(print.temp, file = here('Results', dose_analysis, current_analysis, 'desc_tx_group_matched_rem_3_doses_smd.csv'), sep = ';')
      
      # Creating Graph of SMD Between Groups
      g.temp <- data.frame(var_names = rownames(ExtractSmd(temp.table)), smd = as.vector(ExtractSmd(temp.table))) 
      write.table(g.temp, file = here('Results', dose_analysis, current_analysis, 'desc_tx_group_eligible_matched_rem_3_doses_smd.csv'), sep = ';')
      
      ggsave(here('Results', dose_analysis, current_analysis, 'graph_tx_group_matched_rem_3_doses_smd.pdf'),
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
      write.table(print.temp, file = here('Results', dose_analysis, current_analysis, 'desc_outcomes_tx_group_matched_rem_3_doses_all_levels.csv'), sep = ';')
      
      print.temp <- print(temp.table, showAllLevels = F, nonnormal = vars_outcomes_time)
      write.table(print.temp, file = here('Results', dose_analysis, current_analysis, 'desc_outcomes_tx_group_matched_rem_3_doses_clean.csv'), sep = ';')
      
      # Doing Graphs of Censoring Time to Check
      ggplot(aes(x=outcome_covid_time, color=as.factor(tx_group)), data=dfREMlong) + geom_line(stat = 'bin') +
            ggtitle('Follow-up Time COVID-19 Outcome') + theme_minimal() + theme(legend.position = 'top')
      
      ggsave(here('Results', dose_analysis, current_analysis, 'graph_time_outcome_covid_matched_rem_3_doses.pdf'),
             dpi=600, height = 400*0.5, width=300*0.5, units = 'mm')
      
      ggplot(aes(x=outcome_hosp_death_time, color=as.factor(tx_group)), data=dfREMlong) + geom_line(stat = 'bin') +
            ggtitle('Follow-up Time Hosp-Death Outcome') + theme_minimal() + theme(legend.position = 'top')
      
      ggsave(here('Results', dose_analysis, current_analysis, 'graph_time_outcome_hosp_death_matched_rem_3_doses.pdf'),
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
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_covid_rem_3_.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 180), conf.int = T,
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_covid_rem_3_confint.pdf'))
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
        broom.helpers::tidy_add_reference_rows() %>%  broom.helpers::tidy_add_n() %>%
        write.table(here('Results', dose_analysis, current_analysis, 'outcome_covid_period_three.csv'), sep = ';', row.names = F)
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period + strata(subject_id_pair), 
            data = dfREM_covid) %>% 
            broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>%   
            #broom.helpers::tidy_add_reference_rows() %>%  broom.helpers::tidy_add_n() %>%
        write.table(here('Results', dose_analysis, current_analysis, 'outcome_covid_period_three_stratified.csv'), sep = ';', row.names = F)
      
      if(DO_SUBGROUP_ANALYSIS){
            # Sub-group Analysis 
            temp.results <- lapply(vars_subgroup_analysis, tidyInteractionCox, df = dfREM_covid, outcome = 'outcome_covid')
            subgroup.temp.results <- do.call(bind_rows, temp.results)
            subgroup.temp.results <- apply(subgroup.temp.results, 2, as.character)
            
            write.table(subgroup.temp.results,
                        here('Results', dose_analysis, current_analysis, 'subgroup_outcome_covid_three_periods.csv'), sep = ';', row.names = F)}
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
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_hosp_rem_3_.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 180), conf.int = T,
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_hosp_rem_3_confint.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 30),
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_hosp_rem_3_subset_0_30.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      dfREM_hosp <- tmerge_all_periods(dfREMlong, 'outcome_hosp_time', 'outcome_hosp_status')
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period, 
            data = dfREM_hosp) %>% broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>%   
        broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
        write.table(here('Results', dose_analysis, current_analysis, 'outcome_hosp_period_all.csv'), sep = ';', row.names = F)
      
      dfREM_hosp <- tmerge_three_periods(dfREMlong, 'outcome_hosp_time', 'outcome_hosp_status')
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period, 
            data = dfREM_hosp) %>%   broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>%   
        broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
        write.table(here('Results', dose_analysis, current_analysis, 'outcome_hosp_period_three.csv'), sep = ';', row.names = F)
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period + strata(subject_id_pair), 
            data = dfREM_hosp) %>%   
            broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>%   
            #broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
        write.table(here('Results', dose_analysis, current_analysis, 'outcome_hosp_period_three_stratified.csv'), sep = ';', row.names = F)
      
      if(DO_SUBGROUP_ANALYSIS){
            # Subgroup Analysis
            temp.results <- lapply(vars_subgroup_analysis, tidyInteractionCox, df = dfREM_hosp, outcome = 'outcome_hosp')
            subgroup.temp.results <- do.call(bind_rows, temp.results)
            subgroup.temp.results <- apply(subgroup.temp.results, 2, as.character)
            
            write.table(subgroup.temp.results,
                        here('Results', dose_analysis, current_analysis, 'subgroup_outcome_hosp_three_periods.csv'), sep = ';', row.names = F)
      }}

# 2.2 Outcome = COVID-19 Hospitalization
if(DO_ANY_HOSP){
  # Main Analysis
  fit <- survfit2(Surv(outcome_any_hosp_time, outcome_any_hosp_status == 2) ~ tx_group, 
                  data = dfREMlong)
  
  saveRDS(fit, here('Results', dose_analysis, current_analysis, 'survfit2_outcome_any_hosp.RDS'))
  
  temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 180),
                            legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                            palette = c("#E7B800","#2E9FDF"), risk.table = T)
  
  pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_any_hosp_rem_3_.pdf'))
  print(temp.cumhaz, newpage = FALSE)
  dev.off()
  
  temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 180), conf.int = T,
                            legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                            palette = c("#E7B800","#2E9FDF"), risk.table = T)
  
  pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_any_hosp_rem_3_confint.pdf'))
  print(temp.cumhaz, newpage = FALSE)
  dev.off()
  
  temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 30),
                            legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                            palette = c("#E7B800","#2E9FDF"), risk.table = T)
  
  pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_any_hosp_rem_3_subset_0_30.pdf'))
  print(temp.cumhaz, newpage = FALSE)
  dev.off()
  
  dfREM_any_hosp <- tmerge_all_periods(dfREMlong, 'outcome_any_hosp_time', 'outcome_any_hosp_status')
  
  coxph(Surv(tstart, tstop, outcome == 2) ~ period, 
        data = dfREM_any_hosp) %>% broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>%   
    broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
    write.table(here('Results', dose_analysis, current_analysis, 'outcome_any_hosp_period_all.csv'), sep = ';', row.names = F)
  
  dfREM_any_hosp <- tmerge_three_periods(dfREMlong, 'outcome_any_hosp_time', 'outcome_any_hosp_status')
  
  coxph(Surv(tstart, tstop, outcome == 2) ~ period, 
        data = dfREM_any_hosp) %>%   broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>%   
    broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
    write.table(here('Results', dose_analysis, current_analysis, 'outcome_any_hosp_period_three.csv'), sep = ';', row.names = F)
  
  coxph(Surv(tstart, tstop, outcome == 2) ~ period + strata(subject_id_pair), 
        data = dfREM_any_hosp) %>% 
        broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>%   
        #broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
    write.table(here('Results', dose_analysis, current_analysis, 'outcome_any_hosp_period_three_stratified.csv'), sep = ';', row.names = F)
  
  if(DO_SUBGROUP_ANALYSIS){
    # Subgroup Analysis
    temp.results <- lapply(vars_subgroup_analysis, tidyInteractionCox, df = dfREM_any_hosp, outcome = 'outcome_any_hosp')
    subgroup.temp.results <- do.call(bind_rows, temp.results)
    subgroup.temp.results <- apply(subgroup.temp.results, 2, as.character)
    
    write.table(subgroup.temp.results,
                here('Results', dose_analysis, current_analysis, 'subgroup_outcome_any_hosp_three_periods.csv'), sep = ';', row.names = F)
  }}
rm(dfREM_any_hosp)

# 2.3 Outcome = COVID-19 Severe Hospitalization
if(DO_SEVERE_HOSP){
      # Main Analysis
      fit <- survfit2(Surv(outcome_hosp_severe_time, outcome_hosp_severe_status == 2) ~ tx_group, 
                     data = dfREMlong)
      
      saveRDS(fit, here('Results', dose_analysis, current_analysis, 'survfit2_outcome_hosp_severe.RDS'))
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 180),
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_hosp_severe_rem_3_.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 180), conf.int = T,
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_hosp_severe_rem_3_confint.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      dfREM_hosp_severe <- tmerge_all_periods(dfREMlong, 'outcome_hosp_severe_time', 'outcome_hosp_severe_status')
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period, data = dfREM_hosp_severe) %>% 
        broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>%  
        broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
        write.table(here('Results', dose_analysis, current_analysis, 'outcome_hosp_severe_period_all.csv'), sep = ';', row.names = F)
      
      dfREM_hosp_severe <- tmerge_three_periods(dfREMlong, 'outcome_hosp_severe_time', 'outcome_hosp_severe_status')
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period, 
            data = dfREM_hosp_severe) %>% 
        broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>%   broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
        write.table(here('Results', dose_analysis, current_analysis, 'outcome_hosp_severe_period_three.csv'), sep = ';', row.names = F)
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period + strata(subject_id_pair), 
            data = dfREM_hosp_severe) %>% 
            broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>%   
            #broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
        write.table(here('Results', dose_analysis, current_analysis, 'outcome_hosp_severe_period_three_stratified.csv'), sep = ';', row.names = F)
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
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_death_rem_3_.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 180), conf.int = T,
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_death_rem_3_confint.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      dfREM_death <- tmerge_all_periods(dfREMlong, 'outcome_death_time', 'outcome_death_status')
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period, data = dfREM_death) %>% 
        broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>%   broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
        write.table(here('Results', dose_analysis, current_analysis, 'outcome_death_period_all.csv'), sep = ';', row.names = F)
      
      dfREM_death <- tmerge_three_periods(dfREMlong, 'outcome_death_time', 'outcome_death_status')
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period, data = dfREM_death) %>% 
        broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>%   broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
        write.table(here('Results', dose_analysis, current_analysis, 'outcome_death_period_three.csv'), sep = ';', row.names = F)
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period + strata(subject_id_pair), data = dfREM_death) %>% 
            broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>%   
            #broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
        write.table(here('Results', dose_analysis, current_analysis, 'outcome_death_period_three_stratified.csv'), sep = ';', row.names = F)
      
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
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_hosp_death_rem_3_.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 180), conf.int = T,
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_hosp_death_rem_3_.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 30),
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_hosp_death_rem_3_subset_0_30.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 14),
                                legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                                palette = c("#E7B800","#2E9FDF"), risk.table = T)
      
      pdf(here('Results', dose_analysis, current_analysis, 'graph_curve_hosp_death_rem_3_subset_0_14.pdf'))
      print(temp.cumhaz, newpage = FALSE)
      dev.off()
      
      dfREM_hosp_death <- tmerge_all_periods(dfREMlong, 'outcome_hosp_death_time', 'outcome_hosp_death_status')
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period, data = dfREM_hosp_death) %>% 
        broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>%   broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
        write.table(here('Results', dose_analysis, current_analysis, 'outcome_hosp_death_period_all.csv'), sep = ';', row.names = F)
      
      dfREM_hosp_death <- tmerge_three_periods(dfREMlong, 'outcome_hosp_death_time', 'outcome_hosp_death_status')
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period, 
            data = dfREM_hosp_death) %>% 
            broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>%   
            broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
        write.table(here('Results', dose_analysis, current_analysis, 'outcome_hosp_death_period_three.csv'), sep = ';', row.names = F)
      
      coxph(Surv(tstart, tstop, outcome == 2) ~ period + strata(subject_id_pair), 
            data = dfREM_hosp_death) %>% 
            broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>%   
            #broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
        write.table(here('Results', dose_analysis, current_analysis, 'outcome_hosp_death_period_three_stratified.csv'), sep = ';', row.names = F)
      
      
      if(DO_SUBGROUP_ANALYSIS){# Subgroup Analysis
            temp.results <- lapply(vars_subgroup_analysis, tidyInteractionCox, df = dfREM_hosp_death, outcome = 'outcome_hosp_death')
            subgroup.temp.results <- do.call(bind_rows, temp.results)
            subgroup.temp.results <- apply(subgroup.temp.results, 2, as.character)
            
            write.table(subgroup.temp.results,
                        here('Results', dose_analysis, current_analysis, 'subgroup_outcome_hosp_death_three_periods.csv'), sep = ';', row.names = F)
}}

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
      
      coxph(Surv(tstart, tstop, outcome == 1) ~ period + strata(subject_id_pair), data = dfREM_death) %>% 
        broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>% 
        #broom.helpers::tidy_add_reference_rows() %>% broom.helpers::tidy_add_n() %>%
        write.table(here('Results', dose_analysis, current_analysis, 'outcome_noncovid_death_period_three_stratified.csv'), sep = ';', row.names = F)
      
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