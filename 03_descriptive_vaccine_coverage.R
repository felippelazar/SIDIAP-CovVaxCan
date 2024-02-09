# ============================================================================ #
# 3. Descriptive Analysis - Vaccine Coverage  #
# Author: Felippe Lazar, IDIAP Jordi Gol, 2023 #
# ============================================================================ #

# All datasets used in this analysis were created during STEP 1 ('Importing Cohorts.R') or STEP 2 ('Preparing Dataset.R')
library(tidyverse)
library(readxl)
library(here)
library(tidylog)
library(survminer)
library(tableone)
source('utils.R')

# Creating Folder for Exporting Files if Does Not Exist Yet
ifelse(!dir.exists(here('Results')), dir.create(here('Results')), FALSE)
ifelse(!dir.exists(here('Results', 'descriptive')), dir.create(here('Results', 'descriptive')), FALSE)

#' # Merging Cohorts 
# Merging All Variables and Datasets Created During Previous Steps
# Merging Cancer Patients with COVID-19, Hospitalization, Vaccine, Comorbidities, Influenza and all created variables

#' # Filtering by Inclusion and Exclusion Criteria
##  Filtering by Inclusion and Exclusion Criteria
cancerDesc <- cancerMerged %>%
  # Filtering by Age (>= 18 years)
  filter(as.numeric(difftime(as.Date.character('27/12/2020', format = '%d/%m/%Y'), dob, units = 'days')/365) >= 18) %>%
  # Filtering by First Date of Diagnosis no More than 5 years from 27th December 2020 - Beginning of Vaccination in Catalunia
  filter(cancer_diag_first_date > as.Date.character('27/12/2015', format = '%d/%m/%Y')) %>%
  # Filtering by First Date of Diagnosis no More than 5 years from 27th December 2020 - Beginning of Vaccination in Catalunia
  filter(cancer_diag_first_date <= as.Date.character('27/12/2020', format = '%d/%m/%Y')) %>%
  # Filtering by Patient Alive on beginning of the Vaccination Campaign - 27th December 2020
  filter(coalesce(death_date >= as.Date.character('27/12/2020', format = '%d/%m/%Y'), T)) %>%
  # Filtering by Patient Active on beginning of the Vaccination Campaign - 27th December 2020
  filter(coalesce(end_db_followup >= as.Date.character('27/12/2020', format = '%d/%m/%Y'), T)) %>%
  # Filtering by Previous COVID-19 Infection to Beginning of Vaccination Campaign - 27th December 2020
  filter(coalesce(covid_date_1 >= as.Date.character('27/12/2020', format = '%d/%m/%Y'), T)) %>%
  # Filtering by Previous Being Hospitalized at the Beggining of Vaccination - 27th December 2020
  filter(coalesce(hosp_admission_date_1 >= as.Date.character('27/12/2020', format = '%d/%m/%Y'), T)) %>%
  # Filtering by Care Home Living
  filter(is.na(living_care_home))

# Descriptive Analysis up to 27th December 2020
# Demographic Information and Comorbidities
cancerDesc <- cancerDesc %>%
  mutate(gender_concept_id = factor(gender_concept_id, levels = c('8507', '8532'), labels = c('M', 'F'))) %>% 
  mutate(age = as.numeric(difftime(as.Date.character('27/12/2020', format = '%d/%m/%Y'), dob, units = 'days')/365)) %>%
  mutate(cancer_diagnosis_time = as.numeric(difftime(as.Date.character('27/12/2020', format = '%d/%m/%Y'), cancer_diag_first_date, units = 'days')/365),
         cancer_diagnosis_time = factor(floor(cancer_diagnosis_time))) %>%
  mutate(age_group = case_when(
    age >= 18 & age < 50 ~ '18-49',
    age >= 50 & age < 60 ~ '50-59',
    age >= 60 & age < 70 ~ '60-69',
    age >= 70 & age < 80 ~ '70-79',
    age >= 80 & age < 999 ~ '80-115',
  )) %>%
  mutate(moved_out = if_else(!is.na(death_date) & end_db_followup != as.Date.character('30/06/2022', format = '%d/%m/%Y'), 1, 0)) %>%
  mutate(vac_scheme_12 = paste(vac_concept_id_1, vac_concept_id_2, sep = '-'),
         vac_scheme_123 = paste(vac_concept_id_1, vac_concept_id_2, vac_concept_id_3, sep = '-'),
         vac_any = if_else(!is.na(vac_concept_id_1), 1, 0)) %>%
  mutate(visits_outpatient_cat = fct_case_when(
    n_visits_outpatient == 0 ~ '0',
    n_visits_outpatient == 1 ~ '1',
    n_visits_outpatient == 2 ~ '2',
    n_visits_outpatient >= 3 ~ '3+'
  ))

# Descriptive Table
vars_demographics <- c('age', 'age_group', 'gender_concept_id', 'medea_group_2001', 'aga_name')

vars_vaccine_type <- c('vac_concept_id_1', 'vac_concept_id_2', 'vac_concept_id_3', 'vac_concept_id_4', 'vac_concept_id_5', 
                       'vac_scheme_12', 'vac_scheme_123', 'vac_any')

vars_cancer_time <- c('cancer_diagnosis_time')

vars_comorbidities <- c("CCI_Rheumatologic_Disease", "CCI_Any_Malignancy", "CCI_Metastatic_Solid_Tumor", 
                        "CCI_Diabetes_Mild", "CCI_Mild_Liver_Disease", "CCI_Peptic_Ulcer_Disease", 
                        "CCI_Chronic_Pulmonary_Disease","CCI_Renal_Disease", "CCI_Cerebrovascular_Disease", 
                        "CCI_Diabetes_With_Complications", "CCI_Congestive_Heart_Failure", "CCI_Dementia", 
                        "CCI_Peripheral_Artery_Disease", "CCI_Myocardial_Infarction", "CCI_Moderate_Severe_Liver_Disease", 
                        "CCI_Hemiplegia_Paraplegia", "CCI_AIDS")

vars_cancer_dx <- c('cancer_dx_breast', 'cancer_dx_prostate', 'cancer_dx_colorectal', 'cancer_dx_lung', 
                    'cancer_dx_head_neck', 'cancer_dx_endometrium', 'cancer_dx_cervix_uterus', 'cancer_dx_bladder',
                    'cancer_dx_liver_biliary', 'cancer_dx_melanoma', 'cancer_dx_pancreas', 'cancer_dx_kidney', 
                    'cancer_dx_gastric', 'cancer_dx_esophagus', 'cancer_dx_testis', 'cancer_dx_thyroid',
                    'cancer_dx_CNS', 'cancer_dx_neuroendocrine', 'cancer_dx_sarcomas', 'cancer_dx_leukemia', 
                    'cancer_dx_myeloma', 'cancer_dx_lymphoma', 'cancer_dx_other', 'cancer_dx_other_2',
                    'cancer_dx_undefined')

vars_cancer_group <- c("cancer_group_gastro_intestinal", "cancer_group_genito_urinary", "cancer_group_thyroid",
                       "cancer_group_breast", "cancer_group_sarcomas", "cancer_group_thorax",
                       "cancer_group_gynecology", "cancer_group_head_neck", "cancer_group_hemathological",
                       "cancer_group_skin", "cancer_group_CNS", "cancer_group_undefined", 
                       "cancer_group_nasopharynx", "cancer_group_neuroendocrine", "cancer_group_other", 
                       "cancer_group_other_2")

vars_covid_tests <- c('n_covid_tests_0', 'n_covid_tests_1', 'n_covid_tests')
vars_health_visits <- c('n_visits_outpatient', 'n_visits_telehealth')

# Creating Descriptive Table of all Patients
temp.table <- CreateTableOne(data = cancerDesc,
                             vars = c(vars_demographics,
                                      vars_vaccine_type,
                                      vars_cancer_time,
                                      vars_cancer_dx,
                                      vars_cancer_group,
                                      'charlson_index',
                                      vars_comorbidities,
                                      vars_health_visits,
                                      'visits_outpatient_cat'
                                      ),
                             factorVars = c(vars_vaccine_type,
                                            vars_cancer_time,
                                            vars_cancer_dx,
                                            vars_cancer_group,
                                            vars_comorbidities,
                                            vars_covid_tests,
                                            'visits_outpatient_cat'),
                             includeNA = TRUE)

print.temp <- print(temp.table,  showAllLevels = T, nonnormal = c('charlson_index', vars_covid_tests))
write.table(print.temp, file = here('Results', 'descriptive', 'desc_eligible_all_levels.csv'), sep = ';', row.names = F)

print.temp <- print(temp.table, showAllLevels = F, nonnormal = c('charlson_index', vars_covid_tests))
write.table(print.temp, file = here('Results', 'descriptive', 'desc_eligible_clean.csv'), sep = ';', row.names = F)

# Creating Descriptive Table of Vaccinated and Unvaccinated with Any Dose
temp.table <- CreateTableOne(data = cancerDesc,
                             vars = c(vars_demographics,
                                      vars_cancer_time,
                                      vars_cancer_dx,
                                      vars_cancer_group,
                                      'charlson_index',
                                      vars_comorbidities,
                                      vars_health_visits,
                                      'visits_outpatient_cat'
                             ),
                             factorVars = c(vars_cancer_time,
                                            vars_cancer_dx,
                                            vars_cancer_group,
                                            vars_comorbidities,
                                            vars_covid_tests,
                                            'visits_outpatient_cat'),
                             strata = 'vac_any',
                             includeNA = TRUE)

print.temp <- print(temp.table,  showAllLevels = T, nonnormal = c('charlson_index', vars_covid_tests))
write.table(print.temp, file = here('Results', 'descriptive', 'desc_eligible_vac_unvax_all_levels.csv'), sep = ';', row.names = F)

print.temp <- print(temp.table, showAllLevels = F, nonnormal = c('charlson_index', vars_covid_tests))
write.table(print.temp, file = here('Results', 'descriptive', 'desc_eligible_vac_unvax_clean.csv'), sep = ';', row.names = F)

print.temp <- print(temp.table, showAllLevels = F, nonnormal = c('charlson_index', vars_covid_tests), smd = T)
write.table(print.temp, file = here('Results', 'descriptive', 'desc_eligible_vac_unvax_smd.csv'), sep = ';', row.names = F)
# Creating Graph of SMD Between Groups
g.temp <- data.frame(var_names = rownames(ExtractSmd(temp.table)), smd = as.vector(ExtractSmd(temp.table))) 
write.table(g.temp, file = here('Results', 'descriptive', 'graph_desc_eligible_vac_unvax_smd.csv'), sep = ';', row.names = F)

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

ggsave(here('Results', 'descriptive', 'graph_desc_vac_unvax_smd.pdf'),
       make.smd.plot(g.temp, 'SMD Vaccinated vs Unvaccinated Patients'),
       dpi=600, height = 400*0.8, width=300*0.8, units = 'mm')

# Creating Graphs for Analysis
# Vaccination Rollout by Age
temp.table <- cancerDesc %>%
  pivot_longer(cols = c('vac_concept_id_1', 'vac_concept_id_2', 'vac_concept_id_3'), 
               names_to = c('vac_number'), values_to = c('vac_concept_id')) %>%
  pivot_longer(cols = c('vac_exposure_date_1', 'vac_exposure_date_2', 'vac_exposure_date_3'), 
               names_to = c('vac_date'), values_to = c('vac_exposure_date')) %>%
  filter(str_extract(vac_number, '\\d$') == str_extract(vac_date, '\\d$')) %>%
  group_by(age_group, vac_exposure_date, vac_concept_id) %>%
  count() %>%
  drop_na()

write.table(temp.table, here('Results', 'descriptive','vacc_rollout_by_age.csv'), sep = ';', row.names = F)

temp.graph <- temp.table %>%
  ggplot(aes(x = vac_exposure_date, y = n, fill=vac_concept_id)) + geom_col() +
  facet_wrap(.~age_group, nrow=5) + 
  labs(x = '', y = 'N', fill = '') + theme_bw() + 
  theme(legend.position = 'top')

ggsave(here('Results', 'descriptive', 'graph_vacc_rollout_by_age.pdf'), 
       dpi=600, height = 400*0.8, width=300*0.8, units = 'mm')

# Vaccination Rollout by Type
temp.table <- cancerDesc %>%
  pivot_longer(cols = c('vac_concept_id_1', 'vac_concept_id_2', 'vac_concept_id_3'), 
               names_to = c('vac_number'), values_to = c('vac_concept_id')) %>%
  pivot_longer(cols = c('vac_exposure_date_1', 'vac_exposure_date_2', 'vac_exposure_date_3'), 
               names_to = c('vac_date'), values_to = c('vac_exposure_date')) %>%
  filter(str_extract(vac_number, '\\d$') == str_extract(vac_date, '\\d$')) %>%
  group_by(vac_exposure_date, vac_concept_id) %>%
  count() %>%
  drop_na()

write.table(temp.table, here('Results', 'descriptive','vacc_rollout_by_type.csv'), sep = ';', row.names = F)

temp.graph <- temp.table %>%
  ggplot(aes(x = vac_exposure_date, y = n, fill=vac_concept_id)) + geom_col() +
  facet_wrap(.~vac_concept_id, nrow=5) + 
  labs(x = '', y = 'N', fill = '') + theme_bw() + 
  theme(legend.position = 'none')

ggsave(here('Results', 'descriptive', 'vacc_rollout_by_type.pdf'), 
       dpi=600, height = 400*0.8, width=300*0.8, units = 'mm')

# Vaccination Rollout by Dose
temp.table <- cancerDesc %>%
  pivot_longer(cols = c('vac_concept_id_1', 'vac_concept_id_2', 'vac_concept_id_3'), 
               names_to = c('vac_number'), values_to = c('vac_concept_id')) %>%
  pivot_longer(cols = c('vac_exposure_date_1', 'vac_exposure_date_2', 'vac_exposure_date_3'), 
               names_to = c('vac_date'), values_to = c('vac_exposure_date')) %>%
  filter(str_extract(vac_number, '\\d$') == str_extract(vac_date, '\\d$')) %>%
  mutate(vac_number = factor(vac_number, levels = c('vac_concept_id_1', 'vac_concept_id_2', 'vac_concept_id_3'),
                             labels = c('Dose One', 'Dose Two', 'Booster'))) %>%
  group_by(vac_exposure_date, vac_number, vac_concept_id) %>%
  count() %>%
  drop_na()

write.table(temp.table, here('Results', 'descriptive','vacc_rollout_by_type_and_dose.csv'), sep = ';', row.names = F)

temp.graph <- temp.table %>%
  ggplot(aes(x = vac_exposure_date, y = n, fill=vac_concept_id)) + geom_col() +
  facet_wrap(.~vac_number, nrow=5) + 
  labs(x = '', y = 'N', fill = '') + theme_bw() + 
  theme(legend.position = 'top')

ggsave(here('Results', 'descriptive', 'vacc_rollout_by_type_and_dose.pdf'), 
       dpi=600, height = 400*0.8, width=300*0.8, units = 'mm')

# Vaccination Rollout by Cancer Diagnosis Time
temp.table <- cancerDesc %>%
  pivot_longer(cols = c('vac_concept_id_1', 'vac_concept_id_2', 'vac_concept_id_3'), 
               names_to = c('vac_number'), values_to = c('vac_concept_id')) %>%
  pivot_longer(cols = c('vac_exposure_date_1', 'vac_exposure_date_2', 'vac_exposure_date_3'), 
               names_to = c('vac_date'), values_to = c('vac_exposure_date')) %>%
  filter(str_extract(vac_number, '\\d$') == str_extract(vac_date, '\\d$')) %>%
  mutate(vac_number = factor(vac_number, levels = c('vac_concept_id_1', 'vac_concept_id_2', 'vac_concept_id_3'),
                             labels = c('Dose One', 'Dose Two', 'Booster'))) %>%
  group_by(vac_exposure_date, vac_number, cancer_diagnosis_time) %>%
  count() %>%
  drop_na()

write.table(temp.table, here('Results', 'descriptive','vacc_rollout_by_cancer_diagnosis_time.csv'), sep = ';', row.names = F)

temp.graph <- temp.table %>%
  ggplot(aes(x = vac_exposure_date, y = n, fill=cancer_diagnosis_time)) + geom_col() +
  facet_wrap(.~cancer_diagnosis_time, nrow=7) + 
  labs(x = '', y = 'N', fill = '') + theme_bw() + 
  theme(legend.position = 'none')

ggsave(here('Results', 'descriptive', 'vacc_rollout_by_cancer_diagnosis_time.pdf'), 
       dpi=600, height = 400*0.8, width=300*0.8, units = 'mm')

# Vaccination Rollout by Cancer Diagnosis Time and Type of Vaccine
temp.table <- cancerDesc %>%
  pivot_longer(cols = c('vac_concept_id_1', 'vac_concept_id_2', 'vac_concept_id_3'), 
               names_to = c('vac_number'), values_to = c('vac_concept_id')) %>%
  pivot_longer(cols = c('vac_exposure_date_1', 'vac_exposure_date_2', 'vac_exposure_date_3'), 
               names_to = c('vac_date'), values_to = c('vac_exposure_date')) %>%
  filter(str_extract(vac_number, '\\d$') == str_extract(vac_date, '\\d$')) %>%
  mutate(vac_number = factor(vac_number, levels = c('vac_concept_id_1', 'vac_concept_id_2', 'vac_concept_id_3'),
                             labels = c('Dose One', 'Dose Two', 'Booster'))) %>%
  group_by(vac_exposure_date, vac_number, cancer_diagnosis_time, vac_concept_id) %>%
  count() %>%
  drop_na()

write.table(temp.table, here('Results', 'descriptive','vacc_rollout_by_cancer_diagnosis_time_and_type.csv'), sep = ';', row.names = F)

temp.graph <- temp.table %>%
  ggplot(aes(x = vac_exposure_date, y = n, fill=vac_concept_id)) + geom_col() +
  facet_wrap(.~cancer_diagnosis_time, nrow=7) + 
  labs(x = '', y = 'N', fill = '') + theme_bw() + 
  theme(legend.position = 'top')

ggsave(here('Results', 'descriptive', 'vacc_rollout_by_cancer_diagnosis_time_and_type.pdf'), 
       dpi=600, height = 400*0.8, width=300*0.8, units = 'mm')

# Cumulative Rollout Vaccines
temp.table <- cancerDesc %>%
  pivot_longer(cols = c('vac_concept_id_1', 'vac_concept_id_2', 'vac_concept_id_3'), 
               names_to = c('vac_number'), values_to = c('vac_concept_id')) %>%
  pivot_longer(cols = c('vac_exposure_date_1', 'vac_exposure_date_2', 'vac_exposure_date_3'), 
               names_to = c('vac_date'), values_to = c('vac_exposure_date')) %>%
  filter(str_extract(vac_number, '\\d$') == str_extract(vac_date, '\\d$')) %>%
  mutate(vac_number = factor(vac_number, 
                             levels = c('vac_concept_id_1', 'vac_concept_id_2', 'vac_concept_id_3'),
                             labels = c('Dose One', 'Dose Two', 'Booster'))) %>%
  mutate(n_total = 164467) %>%
  group_by(vac_number, vac_exposure_date) %>%
  arrange(vac_exposure_date) %>%
  summarize(n = n(), n_total = min(n_total)) %>%
  mutate(cum_sum = cumsum(n)/n_total)

write.table(temp.table, here('Results', 'descriptive','vacc_cum_rollout_by_dose.csv'), sep = ';', row.names = F)

temp.graph <- temp.table %>%
  ggplot(aes(x = vac_exposure_date, y = cum_sum, fill = vac_number)) + 
  geom_area(alpha = 1, position = 'identity', color = 'black') + theme_bw() + 
  theme(legend.position = 'top') + ylim(0, 1) + 
  scale_fill_manual(values = c('#e5f5e0', '#a1d99b', '#31a354')) +
  labs(y = '%', x = '', fill = '')

ggsave(here('Results', 'descriptive', 'vacc_cum_rollout_by_dose.pdf'), 
       dpi=600, height = 150*0.8, width=300*0.8, units = 'mm')

# Cumulative Rollout Vaccines and Death and End of Follow-up
temp.table <- cancerDesc %>%
  pivot_longer(cols = c('vac_exposure_date_1', 'vac_exposure_date_2', 'vac_exposure_date_3', 'death_date','end_db_followup'), 
               names_to = c('outcomes'), values_to = c('outcomes_dates')) %>%
  mutate(outomes = factor(outcomes, 
                             levels = c('vac_exposure_date_1', 'vac_exposure_date_2', 'vac_exposure_date_3', 
                                        'death_date', 'end_db_followup'),
                             labels = c('Dose One', 'Dose Two', 'Booster', 'Death', 'Lost-Follow-up'))) %>%
  mutate(n_total = 164467) %>%
  group_by(outcomes, outcomes_dates) %>%
  arrange(outcomes_dates) %>%
  summarize(n = n(), n_total = min(n_total)) %>%
  mutate(cum_sum = cumsum(n)/n_total)

write.table(temp.table, here('Results', 'descriptive','vacc_death_follow_rollout.csv'), sep = ';', row.names = F)

temp.graph <- temp.table %>%
  mutate(outomes = factor(outcomes, 
                          levels = c('death_date', 'end_db_followup', 
                                     'vac_exposure_date_1', 'vac_exposure_date_2', 'vac_exposure_date_3'),
                          labels = c('Death', 'Lost-Follow-up', 'Dose One', 'Dose Two', 'Booster'))) %>%
  ggplot(aes(x = outcomes_dates, y = cum_sum, fill = outcomes)) + 
  geom_area(alpha = 0.5, position = 'identity', color = 'black') + theme_bw() + 
  theme(legend.position = 'top') + ylim(0, 1) + 
  scale_fill_manual(values = c('black', 'lightgray', '#e5f5e0', '#a1d99b', '#31a354')) +
  labs(y = '%', x = '', fill = '')

ggsave(here('Results', 'descriptive', 'vacc_death_follow_rollout.pdf'), 
       dpi=600, height = 150*0.8, width=300*0.8, units = 'mm')

# Cumulative Rollout Vaccines by Age Group
temp.table <- cancerDesc %>%
  pivot_longer(cols = c('vac_concept_id_1', 'vac_concept_id_2', 'vac_concept_id_3'), 
               names_to = c('vac_number'), values_to = c('vac_concept_id')) %>%
  pivot_longer(cols = c('vac_exposure_date_1', 'vac_exposure_date_2', 'vac_exposure_date_3'), 
               names_to = c('vac_date'), values_to = c('vac_exposure_date')) %>%
  filter(str_extract(vac_number, '\\d$') == str_extract(vac_date, '\\d$')) %>%
  mutate(vac_number = factor(vac_number, 
                             levels = c('vac_concept_id_1', 'vac_concept_id_2', 'vac_concept_id_3'),
                             labels = c('Dose One', 'Dose Two', 'Booster'))) %>%
  group_by(age_group) %>%
  mutate(n_total = length(unique(subject_id))) %>% ungroup() %>%
  group_by(age_group, vac_number, vac_exposure_date) %>%
  arrange(vac_exposure_date) %>%
  summarize(n = n(), n_total = min(n_total)) %>%
  mutate(cum_sum = cumsum(n)/n_total)

write.table(temp.table, here('Results', 'descriptive','vacc_cum_rollout_by_age_group.csv'), sep = ';', row.names = F)

temp.graph <- temp.table %>%
  ggplot(aes(x = vac_exposure_date, y = cum_sum, fill = vac_number)) + 
  geom_area(alpha = 1, position = 'identity', color = 'black') + theme_bw() + 
  theme(legend.position = 'top') + ylim(0, 1) + 
  scale_fill_manual(values = c('#e5f5e0', '#a1d99b', '#31a354')) +
  labs(y = '%', x = '', fill = '') + facet_wrap(.~age_group, nrow = 5)

ggsave(here('Results', 'descriptive', 'vacc_cum_rollout_by_age_group.pdf'), 
       dpi=600, height = 400*0.8, width=300*0.8, units = 'mm')


# Cumulative Rollout Vaccines by Cancer Diagnosis Time
temp.table <- cancerDesc %>%
  pivot_longer(cols = c('vac_concept_id_1', 'vac_concept_id_2', 'vac_concept_id_3'), 
               names_to = c('vac_number'), values_to = c('vac_concept_id')) %>%
  pivot_longer(cols = c('vac_exposure_date_1', 'vac_exposure_date_2', 'vac_exposure_date_3'), 
               names_to = c('vac_date'), values_to = c('vac_exposure_date')) %>%
  filter(str_extract(vac_number, '\\d$') == str_extract(vac_date, '\\d$')) %>%
  mutate(vac_number = factor(vac_number, 
                             levels = c('vac_concept_id_1', 'vac_concept_id_2', 'vac_concept_id_3'),
                             labels = c('Dose One', 'Dose Two', 'Booster'))) %>%
  group_by(cancer_diagnosis_time) %>%
  mutate(n_total = length(unique(subject_id))) %>% ungroup() %>%
  group_by(cancer_diagnosis_time, vac_number, vac_exposure_date) %>%
  arrange(vac_exposure_date) %>%
  summarize(n = n(), n_total = min(n_total)) %>%
  mutate(cum_sum = cumsum(n)/n_total)

write.table(temp.table, here('Results', 'descriptive','vacc_cum_rollout_by_cancer_diagnosis.csv'), sep = ';', row.names = F)

temp.graph <- temp.table %>%
  ggplot(aes(x = vac_exposure_date, y = cum_sum, fill = vac_number)) + 
  geom_area(alpha = 1, position = 'identity', color = 'black') + theme_bw() + 
  theme(legend.position = 'top') + ylim(0, 1) + 
  scale_fill_manual(values = c('#e5f5e0', '#a1d99b', '#31a354')) +
  labs(y = '%', x = '', fill = '') + facet_wrap(.~cancer_diagnosis_time, nrow = 7)

ggsave(here('Results', 'descriptive', 'vacc_cum_rollout_by_cancer_diagnosis.pdf'), 
       dpi=600, height = 400*0.8, width=300*0.8, units = 'mm')

# COVID-19 Infection 
covid_date_vars <- colnames(cancerDesc)[grepl('covid_date_', colnames(cancerDesc))]

temp.table <- cancerDesc %>%
  pivot_longer(cols = covid_date_vars, 
               names_to = c('covid_number_infection'), values_to = c('covid_date')) %>%
  group_by(covid_date) %>%
  count() %>%
  drop_na()

write.table(temp.table, here('Results', 'descriptive','covid_frequency.csv'), sep = ';', row.names = F)

temp.graph <- temp.table %>%
  ggplot(aes(x = covid_date, y = n)) + geom_col() +
  labs(x = '', y = 'N', fill = '') + theme_bw() + 
  theme(legend.position = 'none')

ggsave(here('Results', 'descriptive', 'covid_frequency.pdf'), 
       dpi=600, height = 150*0.8, width=300*0.8, units = 'mm')

# Cumulative COVID-19 Infection By Age Group
covid_date_vars <- colnames(cancerDesc)[grepl('covid_date_', colnames(cancerDesc))]

temp.table <- cancerDesc %>%
  pivot_longer(cols = covid_date_vars, 
               names_to = c('covid_number_infection'), values_to = c('covid_date')) %>%
  group_by(age_group) %>%
  mutate(n_total = length(unique(subject_id))) %>% ungroup() %>%
  group_by(age_group, covid_date) %>%
  arrange(covid_date) %>%
  summarize(n = n(), n_total = min(n_total)) %>%
  mutate(cum_sum = cumsum(n)/n_total)

write.table(temp.table, here('Results', 'descriptive','covid_age_group.csv'), sep = ';', row.names = F)

temp.graph <- temp.table %>%
  ggplot(aes(x = covid_date, y = cum_sum)) + 
  geom_area(alpha = 1, position = 'identity', color = 'black') + theme_bw() + 
  theme(legend.position = 'top') + ylim(0, 1) + 
  scale_fill_manual(values = c('#e5f5e0', '#a1d99b', '#31a354')) +
  labs(y = '%', x = '', fill = '') + facet_wrap(.~age_group, nrow = 7)

ggsave(here('Results', 'descriptive', 'covid_age_group.pdf'), 
       dpi=600, height = 400*0.8, width=300*0.8, units = 'mm')

# Cumulative COVID-19 Infection By Cancer Diagnosis Time
covid_date_vars <- colnames(cancerDesc)[grepl('covid_date_', colnames(cancerDesc))]

temp.table <- cancerDesc %>%
  pivot_longer(cols = covid_date_vars, 
               names_to = c('covid_number_infection'), values_to = c('covid_date')) %>%
  group_by(cancer_diagnosis_time) %>%
  mutate(n_total = length(unique(subject_id))) %>% ungroup() %>%
  group_by(cancer_diagnosis_time, covid_date) %>%
  arrange(covid_date) %>%
  summarize(n = n(), n_total = min(n_total))

write.table(temp.table, here('Results', 'descriptive','covid_diagnosis_time.csv'), sep = ';', row.names = F)

temp.graph <- temp.table %>%
  mutate(cum_sum = cumsum(n)/n_total) %>%
  ggplot(aes(x = covid_date, y = cum_sum)) + 
  geom_area(alpha = 1, position = 'identity', color = 'black') + theme_bw() + 
  theme(legend.position = 'top') + ylim(0, 1) + 
  scale_fill_manual(values = c('#e5f5e0', '#a1d99b', '#31a354')) +
  labs(y = '%', x = '', fill = '') + facet_wrap(.~cancer_diagnosis_time, nrow = 7)

ggsave(here('Results', 'descriptive', 'covid_diagnosis_time.pdf'), 
       dpi=600, height = 400*0.8, width=300*0.8, units = 'mm')

rm(cancerDesc)
