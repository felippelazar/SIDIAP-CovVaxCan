# ============================================================================ #
# 12. REM Analysis - COVID-19 Vaccine 1st and 2nd - Test Negative Outcomes     #
# Author: Felippe Lazar, IDIAP Jordi Gol, 2023 #
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
library(glue)

# Creating Folder for Exporting Files if Does Not Exist Yet
ifelse(!dir.exists(here('Results')), dir.create(here('Results')), FALSE)
ifelse(!dir.exists(here('Results', 'dose_12')), dir.create(here('Results', 'dose_12')), FALSE)
ifelse(!dir.exists(here('Results', 'dose_12', 'negative outcomes')), dir.create(here('Results', 'dose_12', 'negative outcomes')), FALSE)


# Importing List of Concept IDS for Negative Outcomes
# We will need to iterate through this table
df_negative_outcomes <- read.csv(here('NCO.csv'), sep = ';', header=T)

for (negative_outcome_id in df_negative_outcomes$ConceptId) {
      type_var <- df_negative_outcomes$Type[df_negative_outcomes$ConceptId == negative_outcome_id]
      
      # Creating a Temporary Dataset to Add Temporary Outcome 
      if(type_var == 'condition_ocurrence'){
            cancerNO <- cancerCohort %>% 
                  select(subject_id) %>%
                  left_join(cdm$condition_occurrence %>% 
                                  filter(person_id %in% cancerIDS) %>%
                                  filter(condition_concept_id == negative_outcome_id) %>%
                                  select(person_id, condition_start_date, condition_concept_id) %>%
                                  collect(),
                            by = c('subject_id' = 'person_id')) %>%
                  filter(coalesce(condition_start_date >= as.Date.character('27/12/2020', format = '%d/%m/%Y'), TRUE)) %>%
                  arrange(subject_id, condition_start_date) %>%
                  mutate(outcome_number = unlist(mapply(
                        function(len, val) if (val == 0) rep(0, len) else 1:len,
                        rle(as.numeric(subject_id))$lengths, rle(as.numeric(subject_id))$values)))
            
            cancerNOWide <- cancerNO %>%
                  pivot_wider(id_cols = subject_id, names_from = c('outcome_number'), 
                              values_from = c('condition_start_date'),
                              names_glue = "{.value}_{outcome_number}") %>%
                  select(-ends_with('_NA'))
            
            cancerNOWide <- cancerCohort %>% select(subject_id) %>% left_join(cancerNOWide)
            
      }else if(type_var == 'drug_exposure'){
            cancerNO <- cancerCohort %>% 
                  select(subject_id) %>%
                  left_join(cdm$drug_exposure %>% 
                                  filter(person_id %in% cancerIDS) %>%
                                  filter(drug_concept_id == negative_outcome_id) %>%
                                  select(person_id, drug_exposure_start_date, drug_concept_id) %>%
                                  collect(),
                            by = c('subject_id' = 'person_id')) %>%
                  filter(coalesce(drug_exposure_start_date >= as.Date.character('27/12/2020', format = '%d/%m/%Y'), TRUE)) %>%
                  arrange(subject_id, drug_exposure_start_date) %>%
                  mutate(outcome_number = unlist(mapply(
                        function(len, val) if (val == 0) rep(0, len) else 1:len,
                        rle(as.numeric(subject_id))$lengths, rle(as.numeric(subject_id))$values)))
            
            cancerNOWide <- cancerNO %>%
              rename("condition_start_date" = "drug_exposure_start_date") %>%
              rename("condition_concept_id" = "drug_concept_id") %>%
                  pivot_wider(id_cols = subject_id, names_from = c('outcome_number'), 
                              values_from = c('condition_start_date'),
                              names_glue = "{.value}_{outcome_number}") %>%
                  select(-ends_with('_NA'))
            
            cancerNOWide <- cancerCohort %>% select(subject_id) %>% left_join(cancerNOWide)
      }else if(type_var == 'visit_ocurrence'){
        cancerNO <- cancerCohort %>% 
          select(subject_id) %>%
          left_join(cdm$visit_occurrence %>% 
                      filter(person_id %in% cancerIDS) %>%
                      filter(visit_concept_id == negative_outcome_id) %>%
                      select(person_id, visit_start_date, visit_concept_id) %>%
                      collect(),
                    by = c('subject_id' = 'person_id')) %>%
          filter(coalesce(visit_start_date >= as.Date.character('27/12/2020', format = '%d/%m/%Y'), TRUE)) %>%
          arrange(subject_id, visit_start_date) %>%
          mutate(outcome_number = unlist(mapply(
            function(len, val) if (val == 0) rep(0, len) else 1:len,
            rle(as.numeric(subject_id))$lengths, rle(as.numeric(subject_id))$values)))
        
        cancerNOWide <- cancerNO %>%
          rename("condition_start_date" = "visit_start_date") %>%
          rename("condition_concept_id" = "visit_concept_id") %>%
          pivot_wider(id_cols = subject_id, names_from = c('outcome_number'), 
                      values_from = c('condition_start_date'),
                      names_glue = "{.value}_{outcome_number}") %>%
          select(-ends_with('_NA'))
        
        cancerNOWide <- cancerCohort %>% select(subject_id) %>% left_join(cancerNOWide)
      }
      
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

negative_outcomes_vars <- colnames(cancerNOWide)[grepl('condition_start_date', colnames(cancerNOWide))]

dfREMlong <- dfREMlong %>%
      left_join(cancerNOWide, by = c('subject_id')) %>%
      # NA occurrences for outcomes before minimum Date
      mutate(across(contains('condition_start_date'), ~ if_else(.x <= enrol_date, as.Date(NA), .x))) %>%
      # NA occurrences for outcomes after maximum Date
      mutate(across(contains('condition_start_date'), ~ if_else(.x > maxDate, as.Date(NA), .x))) %>% 
      # Selecting minimum date of COVID-19 infection and hospitalization (excluding previously created NAs)
      mutate(condition_start_date = exec(pmin, !!!rlang::syms(negative_outcomes_vars), na.rm = TRUE)
      ) %>%
      select(-starts_with('condition_start_date_'))

dfVac <- dfREMlong %>%
      filter(vac_day == 1) %>%
      setNames(paste0('gv_', names(.)))

dfControl <- dfREMlong %>%
      filter(vac_day == 0) %>%
      setNames(paste0('gc_', names(.)))

dfREM <- dfVac %>%
      left_join(dfControl, by = c('gv_subject_pair' = 'gc_subject_pair'))

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
    gc_outcome_condition_date = pmin(gc_outcome_vac_date_1, gc_condition_start_date, gc_death_date, gv_vac_exposure_date_3, na.rm = T),
    # Vaccinated Group Outcomes
    gv_outcome_condition_date = pmin(gc_outcome_vac_date_1, gv_condition_start_date, gv_death_date, gv_vac_exposure_date_3, na.rm = T)) %>%
  # Compare the chosen outcome with the outcome of interest
  mutate(
    # Control Group Outcomes
    gc_outcome_condition_status = case_when(
      gc_outcome_condition_date == gc_condition_start_date ~ 2, # Negative Outcome Condition
      gc_outcome_condition_date == gc_death_date ~ 1, # Death
      gc_outcome_condition_date == gc_outcome_vac_date_1 ~ 0, # 1st dose Vaccinated Control Group
      gc_outcome_condition_date == gv_vac_exposure_date_3 ~ 0, # 3rd dose Vaccinated Group
      is.na(gc_outcome_condition_date) ~ 0 # No Outcome
    ),
    # Vaccinated Group Outcomes
    gv_outcome_condition_status = case_when(
      gv_outcome_condition_date == gv_condition_start_date ~ 2, # Negative Outcome Condition
      gv_outcome_condition_date == gv_death_date ~ 1, # Death
      gv_outcome_condition_date == gc_outcome_vac_date_1  ~ 0, # 1st dose Vaccinated Control Group
      gv_outcome_condition_date == gv_vac_exposure_date_3 ~ 0, # 3rd dose Vaccinated Group
      is.na(gv_outcome_condition_date) ~ 0 # No Outcome
    )) %>%
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
    gc_outcome_condition_time = as.numeric(difftime(gc_outcome_condition_date, gv_vac_exposure_date_1,units = 'days')),
    # Vaccinated Group
    gv_outcome_condition_time = as.numeric(difftime(gv_outcome_condition_date, gv_vac_exposure_date_1, units = 'days')),
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
  setNames(gsub('gc_', '', names(.)))

dfREMlong <- bind_rows(dfREMVac, dfREMControl)
rm(dfREMVac)
rm(dfREMControl)
# Creating Unique Identifiers for Cox Analysis (as matches can be duplicated eventually)
dfREMlong <- dfREMlong %>%
  mutate(new_id = 1:nrow(.))

# Creating tmerge function
# Creating dataset with all periods
tmerge_all_periods <- function(df, outcome_column_time, outcome_column_status){
  
  df <- df %>%
    mutate(p1_time = vac_exposure_time_1,
           p2_time = if_else(coalesce(vac_exposure_time_1 + 14 > vac_exposure_time_2, F), NA, vac_exposure_time_1 + 14),
           p3_time = if_else(coalesce(vac_exposure_time_1 + 59 > vac_exposure_time_2, F), NA, vac_exposure_time_1 + 59),
           p4_time = if_else(coalesce(vac_exposure_time_1 + 59 > vac_exposure_time_2, F), NA, vac_exposure_time_1 + 59),
           p5_time = vac_exposure_time_2,
           p6_time = vac_exposure_time_2 + 14,
           p7_time = vac_exposure_time_2 + 60,
           p8_time = vac_exposure_time_2 + 60,
           p9_time = vac_exposure_time_2 + 90,
           p10_time = vac_exposure_time_2 + 120) %>%
    mutate(other_voc_time = max(0, difftime(covidVOC[['Other VOC']], enrol_date, units = 'days')), 
           delta_voc_time = max(0, difftime(covidVOC[['Delta VOC']], enrol_date, units = 'days')))
  
  dft <- tmerge(df, df, 
                id=new_id, 
                outcome = event(df[[outcome_column_time]], df[[outcome_column_status]]),
                dose_one = tdc(vac_exposure_time_1),
                dose_two = tdc(vac_exposure_time_2),
                dose_three = tdc(vac_exposure_time_3),
                p1 = tdc(p1_time),
                p2 = tdc(p2_time),
                p3 = tdc(p3_time),
                p4 = tdc(p4_time),
                p5 = tdc(p5_time),
                p6 = tdc(p6_time),
                p7 = tdc(p7_time),
                p8 = tdc(p8_time),
                p9 = tdc(p9_time),
                p10 = tdc(p10_time),
                other_voc = tdc(other_voc_time),
                delta_voc = tdc(delta_voc_time)
  )
  
  dft <- dft %>%
    mutate(period = paste(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, sep='-')) %>%
    mutate(period = case_when(
      period == '0-0-0-0-0-0-0-0-0-0' ~ 'no-vax',
      period == '1-0-0-0-0-0-0-0-0-0' ~ 'V1 0-14D',
      period == '1-1-0-0-0-0-0-0-0-0' ~ 'V1 14-59D',
      period == '1-1-1-0-0-0-0-0-0-0' ~ 'V1 60D+',
      period == '1-1-1-1-0-0-0-0-0-0' ~ 'V1 60D+',
      period %in% c('1-1-0-0-1-0-0-0-0-0', '1-1-1-0-1-0-0-0-0-0', '1-1-1-1-1-0-0-0-0-0') ~ 'V1V2 0-13D',
      period %in% c('1-1-0-0-1-1-0-0-0-0', '1-1-1-0-1-1-0-0-0-0', '1-1-1-1-1-1-0-0-0-0') ~ 'V1V2 14-59D',
      period %in% c('1-1-0-0-1-1-1-0-0-0', '1-1-1-0-1-1-1-0-0-0', '1-1-1-1-1-1-1-0-0-0') ~ 'V1V2 14-59D',
      period %in% c('1-1-0-0-1-1-1-1-0-0', '1-1-1-0-1-1-1-1-0-0', '1-1-1-1-1-1-1-1-0-0') ~ 'V1V2 60-89D',
      period %in% c('1-1-0-0-1-1-1-1-1-0', '1-1-1-0-1-1-1-1-1-0', '1-1-1-1-1-1-1-1-1-0') ~ 'V1V2 90-120D',
      period %in% c('1-1-0-0-1-1-1-1-1-1', '1-1-1-0-1-1-1-1-1-1') ~ 'V1V2 120D+'
    )) %>% 
    mutate(period = factor(period, levels = c('no-vax', 'V1 0-14D', 'V1 14-59D', 'V1 60D+', 'V1V2 0-13D', 'V1V2 14-59D',
                                              'V1V2 60-89D', 'V1V2 90-120D', 'V1V2 120D+'))) %>%
    mutate(voc = paste(other_voc, delta_voc, sep='-')) %>%
    mutate(covid_voc = fct_case_when(
      voc == '1-0' ~ 'Other VOC',
      voc == '1-1' ~ 'Delta VOC'
    ))
  
  return(dft)
}

# Creating Dataset with Three Periods
tmerge_three_periods <- function(df, outcome_column_time, outcome_column_status){
  
  df <- df %>%
    mutate(p1_time = vac_exposure_time_1 + 14,
           p2_time = vac_exposure_time_2 + 7) %>%
    mutate(other_voc_time = max(0, difftime(covidVOC[['Other VOC']], enrol_date, units = 'days')), 
           delta_voc_time = max(0, difftime(covidVOC[['Delta VOC']], enrol_date, units = 'days'))
    )
  
  dft <- tmerge(df, df, 
                id=new_id, 
                outcome = event(df[[outcome_column_time]], df[[outcome_column_status]]),
                dose_one = tdc(vac_exposure_time_1),
                dose_two = tdc(vac_exposure_time_2),
                dose_three = tdc(vac_exposure_time_3),
                p1 = tdc(p1_time),
                p2 = tdc(p2_time),
                other_voc = tdc(other_voc_time),
                delta_voc = tdc(delta_voc_time)
  )
  
  dft <- dft %>%
    mutate(period = paste(p1, p2, sep='-')) %>%
    mutate(period = fct_case_when(
      period == '0-0' ~ 'no-vax',
      period == '1-0' ~ 'V1 14D+',
      period == '1-1' ~ 'V2 7D+')) %>%
    mutate(voc = paste(other_voc, delta_voc, sep='-')) %>%
    mutate(covid_voc = fct_case_when(
      voc == '1-0' ~ 'Other VOC',
      voc == '1-1' ~ 'Delta VOC'
    ))
  
  return(dft)
}

tidyInteractionCox <- function(interaction_var, df, outcome){
  print(paste('Testing Interaction for Variable:', interaction_var))
  
  formulaStringInt <- paste("Surv(tstart, tstop, outcome == 2) ~", paste('period', interaction_var, sep="*"))
  m <- coxph(as.formula(formulaStringInt), data=df)
  
  m_aic <- AIC(m)
  m_bic <- BIC(m)
  size  <- m$n
  nevents <- m$nevent
  
  formulaStringNull <- paste("Surv(tstart, tstop, outcome == 2) ~", paste('period', interaction_var, sep="+"))
  m_null <- coxph(as.formula(formulaStringNull), data=df)
  
  p_int_lrtest <- anova(m, m_null)[['Pr(>|Chi|)']][2]
  
  broom::tidy(m, conf.int = T)
  
  m_emeans <- emmeans(m, specs = c('period', interaction_var))
  m_contrasts <- contrast(m_emeans, 'trt.vs.ctrl', by = interaction_var)
  tidy_contrasts <- confint(m_contrasts, type = 'wald') %>% as.tibble() %>%
    rename('term' = all_of(interaction_var)) %>%
    mutate(term = as.character(term)) %>%
    mutate(model_AIC = m_aic, 
           model_BIC = m_bic,
           model_size = size,
           model_nevents = nevents,
           model_p_value = p_int_lrtest,
           model_interaction_var = interaction_var)
  
  return(tidy_contrasts)
}

#-- Analysis
#-- Negative Outcome
fit <- survfit(Surv(outcome_condition_time, outcome_condition_status == 2) ~ tx_group, 
               data = dfREMlong)

temp.cumhaz <- ggsurvplot(fit, data = dfREMlong, fun = 'cumhaz', xlim = c(0, 180),
                          legend.labs = c("Control", "Vaccinated"),   break.x.by = 30, ggtheme = theme_bw(), 
                          palette = c("#E7B800","#2E9FDF"), risk.table = T)

pdf(here('Results', 'dose_12', 'negative outcomes', glue('outcome_curve_periods', negative_outcome_id, '.pdf')))
print(temp.cumhaz, newpage = FALSE)
dev.off()
# 
# dfREM_condition <- tmerge_all_periods(dfREMlong, 'outcome_condition_time', 'outcome_condition_status')
# 
# coxph(Surv(tstart, tstop, outcome == 2) ~ period, 
#       data = dfREM_condition) %>% broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>% broom.helpers::tidy_add_n() %>%
#       write.table(here('Results', 'dose_12', 'negative outcomes', glue('outcome_all_periods', negative_outcome_id, '.csv')), sep = ';', row.names = F)

dfREM_condition <- tmerge_three_periods(dfREMlong, 'outcome_condition_time', 'outcome_condition_status')

coxph(Surv(tstart, tstop, outcome == 2) ~ period, 
      data = dfREM_condition) %>% broom.helpers::tidy_and_attach(exponentiate=T, conf.int=T) %>% 
      broom.helpers::tidy_add_reference_rows() %>% 
      broom.helpers::tidy_add_n() %>%
      write.table(here('Results', 'dose_12', 'negative outcomes', glue('outcome_three_periods', negative_outcome_id, '.csv')), sep = ';', row.names = F)
}
