# ============================================================================ #
# X. Creating Auxiliary Variables for COVID-19 1st and 2nd Dose Analysis  #
# Author: Felippe Lazar, IDIAP Jordi Gol, 2023 #
# ============================================================================ #

# Creating Dose Object
dose_analysis <- 'dose_12'

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
            mutate(p1_time = vac_exposure_time_1,
                   p2_time = vac_exposure_time_1 + 14,
                   p3_time = vac_exposure_time_2 + 7) %>%
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
                    p3 = tdc(p3_time),
                    other_voc = tdc(other_voc_time),
                    delta_voc = tdc(delta_voc_time)
      )
      
      dft <- dft %>%
            mutate(period = paste(p1, p2, p3, sep='-')) %>%
            mutate(period = fct_case_when(
                  period == '0-0-0' ~ 'no-vax',
                  period == '1-0-0' ~ 'V1 0-14D',
                  period == '1-1-0' ~ 'V1 14D+',
                  period == '1-1-1' ~ 'V2 7D+')) %>%
            mutate(voc = paste(other_voc, delta_voc, sep='-')) %>%
            mutate(covid_voc = fct_case_when(
                  voc == '1-0' ~ 'Other VOC',
                  voc == '1-1' ~ 'Delta VOC'
            ))
      
      return(dft)
}


tidyInteractionCox <- function(interaction_var, df, outcome, outcome_number = 2){
      print(paste('Testing Interaction for Variable:', interaction_var))
      
      
      formulaStringInt <- paste0("Surv(tstart, tstop, outcome == ", outcome_number, ") ~ ", paste('period', interaction_var, sep="*"))
      m <- coxph(as.formula(formulaStringInt), data=df)
      
      m_aic <- AIC(m)
      m_bic <- BIC(m)
      size  <- m$n
      
      formulaStringNull <- paste0("Surv(tstart, tstop, outcome == ", outcome_number, ") ~ ", paste('period', interaction_var, sep="+"))
      m_null <- coxph(as.formula(formulaStringNull), data=df)
      
      p_int_lrtest <- anova(m, m_null)[['Pr(>|Chi|)']][2]
      
      m_emeans <- emmeans(m, specs = c('period', interaction_var))
      m_contrasts <- contrast(m_emeans, 'trt.vs.ctrl', by = interaction_var)
      m_contrasts <- confint(m_contrasts, type = 'wald') %>% as.tibble() 
      
      m_obs <- df %>%
            mutate(ttotal = tstop - tstart) %>%
            group_by(across(c(interaction_var, 'period'))) %>%
            summarise(n_events = sum(across('outcome') == outcome_number), n_obs = sum(!duplicated(new_id)), exp_time = sum(across('ttotal')))
      
      tidy_contrasts <- m_contrasts %>%
            mutate(contrast2 = as.character(contrast)) %>%
            separate(col = 'contrast2', into = c('trt', 'ctrl'), sep = ' - ') %>%
            mutate(across(c('trt', 'ctrl'), ~ gsub('[()]', '', .x))) %>%
            left_join(m_obs %>% rename("crtl.n_events" = "n_events", 
                                       "crtl.n_obs" = "n_obs", 
                                       "crtl.exp_time" = "exp_time",
                                       "ctrl" = "period"), 
                      by = c(interaction_var, 'ctrl')) %>%
            left_join(m_obs %>% rename("trt.n_events" = "n_events", 
                                       "trt.n_obs" = "n_obs", 
                                       "trt.exp_time" = "exp_time",
                                       "trt" = "period"), 
                      by = c(interaction_var, 'trt')) %>%
            rename('term' = all_of(interaction_var)) %>%
            mutate(term = as.character(term)) %>%
            mutate(model_AIC = m_aic, 
                   model_BIC = m_bic,
                   model_size = size,
                   model_p_value = p_int_lrtest,
                   model_interaction_var = interaction_var)
      
      return(tidy_contrasts)
}
# Creating Vector of Variables for Descriptive Analysis
# Demographics
vars_demographics <- c('age', 'age_group', 'gender_concept_id', 'medea_group_2001', 'aga_name')

vars_vaccine_type <- c('vac_concept_id_1', 'vac_concept_id_2', 'vac_concept_id_3', 'vac_scheme', 'vac_heterologous')

vars_others <- c('vac_day')

vars_matching <- c('matched_vac', 'matched_control', 'matched_any')

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

vars_outcomes_status <- c('outcome_covid_status', 'outcome_hosp_status', 'outcome_any_hosp_status', 'outcome_hosp_severe_status', 'outcome_death_status', 'outcome_hosp_death_status')
vars_outcomes_time <- c('outcome_covid_time', 'outcome_hosp_time', 'outcome_any_hosp_time', 'outcome_hosp_severe_time', 'outcome_death_time', 'outcome_hosp_death_time')

vars_covid_tests <- c('n_covid_tests_0', 'n_covid_tests_1', 'n_covid_tests')
vars_health_visits <- c('n_visits_outpatient', 'n_visits_telehealth', 'n_visits_inpatient')

vars_subgroup_analysis <- c('age_bin_60', 'age_bin_65', 'age_bin_70', 'age_bin_75', 'age_bin_80', 'age_bin_85',
                            'gender_concept_id', 'covid_voc', 'vac_heterologous', 'visits_outpatient_cat',
                            'cancer_diagnosis_time_bin_0', 'cancer_diagnosis_time_bin_1', 'cancer_diagnosis_time_bin_2',
                            'cancer_diagnosis_time_bin_3', 'CCI_Metastatic_Solid_Tumor', 
                            'cancer_dx_breast', 'cancer_dx_prostate', 'cancer_dx_colorectal', 'cancer_dx_lung', 
                            'cancer_dx_head_neck', 'cancer_dx_endometrium', 'cancer_dx_bladder',
                            'cancer_dx_liver_biliary', 'cancer_dx_melanoma', 'cancer_dx_pancreas', 'cancer_dx_kidney', 
                            'cancer_dx_gastric', 'cancer_dx_esophagus', 'cancer_dx_testis', 'cancer_dx_thyroid',
                            'cancer_dx_CNS', 'cancer_dx_neuroendocrine', 'cancer_dx_sarcomas', 'cancer_dx_leukemia', 
                            'cancer_dx_myeloma', 'cancer_dx_lymphoma', "cancer_group_gastro_intestinal", "cancer_group_genito_urinary", 
                            "cancer_group_thyroid", "cancer_group_breast", "cancer_group_sarcomas", "cancer_group_thorax",
                            "cancer_group_gynecology", "cancer_group_head_neck", "cancer_group_hemathological",
                            "cancer_group_skin")
