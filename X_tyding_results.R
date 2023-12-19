library(tidyverse)
library(openxlsx)
library(here)
library(readxl)

# Setting WD
# setwd('/Users/felippelazarneto/Google Drive (felippe.neto@alumni.usp.br)/SIDIAP Analysis/')
source('utils.R')
source('utils_tidying.R')

#----- Creating Cancer and Charlson Definitions Workbook
# # Reading Charlson Diagnosis
# charlson_df <- read.table('CharlsonIDS_17042023.csv', sep = ';')
# 
# charlson_df_tidied <- charlson_df %>%
#   group_by(charlson_comorbidity) %>%
#   summarise(descendant_codes = list(unique(descendant_concept_id))) %>%
#   left_join(charlson_df %>%
#               group_by(charlson_comorbidity) %>%
#               summarise(ancestor_codes = list(unique(ancestor_concept_id)),
#                         charlson_weight = min(charlson_weight))) %>%
#   mutate(descendant_codes = map_chr(descendant_codes, str_flatten_comma)) %>%
#   mutate(ancestor_codes = map_chr(ancestor_codes, str_flatten_comma)) %>%
#   select(charlson_comorbidity, charlson_weight, ancestor_codes)
# 
# # Cancer Diagnosis Tidied
# cancer_diagnosis <- read_excel('CancerDiagnosis26042023.xlsx') %>% filter(include == 1)
# 
# # - Per Diagnosis
# cancer_diagnosis_dx_tidied <- cancer_diagnosis %>%
#   group_by(cancer_dx) %>%
#   summarise(cancer_dx_codes = list(unique(condition_concept_id))) %>%
#   mutate(cancer_dx_codes = map_chr(cancer_dx_codes, str_flatten_comma))
# 
# # - Per Group
# cancer_diagnosis_group_tidied <- cancer_diagnosis %>%
#   group_by(cancer_group) %>%
#   summarise(cancer_group_codes = list(unique(condition_concept_id))) %>%
#   mutate(cancer_group_codes = map_chr(cancer_group_codes, str_flatten_comma))
# 
# ## Create a new workbook
# wb <- createWorkbook()
# 
# ## Add a worksheet
# addStyledSheet(wb, 'charlson_diagnosis', charlson_df_tidied)
# addStyledSheet(wb, 'cancer_diagnosis', cancer_diagnosis_dx_tidied)
# addStyledSheet(wb, 'cancer_group_diagnosis', cancer_diagnosis_group_tidied)
# 
# # Save workbook
# saveWorkbook(wb, "Definitions.xlsx", overwrite = TRUE)

#----- Tidying Descriptive Analysis
# Getting Var Names to Better Rename
var_names_recode <- list(
      'age' = 'Age',
      'age_group' = 'Age (Group)',
      'gender_concept_id' = 'Sex/Gender Female',
      'medea_group_2001' = 'MEDEA (2001)',
      'aga_name' =  'AGA',
      'cancer_diagnosis_time' = 'Time Since Diagnosis (Years)',
      'cancer_dx_breast' = 'Breast',
      'cancer_dx_prostate' = 'Prostate',
      'cancer_dx_colorectal' = 'Colorectal',
      'cancer_dx_lung' = 'Lung',
      'cancer_dx_head_neck' = 'Head and Neck', 
      'cancer_dx_endometrium' = 'Endometrium',
      'cancer_dx_cervix_uterus' = 'Cervix/Uterus',
      'cancer_dx_bladder' = 'Bladder',
      'cancer_dx_liver_biliary' = 'Biliary/CHC',
      'cancer_dx_melanoma' = 'Melanoma',
      'cancer_dx_pancreas' = 'Pancreas',
      'cancer_dx_kidney' = 'Kidney', 
      'cancer_dx_gastric' = 'Gastric',
      'cancer_dx_esophagus' = 'Esophagus',
      'cancer_dx_testis' = 'Testis',
      'cancer_dx_thyroid' = 'Thyroid',
      'cancer_dx_CNS' = 'CNS',
      'cancer_dx_neuroendocrine' = 'Neuroendocrine Cancer (NEC)',
      'cancer_dx_sarcomas' = 'Sarcomas',
      'cancer_dx_leukemia' = 'Leukemia',
      'cancer_dx_myeloma' = 'Myeloma',
      'cancer_dx_lymphoma' = 'Lymphoma',
      'cancer_dx_other' = 'Other',
      'cancer_dx_other2' = 'Other',
      'cancer_dx_undefined' = 'Undefined',
      'cancer_group_gastro_intestinal' = 'Gastro-intestinal',
      'cancer_group_genito_urinary' = 'Genito-urinary',
      'cancer_group_thyroid' = 'Thyroid',
      'cancer_group_breast' = 'Breast', 
      'cancer_group_sarcomas' = 'Sarcomas',
      'cancer_group_thorax' = 'Thorax', 
      'cancer_group_gynecology' = 'Gynecology',
      'cancer_group_head_neck' = 'Head-Neck', 
      'cancer_group_hemathological' = 'Hematological',
      'cancer_group_skin' = 'Skin',
      'cancer_group_CNS' = 'CNS',
      'cancer_group_undefined' = 'Undefined',
      'cancer_group_nasopharynx' = 'Nasopharynx',
      'cancer_group_neuroendocrine' = 'Neuroendocrine Cancer (NEC)',
      'cancer_group_other' = 'Other',
      'cancer_group_other_2' = 'Other',
      'charlson_index' = 'Charlson Comorbidity Index (CCI)',
      'CCI_Rheumatologic_Disease' = 'Reumatological Disease',
      'CCI_Any_Malignancy' = 'Malignancy',
      'CCI_Metastatic_Solid_Tumor' = 'Metastatic Solid Tumor',
      'CCI_Diabetes_Mild' = 'Mild Diabetes',
      'CCI_Mild_Liver_Disease' = 'Mild Liver Disease',
      'CCI_Peptic_Ulcer_Disease' = 'Peptic Ulcer Disease',
      'CCI_Chronic_Pulmonary_Disease' = 'Chronic Pulmonary Disease',
      'CCI_Renal_Disease' = 'Chronic Renal Disease',
      'CCI_Cerebrovascular_Disease' = 'Cerebrovascular Disease', 
      'CCI_Diabetes_With_Complications' = 'Diabetes with Complications',
      'CCI_Congestive_Heart_Failure' = 'Congestive Heart Failure (CHF)',
      'CCI_Dementia' = 'Dementia',
      'CCI_Peripheral_Artery_Disease' = 'Peripheral Artery  Disease',
      'CCI_Myocardial_Infarction' = 'Myocardial Infarction',
      'CCI_Moderate_Severe_Liver_Disease' = 'Moderate/Severe Liver Disease',
      'CCI_Hemiplegia_Paraplegia' = 'Hemiplegia/Paraplegia',
      'CCI_AIDS' = 'AIDS/HIV',
      'n_visits_outpatient' = 'Number of Outpatient Visits',
      'n_visits_telehealth' = 'Number of Telehealth Visits',
      'visits_outpatient_cat' = 'Number of Outpatient Visits (category)',
      'outcome_covid_status' = 'Infection',
      'outcome_hosp_status' = 'Hospitalizations',
      'outcome_hosp_severe_status' = 'Severe Hospitalizations',
      'outcome_hosp_death_status' = 'Hospitalizations or Death (Combined)',
      'outcome_covid_time' = 'Fup Time COVID-19 Infection (Median, IQR)',
      'outcome_hosp_time' = 'Fup Time COVID-19 Hosp. (Median, IQR)',
      'outcome_hosp_severe_time' = 'Fup Time COVID-19 Severe Hosp. (Median, IQR)',
      'outcome_hosp_death_time' = 'Fup Time COVID-19 Hosp. or Death (Median, IQR)',
      'tx_group' = 'Treatment Group',
      'vac_day' = 'Vaccinated on Elegibility',
      'matched_vac' = 'Matched as Vaccinated',
      'matched_control' = 'Matched as Control',
      'matched_any' = 'Matched Any',
      'vac_concept_id_1' = 'Vaccine Dose One',
      'vac_concept_id_2' = 'Vaccine Dose Two', 
      'vac_concept_id_3' = 'Vaccine Dose Three',
      'vac_scheme' = '1st and 2nd Vaccine Combination',
      'vac_heterologous' = 'Unmatched 1st and 2nd Vaccine (Heterologous)'
)


#---- Tidying Descriptive Analysis
files_descriptive <- list(
      'desc_eligible_[^m].*_doses_clean.csv' = 'eligibles',
      'desc_eligible_matched_.*_doses_clean.csv' = 'matched vs unmatched',
      'desc_tx_group_matched_.*_doses_clean.csv' = 'treat groups baseline',
      'desc_outcomes_tx_group_matched_.*_doses_clean.csv' = 'treat groups outcomes')

# Creating Regex for Loading File Paths
string_regex_descriptive = paste0('(', paste(names(files_descriptive), collapse = '|'), ')')

tidy_add_descriptive <- function(file_path, workbook){
      
      file_logical <- sapply(names(files_descriptive), function(x) str_detect(file_path, x))
      analysis <- files_descriptive[[names(files_descriptive)[file_logical]]]
      
      if(analysis == 'matched vs unmatched'){
            temp_table <- read.csv2(file_path, row.names=NULL, header = T) %>% mutate(row.names = map_chr(row.names, adjust_names))
            colnames(temp_table) <- c('variable', 'unmatched', 'matched', 'p-test', 'test')
      }else if(analysis == 'eligibles'){
            temp_table <- read.csv2(file_path, row.names=NULL, header = T) %>% mutate(row.names = map_chr(row.names, adjust_names))
            colnames(temp_table) <- c('variable', 'elegibles')
      }else if(analysis == 'eligibles desc'){
            temp_table <- read.csv2(file_path, row.names=NULL, header = T) %>% 
                  rename(row.names = X) %>%
                  mutate(row.names = map_chr(row.names, adjust_names))
            colnames(temp_table) <- c('variable', 'elegibles')
      }else if(analysis == 'vacc vs unvax'){
            temp_table <- read.csv2(file_path, row.names=NULL, header = T) %>%                   
                  rename(row.names = X) %>%
                  mutate(row.names = map_chr(row.names, adjust_names))
            colnames(temp_table) <- c('variable', 'un-vax', 'vaccinated', 'p-test', 'test')
      }else if(analysis == 'treat groups outcomes'){
            temp_table <- read.csv2(file_path, row.names=NULL, header = T) %>% mutate(row.names = map_chr(row.names, adjust_names))
            colnames(temp_table) <- c('variable', 'un-vax', 'vaccinated', 'p-test', 'test')
      }else if(analysis == 'treat groups baseline'){
            temp_table <- read.csv2(file_path, row.names=NULL, header = T) %>% mutate(row.names = map_chr(row.names, adjust_names))
            colnames(temp_table) <- c('variable', 'un-vax', 'vaccinated', 'p-test', 'test')
      }
      
      # Getting Dimensions
      n_row = dim(temp_table)[1]
      n_col = dim(temp_table)[2]
      
      # Creating Worksheet
      sheet_name = analysis
      addWorksheet(workbook, sheet_name)
      
      # Getting Table Dimension
      max_row = dim(temp_table)[1]+1
      max_col = dim(temp_table)[2]
      
      # Getting Odds and Even Rows (except title)
      rows_odd <- seq(2, max_row)[seq(2, max_row) %% 2 == 1]
      rows_even <- seq(2, max_row)[seq(2, max_row) %% 2 == 0]
      
      writeData(workbook, sheet = sheet_name, x = temp_table, headerStyle = header_gray_bordered)
      
      # Adding Rows and Columns Styles
      addStyle(workbook, sheet_name, style = whitebg_bordered_left, rows = rows_even, cols = 1, gridExpand = T)
      addStyle(workbook, sheet_name, style = lightgraybg_bordered_left, rows = rows_odd, cols = 1, gridExpand = T)
      addStyle(workbook, sheet_name, style = whitebg_bordered, rows = rows_even, cols = 2:n_col, gridExpand = T)
      addStyle(workbook, sheet_name, style = lightgraybg_bordered, rows = rows_odd, cols = 2:n_col, gridExpand = T)
      
      # Setting ColWidths and RowHeights
      setColWidths(workbook, sheet_name, cols = 1, widths = 50)
      setColWidths(workbook, sheet_name, cols = 2:n_col, widths = 19)
      setRowHeights(workbook, sheet_name, rows = 2:max_row, heights = 24)
      
      return(workbook)
}

#----- Tidying Main Results Analysis
files_main_results <- list(
      'outcome_covid_period_all.csv' = 'infection - all-periods',
      'outcome_covid_period_three.csv' = 'infection - three periods',
      'outcome_hosp_period_all.csv' = 'hospitalization - all-periods',
      'outcome_hosp_period_three.csv' = 'hospitalization - three periods',
      'outcome_death_period_all.csv' = 'death - all-periods',
      'outcome_death_period_three.csv' = 'death - three periods',
      'outcome_hosp_death_period_all.csv' = 'combined hosp death - all-periods',
      'outcome_hosp_death_period_three.csv' = 'combined hosp death - three periods')

# Creating Regex for Loading File Paths
string_regex_results = paste0('(', paste(names(files_main_results), collapse = '|'), ')')

tidy_add_main_results <- function(files_path, workbook){
      
      # Getting Files Results
      temp_tables <- lapply(files_path, function(x) read.csv(x, row.names=NULL, header = T, sep = ';') %>% 
                                  mutate(analysis = files_main_results[[str_extract(x, '([^/]*)$')]], .before=term))
      
      # Binding Tables
      temp_table <- do.call(bind_rows, temp_tables)
      temp_table <- temp_table %>%
            separate(analysis, into = c('outcome', 'periods'), sep = '-') %>%
            mutate(term = str_replace(term, 'period', '')) %>%
            mutate(est.conf.interval = sprintf('%.2f (%.2f - %.2f)', estimate, conf.low, conf.high)) %>%
            mutate(estimate_ve = if_else(estimate>1, 
                                         -(1-(1/estimate))*100, (1-estimate)*100),
                   conf.low_ve = if_else(conf.low>1, 
                                         -(1-(1/conf.low))*100, (1-conf.low)*100),
                   conf.high_ve= if_else(conf.high>1, 
                                         -(1-(1/conf.high))*100, (1-conf.high)*100)) %>%
            mutate(est_ve.conf.interval = sprintf('%.1f%% (%.1f - %.1f)', estimate_ve, conf.high_ve, conf.low_ve))
            
      
      # Getting Dimensions
      n_row = dim(temp_table)[1]
      n_col = dim(temp_table)[2]
      
      # Creating Worksheet
      sheet_name = 'main-outcomes'
      addWorksheet(workbook, sheet_name)
      
      # Getting Table Dimension
      max_row = dim(temp_table)[1]+1
      max_col = dim(temp_table)[2]
      
      # Getting Odds and Even Rows (except title)
      rows_odd <- seq(2, max_row)[seq(2, max_row) %% 2 == 1]
      rows_even <- seq(2, max_row)[seq(2, max_row) %% 2 == 0]
      
      # Adding Worksheet
      writeData(workbook, sheet = sheet_name, x = temp_table, headerStyle = header_gray_bordered)
      
      # Adding Rows and Columns Styles
      addStyle(workbook, sheet_name, style = whitebg_bordered, rows = rows_even, cols = 1:n_col, gridExpand = T)
      addStyle(workbook, sheet_name, style = lightgraybg_bordered, rows = rows_odd, cols = 1:n_col, gridExpand = T)
      
      # Setting ColWidths and RowHeights
      setColWidths(workbook, sheet_name, cols = 1:3, widths = 25)
      setColWidths(workbook, sheet_name, cols = 4:n_col, widths = 18)
      setRowHeights(workbook, sheet_name, rows = 2:max_row, heights = 24)
      
      return(workbook)
}

#----- Tidying Sub-group Analysis
files_subgroup_results <- list(
      'subgroup_outcome_covid_three_periods.csv' = 'infection',
      'subgroup_outcome_hosp_three_periods.csv' = 'hospitalization',
      'subgroup_outcome_hosp_death_three_periods.csv' = 'combined hosp death')

# Creating Regex for Loading File Paths
string_regex_subgroup = paste0('(', paste(names(files_subgroup_results), collapse = '|'), ')')

tidy_add_raw_subgroup <- function(files_path, workbook){

      # Getting Files Results
      temp_tables <- lapply(files_path, function(x) read_delim(x, delim = ';', locale = locale(decimal_mark = ".")) %>% 
                                  mutate(analysis = files_subgroup_results[[str_extract(x, '([^/]*)$')]]))
      # Binding Tables
      temp_table <- do.call(rbind, temp_tables)
      temp_table <- temp_table %>% select(analysis, model_interaction_var, term, contrast) %>%
            cbind(temp_table %>% select(-model_interaction_var, -term, -contrast, -analysis) %>%
                        mutate_all(as.numeric))
      
      # Getting Table Dimension
      max_row = dim(temp_table)[1]+1
      max_col = dim(temp_table)[2]
      
      # Getting Odds and Even Rows (except title)
      rows_odd <- seq(2, max_row)[seq(2, max_row) %% 2 == 1]
      rows_even <- seq(2, max_row)[seq(2, max_row) %% 2 == 0]
      
      #-- Adding Raw Results
      # Adding Worksheet
      sheet_name = 'sub-group raw results'
      addWorksheet(workbook, sheet_name)
      writeData(workbook, sheet = sheet_name, x = temp_table, headerStyle = header_gray_bordered)
      
      # Adding Rows and Columns Styles
      addStyle(workbook, sheet_name, style = whitebg_bordered, rows = rows_even, cols = 1:max_col, gridExpand = T)
      addStyle(workbook, sheet_name, style = lightgraybg_bordered, rows = rows_odd, cols = 1:max_col, gridExpand = T)
      
      # Setting ColWidths and RowHeights
      setColWidths(workbook, sheet_name, cols = c(1, 3, 5:max_col), widths = 16)
      setColWidths(workbook, sheet_name, cols = 2, widths = 30)
      setColWidths(workbook, sheet_name, cols = 4, widths = 25)
      setRowHeights(workbook, sheet_name, rows = 2:max_row, heights = 24)
      
      return(workbook)
}

tidy_add_tidy_subgroup <- function(files_path, workbook){
      
      # Getting Files Results
      temp_tables <- lapply(files_path, function(x) read_delim(x, delim = ';', locale = locale(decimal_mark = ".")) %>% 
                                  mutate(analysis = files_subgroup_results[[str_extract(x, '([^/]*)$')]]))
      # Binding Tables
      temp_table <- do.call(rbind, temp_tables)
      temp_table <- temp_table %>% select(analysis, model_interaction_var, term, contrast) %>%
            cbind(temp_table %>% select(-model_interaction_var, -term, -contrast, -analysis) %>%
                        mutate_all(as.numeric))
      
      # Tidying Table
      temp_table <- temp_table %>%
            select(analysis, model_interaction_var, term, contrast,
                   estimate, asymp.LCL, asymp.UCL, model_p_value) %>%
            mutate(estimate = exp(estimate),
                   asymp.UCL = exp(asymp.UCL),
                   asymp.LCL = exp(asymp.LCL)) %>%
            rename(estimate.HR = estimate,
                   asymp.UCL.HR = asymp.UCL,
                   asymp.LCL.HR = asymp.LCL) %>%
            mutate(estimate_ve = if_else(estimate.HR > 1, 
                                         -(1-(1/estimate.HR))*100, (1-estimate.HR)*100),
                   conf.low_ve = if_else(asymp.LCL.HR > 1, 
                                         -(1-(1/asymp.LCL.HR))*100, (1-asymp.LCL.HR)*100),
                   conf.high_ve= if_else(asymp.UCL.HR > 1, 
                                         -(1-(1/asymp.UCL.HR))*100, (1-asymp.UCL.HR)*100)) %>%
            mutate(est_ve.conf.interval = sprintf('%.1f%% (%.1f - %.1f)', estimate_ve, conf.high_ve, conf.low_ve))
      
      # Adding Worksheet
      sheet_name = 'sub-group tidy results'
      addWorksheet(workbook, sheet_name)

      # Getting Table Dimension
      max_row = dim(temp_table)[1]+1
      max_col = dim(temp_table)[2]
      
      # Getting Odds and Even Rows (except title)
      rows_odd <- seq(2, max_row)[seq(2, max_row) %% 2 == 1]
      rows_even <- seq(2, max_row)[seq(2, max_row) %% 2 == 0]
      
      writeData(workbook, sheet = sheet_name, x = temp_table, headerStyle = header_gray_bordered)
      
      # Adding Rows and Columns Styles
      addStyle(workbook, sheet_name, style = whitebg_bordered, rows = rows_even, cols = 1:max_col, gridExpand = T)
      addStyle(workbook, sheet_name, style = lightgraybg_bordered, rows = rows_odd, cols = 1:max_col, gridExpand = T)
      
      # Setting ColWidths and RowHeights
      setColWidths(workbook, sheet_name, cols = c(1, 3, 5:max_col), widths = 16)
      setColWidths(workbook, sheet_name, cols = 2, widths = 30)
      setColWidths(workbook, sheet_name, cols = 4, widths = 25)
      setRowHeights(workbook, sheet_name, rows = 2:max_row, heights = 24)
      
      return(workbook)

}

#--- Creating Tidied Excel Files
all_analysis <- list(
      'Results/dose_12/rem_main_analysis',
      'Results/dose_12/sub_group_cancer_strict',
      'Results/dose_12/sub_group_covid_lab',
      'Results/dose_12/sub_group_hosp_3_days',
      'Results/dose_12/sub_group_tested_patients',
      'Results/dose_12/sub_group_not_jansen',
      'Results/dose_3/rem_main_analysis',
      'Results/dose_3/sub_group_cancer_strict',
      'Results/dose_3/sub_group_covid_lab',
      'Results/dose_3/sub_group_hosp_3_days',
      'Results/dose_3/sub_group_tested_patients')

for(analysis in all_analysis){
      # Creating Workbook
      workbook <- createWorkbook()
      
      # Adding Descriptive Analysis
      files_path_descriptive <- list.files(analysis, pattern = string_regex_descriptive, full.names = T)
      lapply(files_path_descriptive, tidy_add_descriptive, workbook)
      
      # Adding Main Results
      files_path_results <- list.files(analysis, pattern = string_regex_results, full.names = T)
      workbook <- tidy_add_main_results(files_path_results, workbook)
      
      # Adding Sub-Group Analysis
      files_path_subgroup <- list.files(analysis, pattern = string_regex_subgroup, full.names = T)
      workbook <- tidy_add_raw_subgroup(files_path_subgroup, workbook)
      workbook <- tidy_add_tidy_subgroup(files_path_subgroup, workbook)
      
      # Saving File
      filename <- str_replace(analysis, 'Results/', '')
      filename <- str_replace(filename, '/', '_')
      saveWorkbook(workbook, paste0('Results Tidy/', filename, '.xlsx'), overwrite = TRUE)
      
}

desc_analysis <- list(
      'Results/descriptive')

for(descriptive_analysis in desc_analysis){
      # Creating Workbook
      workbook <- createWorkbook()
      
      # Adding Descriptive Analysis
      files_path_descriptive <- list.files(descriptive_analysis, pattern = string_regex_descriptive, full.names = T)
      lapply(files_path_descriptive, tidy_add_descriptive, workbook)
      
      # Saving File
      filename <- str_replace(descriptive_analysis, 'Results/', '')
      filename <- str_replace(filename, '/', '_')
      saveWorkbook(workbook, paste0('Results Tidy/', filename, '.xlsx'), overwrite = TRUE)
      
}


#--- Creating Forest Plots
library(meta)
library(forestplot)

files_subgroup_outcomes_forest <- list(
      'subgroup_outcome_hosp_death_three_periods.csv' = 'hosp_death',
      'subgroup_outcome_covid_three_periods.csv' = 'infection',
      'subgroup_outcome_hosp_three_periods.csv' = 'hospitalization')

all_analysis_forest <- list(
      'Results/dose_12/rem_main_analysis' = '_dose_12_rem_main_analysis',
      'Results/dose_3/rem_main_analysis' = '_dose_3_rem_main_analysis')

subgroups_analysis <- list(
      'gender|age',
      'cancer_diagnosis_time|CCI_Metastatic',
      'cancer_dx',
      'cancer_group',
      'covid_voc|vac_mRNA_12|visits_outpatient_cat',
      'age_bin_65|gender|cancer_diagnosis_time_bin_0|CCI_Metastatic_Solid_Tumor|covid_voc|vac_mRNA_12',
      
)

create_forest_table_subgroup <- function(file_forest){
      
      temp_table <- read_delim(file_forest, delim = ';', locale = locale(decimal_mark = "."))
      temp_table <- temp_table %>% select(model_interaction_var, term, contrast) %>%
            cbind(temp_table %>% select(-model_interaction_var, -term, -contrast) %>%
                        mutate_all(as.numeric))
      
      forest_table <- temp_table %>%
            select(model_interaction_var, contrast, 
                   term, estimate, asymp.LCL, asymp.UCL, model_p_value) %>%
            mutate(estimate = exp(estimate),
                   asymp.UCL = exp(asymp.UCL),
                   asymp.LCL = exp(asymp.LCL)) %>%
            mutate(estimate_ve = if_else(estimate > 1, 
                                         -(1-(1/estimate))*100, (1-estimate )*100),
                   conf.low_ve = if_else(asymp.LCL  > 1, 
                                         -(1-(1/asymp.LCL))*100, (1-asymp.LCL)*100),
                   conf.high_ve= if_else(asymp.UCL > 1, 
                                         -(1-(1/asymp.UCL))*100, (1-asymp.UCL)*100)) %>%
            mutate(est_ve.conf.interval = sprintf('%.1f (%.1f - %.1f)', estimate_ve, conf.low_ve, conf.high_ve)) %>%
            rename(mean = estimate_ve,
                   upper = conf.low_ve,
                   lower = conf.high_ve) %>%
            mutate(model_p_value = sprintf('%.5f', model_p_value)) %>%
            mutate(contrast = case_when(
                  contrast == '(V1 14D+) - (no-vax)' ~ 'Partially Vaccinated',
                  contrast == '(V2 7D+) - (no-vax)' ~ 'Fully Vaccinated',
                  contrast == '(V3 14-60D) - (no-vax)' ~ 'Booster 14D-60D',
                  contrast == '(V3 60+) - (no-vax)' ~ 'Booster 60D+'
            ))
      
      return(forest_table)
}

create_forest_plot_subgroup <- function(table, subgroup){
      f <- table %>% 
            mutate(term = case_when(
                  term == 'Age < 60' ~ '  < 60 years',
                  term == 'Age < 65' ~ '  < 65 years',
                  model_interaction_var == 'age_bin_70' & term == 'Age < 75' ~ '  < 70 years',
                  model_interaction_var == 'age_bin_80' & term == 'Age < 75' ~ '  < 80 years',
                  term == 'Age < 70' ~ '  < 75 years',
                  term == 'Age < 80' ~ '  < 85 years',
                  term == 'Age 60+' ~ '  >= 60 years',
                  term == 'Age 65+' ~ '  >= 65 years',
                  term == 'Age 70+' ~ '  >= 70 years',
                  term == 'Age 75+' ~ '  >= 75 years',
                  term == 'Age 80+' ~ '  >= 80 years',
                  term == 'Age 85+' ~ '  >= 85 years',
                  term == 'Other VOC' ~ '  Other',
                  term == 'Delta VOC' ~ '  Delta',
                  term == 'Omicron VOC' ~ '  Omicron',
                  term == 'M' ~ '  Male',
                  term == 'F' ~ '  Female',
                  term == '< 1y' ~ '  < 1 year',
                  term == '< 2y' ~ '  < 2 years',
                  term == '< 3y' ~ '  < 3 years',
                  term == '< 4y' ~ '  < 4 years',
                  term == '1-5yr' ~ '  1 - 5 years',
                  term == '2-5yr' ~ '  2 - 5 years',
                  term == '3-5yr' ~ '  3 - 5 years',
                  term == '4-5yr' ~ '  4 - 5 years',
                  model_interaction_var == 'visits_outpatient_cat' & term == '0' ~ '  0',
                  model_interaction_var == 'visits_outpatient_cat' & term == '1' ~ '  1',
                  model_interaction_var == 'visits_outpatient_cat' & term == '2' ~ '  2',
                  model_interaction_var == 'visits_outpatient_cat' & term == '3+' ~ '  >= 3',
                  model_interaction_var != 'visits_outpatient_cat' & term == '0' ~ '  No',
                  model_interaction_var != 'visits_outpatient_cat' & term == '1' ~ '  Yes',
                  TRUE ~ term
            )) %>%
            filter(str_detect(model_interaction_var, subgroup)) %>%
            group_by(contrast) %>%
            forestplot(labeltext = c(term, model_p_value),
                       vertices = TRUE,
                       clip = c(0.1, 2.5),
                       xlog = TRUE,
                       align = "l",
                       xticks = c(log(0.1), log(0.5), log(1.0), log(1.5), log(3)),
                       xlab = 'Hazard Ratio',
                       plotwidth=unit(20, "cm"),
                       boxsize = .2) %>%
            fp_set_style(txt_gp = fpTxtGp(label = gpar(cex=0.75))) %>%
            fp_decorate_graph(graph.pos = 2) %>%
            fp_set_zebra_style("#f9f9f9") %>%
            fp_set_style(box = c("lightgray", "black") %>% 
                               lapply(function(x) gpar(fill = x, col = "#555555")),
                         default = gpar(vertices = TRUE))
      
      if(subgroup == 'gender|age'){
            f %>%
                  # Age and Gender
                  fp_insert_row('Age', position = 1, is.summary = T) %>%
                  fp_insert_row('Gender', position = 14, is.summary = T)
            
      } else if(subgroup == 'cancer_diagnosis_time|CCI_Metastatic'){
            f %>%
                  # Cancer Diagnosis Time and Metastatic Tumor
                  fp_insert_row('Cancer Diagnosis Time', position = 1, is.summary = T) %>%
                  fp_insert_row('Metastatic Tumor', position = 10, is.summary = T)
                  
      } else if(subgroup == 'cancer_dx'){
            f %>%
                  fp_set_style(txt_gp = fpTxtGp(label = gpar(cex=0.6))) %>%
                  fp_insert_row('Breast', position = 1, is.summary = T) %>%
                  fp_insert_row('Prostate', position = 4, is.summary = T) %>%
                  fp_insert_row('Colorectal', position = 7, is.summary = T) %>%
                  fp_insert_row('Lung', position = 10, is.summary = T) %>%
                  fp_insert_row('Head and Neck', position = 13, is.summary = T) %>%
                  fp_insert_row('Endometrium', position = 16, is.summary = T) %>%
                  fp_insert_row('Bladder', position = 19, is.summary = T) %>%
                  fp_insert_row('Biliary/HCC', position = 22, is.summary = T) %>%
                  fp_insert_row('Melanoma', position = 25, is.summary = T) %>%
                  fp_insert_row('Pancreas', position = 28, is.summary = T) %>%
                  fp_insert_row('Kidney', position = 31, is.summary = T) %>%
                  fp_insert_row('Gastric', position = 34, is.summary = T) %>%
                  fp_insert_row('Esophagus', position = 37, is.summary = T) %>%
                  fp_insert_row('Testicular', position = 40, is.summary = T) %>%
                  fp_insert_row('Thyroid', position = 43, is.summary = T) %>%
                  fp_insert_row('CNS', position = 46, is.summary = T) %>%
                  fp_insert_row('Neuroendocrine', position = 49, is.summary = T) %>%
                  fp_insert_row('Sarcomas', position = 52, is.summary = T) %>%
                  fp_insert_row('Leukemia', position = 55, is.summary = T) %>%
                  fp_insert_row('Myeloma', position = 58, is.summary = T) %>%
                  fp_insert_row('Lymphoma', position = 61, is.summary = T)
            
      } else if(subgroup == 'cancer_group'){
      
            f %>% 
                  fp_insert_row('Gastro-Intestinal', position = 1, is.summary = T) %>%
                  fp_insert_row('Genito-Urinary', position = 4, is.summary = T) %>%
                  fp_insert_row('Thyroid', position = 7, is.summary = T) %>%
                  fp_insert_row('Breast', position = 10, is.summary = T) %>%
                  fp_insert_row('Sarcomas', position = 13, is.summary = T) %>%
                  fp_insert_row('Thorax', position = 16, is.summary = T) %>%
                  fp_insert_row('Gynecology', position = 19, is.summary = T) %>%
                  fp_insert_row('Head-Neck', position = 22, is.summary = T) %>%
                  fp_insert_row('Hemathological', position = 25, is.summary = T) %>%
                  fp_insert_row('Skin', position = 28, is.summary = T)
            
      } else if(subgroup == 'covid_voc|vac_mRNA_12|visits_outpatient_cat' & 
                sum(table$model_interaction_var == 'vac_mRNA_12') >= 1) {
            
            f %>%
                  fp_insert_row('Number of Outpatient Visits', position = 1, is.summary = T) %>%
                  fp_insert_row('Variants of Concern (VOC)', position = 6, is.summary = T) %>%
                  fp_insert_row('Previous mRNA Vaccine Scheme', position = 9, is.summary = T)
            
      } else if(subgroup == 'covid_voc|vac_mRNA_12|visits_outpatient_cat' & 
                sum(table$model_interaction_var == 'vac_mRNA_12') == 0) { 
            
            f %>%
                  fp_insert_row('Variants of Concern (VOC)', position = 1, is.summary = T) %>%
                  fp_insert_row('Number of Outpatient Visits', position = 4, is.summary = T)
            
      } else if(subgroup == 'age_bin_65|gender|cancer_diagnosis_time_bin_0|CCI_Metastatic_Solid_Tumor|covid_voc|vac_mRNA_12' & 
                sum(table$model_interaction_var == 'vac_mRNA_12') >= 1) {
            
            f %>%
                  fp_insert_row('Age', position = 1, is.summary = T) %>%
                  fp_insert_row('Gender', position = 4, is.summary = T) %>%
                  fp_insert_row('Cancer Diagnosis Time', position = 7, is.summary = T) %>%
                  fp_insert_row('Metastatic Disease', position = 10, is.summary = T) %>%
                  fp_insert_row('Variant of Concern', position = 13, is.summary = T) %>%
                  fp_insert_row('Previous mRNA Vaccine Scheme', position = 16, is.summary = T)
            
      } else if(subgroup == 'age_bin_65|gender|cancer_diagnosis_time_bin_0|CCI_Metastatic_Solid_Tumor|covid_voc|vac_mRNA_12' & 
                sum(table$model_interaction_var == 'vac_mRNA_12') == 0) { 
            
            f %>%
                  fp_insert_row('Age', position = 1, is.summary = T) %>%
                  fp_insert_row('Gender', position = 4, is.summary = T) %>%
                  fp_insert_row('Variant of Concern', position = 7, is.summary = T) %>%
                  fp_insert_row('Cancer Diagnosis Time', position = 10, is.summary = T) %>%
                  fp_insert_row('Metastatic Disease', position = 13, is.summary = T) 
            
      }
      
}



for(subgroup_outcomes in names(files_subgroup_outcomes_forest)){
      for(analysis in names(all_analysis_forest)){
            
            file_forest <- paste(analysis, subgroup_outcomes, sep = '/')
            forest_table <- create_forest_table_subgroup(file_forest)
            
            pdf(paste0('Results Tidy/', 'forest_plots_subgroups_', files_subgroup_outcomes_forest[[subgroup_outcomes]], 
                       all_analysis_forest[[analysis]], '.pdf'))
            
            for(subgroup in subgroups_analysis){
                  
                  print(create_forest_plot_subgroup(forest_table, subgroup))
                  
            }
            
            dev.off()
      }
}

# Forest Plot of Main Results
#----- Tidying Main Results Analysis
create_forest_table_main_results <- function(files_forest){
      
      # Getting Files Results
      temp_tables <- lapply(files_forest , function(x) read.csv(x, row.names=NULL, header = T, sep = ';') %>% 
                                  mutate(analysis = files_main_results[[str_extract(x, '([^/]*)$')]], .before=term))
      
      # Binding Tables
      temp_table <- do.call(bind_rows, temp_tables)
      temp_table <- temp_table %>%
            separate(analysis, into = c('outcome', 'periods'), sep = '-') %>%
            mutate(term = str_replace(term, 'period', '')) %>%
            mutate(est.conf.interval = sprintf('%.2f (%.2f - %.2f)', estimate, conf.low, conf.high)) %>%
            mutate(estimate_ve = if_else(estimate>1, 
                                         -(1-(1/estimate))*100, (1-estimate)*100),
                   conf.low_ve = if_else(conf.low>1, 
                                         -(1-(1/conf.low))*100, (1-conf.low)*100),
                   conf.high_ve= if_else(conf.high>1, 
                                         -(1-(1/conf.high))*100, (1-conf.high)*100)) %>%
            mutate(est_ve.conf.interval = sprintf('%.1f%% (%.1f - %.1f)', estimate_ve, conf.high_ve, conf.low_ve))
      
      forest_table <- temp_table %>%
            select(outcome, periods,
                   term, estimate, conf.low, conf.high, p.value, est.conf.interval,
                   estimate_ve, conf.low_ve, conf.high_ve, est_ve.conf.interval) %>%
            rename(mean = estimate_ve,
                   lower = conf.high_ve,
                   upper = conf.low_ve) %>%
            mutate(p.value = sprintf('%.5f', p.value))
      
      
      return(forest_table)
}

create_forest_plot_main_results <- function(table){
      
      f <- table %>% 
            arrange(outcome) %>%
            mutate(term = case_when(
                  term == 'V1 0-14D' ~ '  0 - 13 days after dose one',
                  term == 'V1 14-59D' ~ '  14 - 59 days after dose one',
                  term == 'V1 60D+' ~ '  60 or more days after dose one',
                  term == 'V1V2 0-13D' ~ '  0 - 13 days after dose two',
                  term == 'V1V2 14-59D' ~ '  14 - 59 days after dose two',
                  term == 'V1V2 60-89D' ~ '  60 - 89 days after dose two',
                  term == 'V1V2 90-120D' ~ '  90 - 119 days after dose two',
                  term == 'V1V2 120D+' ~ '  120 or more days after dose two',
                  term == 'V1 14D+' ~ '  Partially Vaccinated',
                  term == 'V2 7D+' ~ '  Fully Vaccinated',

            )) %>%
            forestplot(labeltext = c(term, est_ve.conf.interval),
                       vertices = TRUE,
                       clip = c(-20, 100),
                       xlog = F,
                       zero = 0,
                       align = c("l", 'c'),
                       xticks = c(-20, 0, 50, 100),
                       xlab = 'Vaccine Effectiveness',
                       plotwidth=unit(20, "cm"),
                       boxsize = .1) %>%
            fp_set_style(txt_gp = fpTxtGp(label = gpar(cex=0.75))) %>%
            fp_decorate_graph(graph.pos = 2) %>%
            fp_set_zebra_style("#f9f9f9")
      
      if(sum(str_detect(table$term, 'V3')) >= 1){
            f %>%
                  fp_insert_row('Hospitalization', position = 1, is.summary = T) %>%
                  fp_insert_row('Death', position = 7, is.summary = T) %>%
                  fp_insert_row('Hospitalization or Death', position = 13, is.summary = T)
            
      } else if(sum(str_detect(table$term, 'V1')) >= 1){
            f %>%
                  fp_insert_row('Hospitalization', position = 1, is.summary = T) %>%
                  fp_insert_row('Death', position = 10, is.summary = T) %>%
                  fp_insert_row('Hospitalization or Death', position = 19, is.summary = T)
      }
      
}

all_analysis <- list(
      'Results/dose_12/rem_main_analysis',
      'Results/dose_12/sub_group_cancer_strict',
      'Results/dose_12/sub_group_covid_lab',
      'Results/dose_12/sub_group_hosp_3_days',
      'Results/dose_12/sub_group_tested_patients',
      'Results/dose_12/sub_group_not_jansen',
      'Results/dose_3/rem_main_analysis',
      'Results/dose_3/sub_group_cancer_strict',
      'Results/dose_3/sub_group_covid_lab',
      'Results/dose_3/sub_group_hosp_3_days',
      'Results/dose_3/sub_group_tested_patients')

files_main_results <- list(
      'outcome_hosp_period_all.csv' = '2 hospitalization',
      'outcome_death_period_all.csv' = '3 death',
      'outcome_hosp_death_period_all.csv' = '4 combined hosp death')

# Creating Regex for Loading File Paths
string_regex_results = paste0('(', paste(names(files_main_results), collapse = '|'), ')')

for(analysis in all_analysis){
      file_forest <- list.files(analysis, pattern = string_regex_results, full.names = T)
      forest_table <- create_forest_table_main_results(file_forest)
      
      pdf(paste0('Results Tidy/', 'forest_plots_main_results', str_replace_all(analysis, '(Results\\/|\\/)', '_'), '.pdf'))
 
      print(create_forest_plot_main_results(forest_table))
            
      dev.off()
}

#-- Forest Plot Sensitivity Analysis
create_forest_table_sens_results <- function(files_forest){
      
      # Getting Files Results
      temp_tables <- lapply(files_forest , function(x) read.csv(x, row.names=NULL, header = T, sep = ';') %>% 
                                  mutate(sens_analysis = 'sens-analysis') %>%
                                  bind_rows(read.csv(gsub('(.*\\/.*\\/).*(\\/.*)', '\\1rem_main_analysis\\2', x), row.names=NULL, header = T, sep = ';')) %>%
                                  mutate(sens_analysis = if_else(is.na(sens_analysis), 'main-analysis', sens_analysis)) %>%
                                  mutate(analysis = files_main_results[[str_extract(x, '([^/]*)$')]], .before=term))
      
      # Binding Tables
      temp_table <- do.call(bind_rows, temp_tables)
      temp_table <- temp_table %>%
            separate(analysis, into = c('outcome', 'periods'), sep = '-') %>%
            mutate(term = str_replace(term, 'period', '')) %>%
            mutate(est.conf.interval = sprintf('%.2f (%.2f - %.2f)', estimate, conf.low, conf.high))
      
      forest_table <- temp_table %>%
            select(outcome, sens_analysis,
                   term, estimate, conf.low, conf.high, p.value, est.conf.interval) %>%
            rename(mean = estimate,
                   lower = conf.low,
                   upper = conf.high) %>%
            mutate(p.value = sprintf('%.3f', p.value))
      
      
      return(forest_table)
}

create_forest_plot_sens_results <- function(table){
      
      forest_table %>%
            filter(outcome == '4 combined hosp death') %>%
            mutate(term = case_when(
                  term == 'V1 0-14D' ~ '  0 - 13 days after dose one',
                  term == 'V1 14-59D' ~ '  14 - 59 days after dose one',
                  term == 'V1 60D+' ~ '  60 or more days after dose one',
                  term == 'V1V2 0-13D' ~ '  0 - 13 days after dose two',
                  term == 'V1V2 14-59D' ~ '  14 - 59 days after dose two',
                  term == 'V1V2 60-89D' ~ '  60 - 89 days after dose two',
                  term == 'V1V2 90-120D' ~ '  90 - 119 days after dose two',
                  term == 'V1V2 120D+' ~ '  120 or more days after dose two',
                  term == 'V1 14D+' ~ '  Partially Vaccinated',
                  term == 'V2 7D+' ~ '  Fully Vaccinated',
                  term == 'V3 0-14D' ~ '  0 - 13 days after booster',
                  term == 'V3 14-28D' ~ '  14 - 27 days after booster',
                  term == 'V3 28-60D' ~ '  28 - 59 days after booster',
                  term == 'V3 60-120' ~ '  60 - 119 days after booster',
                  term == 'V3 120+' ~ '  120 or more days after booster',
                  term == 'V3 14-60D' ~ '  14 - 59 days after booster',
                  term == 'V3 60+' ~ '  60 or more days after booster',
            )) %>%
            mutate(term = factor(term, 
                                 levels = c(
                                       '  0 - 13 days after dose one', '  14 - 59 days after dose one', '  60 or more days after dose one',
                                       '  0 - 13 days after dose two', '  14 - 59 days after dose two', '  60 - 89 days after dose two',
                                       '  90 - 119 days after dose two', '  120 or more days after dose two', '  Partially Vaccinated',
                                       '  Fully Vaccinated', '  0 - 13 days after booster', '  14 - 27 days after booster',
                                       '  28 - 59 days after booster', '  60 - 119 days after booster', '  120 or more days after booster',
                                       '  14 - 59 days after booster', '  60 or more days after booster'))) %>%
            group_by(sens_analysis) %>%
            forestplot(labeltext = c(term),
                       vertices = TRUE,
                       clip = c(0.1, 2.5),
                       xlog = TRUE,
                       align = "l",
                       xticks = c(log(0.1), log(0.5), log(1.0), log(1.5), log(3)),
                       xlab = 'Hazard Ratio',
                       plotwidth=unit(20, "cm"),
                       boxsize = .2) %>%
            fp_set_style(txt_gp = fpTxtGp(label = gpar(cex=0.75))) %>%
            fp_decorate_graph(graph.pos = 2) %>%
            fp_set_zebra_style("#f9f9f9") %>%
            fp_set_style(box = c("red", "black") %>% 
                               lapply(function(x) gpar(fill = x, col = "#555555")),
                         default = gpar(vertices = TRUE)) %>%
            fp_insert_row('Hosp. or Death (Combined)', position = 1, is.summary = T)
      
}

all_sens_analysis <- list(
      # 'Results/dose_12/rem_main_analysis',
      'Results/dose_12/sub_group_cancer_strict',
      'Results/dose_12/sub_group_covid_lab',
      'Results/dose_12/sub_group_hosp_3_days',
      'Results/dose_12/sub_group_tested_patients',
      'Results/dose_12/sub_group_not_jansen',
      # 'Results/dose_3/rem_main_analysis',
      'Results/dose_3/sub_group_cancer_strict',
      'Results/dose_3/sub_group_covid_lab',
      'Results/dose_3/sub_group_hosp_3_days',
      'Results/dose_3/sub_group_tested_patients')

files_sens_results <- list(
      'outcome_covid_period_all.csv' = '1 infection',
      'outcome_covid_period_three.csv' = '1 infection',
      'outcome_hosp_period_all.csv' = '2 hospitalization',
      'outcome_hosp_period_three.csv' = '2 hospitalization',
      'outcome_death_period_all.csv' = '3 death',
      'outcome_death_period_three.csv' = '3 death',
      'outcome_hosp_death_period_all.csv' = '4 combined hosp death',
      'outcome_hosp_death_period_three.csv' = '4 combined hosp death')

# Creating Regex for Loading File Paths
string_regex_results = paste0('(', paste(names(files_sens_results), collapse = '|'), ')')

for(analysis in all_sens_analysis){
      file_forest <- list.files(analysis, pattern = string_regex_results, full.names = T)
      forest_table <- create_forest_table_sens_results(file_forest)
      
      pdf(paste0('Results Tidy/', 'forest_plots_sens_results', str_replace_all(analysis, '(Results\\/|\\/)', '_'), '.pdf'),
          width = 8, height = 4)
      
      print(create_forest_plot_sens_results(forest_table))
      
      dev.off()
}





# CODE FOR TESTING PLOTS - Main Results Plots
file_forest <- list.files('Results/dose_12/rem_main_analysis', pattern = string_regex_results, full.names = T)
forest_table <- create_forest_table_main_results(file_forest)

table(forest_table$outcome == 'infection ')

forest_table %>% 
      arrange(outcome) %>%
      mutate(term = case_when(
            term == 'V1 0-14D' ~ '  0 - 13 days after dose one',
            term == 'V1 14-59D' ~ '  14 - 59 days after dose one',
            term == 'V1 60D+' ~ '  60 or more days after dose one',
            term == 'V1V2 0-13D' ~ '  0 - 13 days after dose two',
            term == 'V1V2 14-59D' ~ '  14 - 59 days after dose two',
            term == 'V1V2 60-89D' ~ '  60 - 89 days after dose two',
            term == 'V1V2 90-120D' ~ '  90 - 119 days after dose two',
            term == 'V1V2 120D+' ~ '  120 or more days after dose two',
            term == 'V1 14D+' ~ '  Partially Vaccinated',
            term == 'V2 7D+' ~ '  Fully Vaccinated',
            term == 'V3 0-14D' ~ '  0 - 13 days after booster',
            term == 'V3 14-28D' ~ '  14 - 27 days after booster',
            term == 'V3 28-60D' ~ '  28 - 59 days after booster',
            term == 'V3 60-120' ~ '  60 - 119 days after booster',
            term == 'V3 120+' ~ '  120 or more days after booster',
            term == 'V3 14-60D' ~ '  14 - 59 days after booster',
            term == 'V3 60+' ~ '  60 or more days after booster',
      )) %>%
      forestplot(labeltext = c(term, est_ve.conf.interval),
                 vertices = TRUE,
                 clip = c(-20, 100),
                 xlog = F,
                 zero = 0,
                 align = c("l", 'c'),
                 xticks = c(-20, 0, 50, 100),
                 xlab = 'Vaccine Effectiveness',
                 plotwidth=unit(20, "cm"),
                 boxsize = .1) %>%
      fp_set_style(txt_gp = fpTxtGp(label = gpar(cex=0.75))) %>%
      fp_decorate_graph(graph.pos = 2) %>%
      fp_set_zebra_style("#f9f9f9") %>%
      fp_insert_row('Hospitalization', position = 1, is.summary = T) %>%
      fp_insert_row('Death', position = 10, is.summary = T) %>%
      fp_insert_row('Hospitalization or Death', position = 19, is.summary = T)


files_subgroup_outcomes_forest <- list(
      'subgroup_outcome_hosp_death_three_periods.csv' = 'hosp_death')

all_analysis_forest <- list(
      'Results/dose_12/rem_main_analysis' = '_dose_12_rem_main_analysis',
      'Results/dose_3/rem_main_analysis' = '_dose_3_rem_main_analysis')




# Creating Final Sub-Group Graphs
subgroups_analysis <- list(
      'gender|age',
      'cancer_diagnosis_time|CCI_Metastatic',
      'cancer_dx',
      'cancer_group',
      'covid_voc|vac_mRNA_12|visits_outpatient_cat',
      'age_bin_65|gender|cancer_diagnosis_time_bin_0|CCI_Metastatic_Solid_Tumor|covid_voc|vac_mRNA_12',
      'age_bin_65|gender|cancer_diagnosis_time_bin_0|CCI_Metastatic_Solid_Tumor|cancer_dx_lung|cancer_group_hemathological|covid_voc|vac_mRNA_12'
)

file_forest <- paste('Results/dose_12/rem_main_analysis', 
                     'subgroup_outcome_hosp_death_three_periods.csv', sep = '/')

forest_table <- create_forest_table_subgroup(file_forest)

subgroup <- subgroups_analysis[[7]]

forest_sg_12 <- forest_table %>% 
      mutate(term = case_when(
            term == 'Age < 60' ~ '  < 60 years',
            term == 'Age < 65' ~ '  < 65 years',
            model_interaction_var == 'age_bin_70' & term == 'Age < 75' ~ '  < 70 years',
            model_interaction_var == 'age_bin_80' & term == 'Age < 75' ~ '  < 80 years',
            term == 'Age < 70' ~ '  < 75 years',
            term == 'Age < 80' ~ '  < 85 years',
            term == 'Age 60+' ~ '  ≥ 60 years',
            term == 'Age 65+' ~ '  ≥ 65 years',
            term == 'Age 70+' ~ '  ≥ 70 years',
            term == 'Age 75+' ~ '  ≥ 75 years',
            term == 'Age 80+' ~ '  ≥ 80 years',
            term == 'Age 85+' ~ '  ≥ 85 years',
            term == 'Other VOC' ~ '  Other',
            term == 'Delta VOC' ~ '  Delta',
            term == 'Omicron VOC' ~ '  Omicron',
            term == 'M' ~ '  Male',
            term == 'F' ~ '  Female',
            term == '< 1y' ~ '  < 1 year',
            term == '< 2y' ~ '  < 2 years',
            term == '< 3y' ~ '  < 3 years',
            term == '< 4y' ~ '  < 4 years',
            term == '1-5yr' ~ '  1 - 5 years',
            term == '2-5yr' ~ '  2 - 5 years',
            term == '3-5yr' ~ '  3 - 5 years',
            term == '4-5yr' ~ '  4 - 5 years',
            model_interaction_var == 'visits_outpatient_cat' & term == '0' ~ '  0',
            model_interaction_var == 'visits_outpatient_cat' & term == '1' ~ '  1',
            model_interaction_var == 'visits_outpatient_cat' & term == '2' ~ '  2',
            model_interaction_var == 'visits_outpatient_cat' & term == '3+' ~ '  ≥ 3',
            model_interaction_var != 'visits_outpatient_cat' & term == '0' ~ '  No',
            model_interaction_var != 'visits_outpatient_cat' & term == '1' ~ '  Yes',
            TRUE ~ term
      )) %>%
      filter(str_detect(model_interaction_var, subgroup)) %>%
      arrange(match(model_interaction_var, c('age_bin_65', 'gender_concept_id', 'cancer_diagnosis_time_bin_0',
                                             'CCI_Metastatic_Solid_Tumor', 'cancer_dx_lung', 'cancer_group_hemathological',
                                             'covid_voc', 'vac_mRNA_12'))) %>%
      group_by(contrast) %>%
      forestplot(labeltext = c(term, model_p_value),
                 vertices = TRUE,
                 clip = c(-20, 100),
                 xlog = F,
                 zero = 0,
                 align = c("l", 'l', 'c'),
                 xticks = c(-20, 0, 50, 100),
                 xlab = 'Vaccine Effectiveness',
                 plotwidth=unit(20, "cm"),
                 boxsize = .1) %>%
      fp_set_style(txt_gp = fpTxtGp(label = gpar(cex=0.60))) %>%
      fp_decorate_graph(graph.pos = 2) %>%
      fp_set_zebra_style("#f9f9f9") %>%
      fp_set_style(box = c("lightgray", "black") %>% 
                         lapply(function(x) gpar(fill = x, col = "#555555")),
                   default = gpar(vertices = TRUE)) %>%
      fp_insert_row(term = 'Age',  position = 1, is.summary = F) %>%
      fp_insert_row(term = 'Gender',  position = 4, is.summary = F) %>%
      fp_insert_row(term = 'Cancer Diagnosis Time',  position = 7, is.summary = F) %>%
      fp_insert_row(term = 'Metastatic Disease',  position = 10, is.summary = F) %>%
      fp_insert_row(term = 'Lung Cancer',  position = 13, is.summary = F) %>%
      fp_insert_row(term = 'Hemathological Cancer',  position = 16, is.summary = F) %>%
      fp_insert_row(term = 'Variant of Concern',  position = 19, is.summary = F)


file_forest <- paste('Results/dose_3/rem_main_analysis', 
                     'subgroup_outcome_hosp_death_three_periods.csv', sep = '/')

forest_table <- create_forest_table_subgroup(file_forest)

subgroup <- subgroups_analysis[[7]]

forest_sg_3 <- forest_table %>% 
      mutate(term = case_when(
            term == 'Age < 60' ~ '  < 60 years',
            term == 'Age < 65' ~ '  < 65 years',
            model_interaction_var == 'age_bin_70' & term == 'Age < 75' ~ '  < 70 years',
            model_interaction_var == 'age_bin_80' & term == 'Age < 75' ~ '  < 80 years',
            term == 'Age < 70' ~ '  < 75 years',
            term == 'Age < 80' ~ '  < 85 years',
            term == 'Age 60+' ~ '  ≥ 60 years',
            term == 'Age 65+' ~ '  ≥ 65 years',
            term == 'Age 70+' ~ '  ≥ 70 years',
            term == 'Age 75+' ~ '  ≥ 75 years',
            term == 'Age 80+' ~ '  ≥ 80 years',
            term == 'Age 85+' ~ '  ≥ 85 years',
            term == 'Other VOC' ~ '  Other',
            term == 'Delta VOC' ~ '  Delta',
            term == 'Omicron VOC' ~ '  Omicron',
            term == 'M' ~ '  Male',
            term == 'F' ~ '  Female',
            term == '< 1y' ~ '  < 1 year',
            term == '< 2y' ~ '  < 2 years',
            term == '< 3y' ~ '  < 3 years',
            term == '< 4y' ~ '  < 4 years',
            term == '1-5yr' ~ '  1 - 5 years',
            term == '2-5yr' ~ '  2 - 5 years',
            term == '3-5yr' ~ '  3 - 5 years',
            term == '4-5yr' ~ '  4 - 5 years',
            model_interaction_var == 'visits_outpatient_cat' & term == '0' ~ '  0',
            model_interaction_var == 'visits_outpatient_cat' & term == '1' ~ '  1',
            model_interaction_var == 'visits_outpatient_cat' & term == '2' ~ '  2',
            model_interaction_var == 'visits_outpatient_cat' & term == '3+' ~ '  ≥ 3',
            model_interaction_var != 'visits_outpatient_cat' & term == '0' ~ '  No',
            model_interaction_var != 'visits_outpatient_cat' & term == '1' ~ '  Yes',
            TRUE ~ term
      )) %>%
      filter(str_detect(model_interaction_var, subgroup)) %>%
      arrange(match(model_interaction_var, c('age_bin_65', 'gender_concept_id', 'cancer_diagnosis_time_bin_0',
                                             'CCI_Metastatic_Solid_Tumor', 'cancer_dx_lung', 'cancer_group_hemathological',
                                             'covid_voc', 'vac_mRNA_12'))) %>%
      group_by(contrast) %>%
      forestplot(labeltext = c(term, model_p_value),
                 vertices = TRUE,
                 clip = c(-20, 100),
                 xlog = F,
                 zero = 0,
                 align = c("l", 'l', 'c'),
                 xticks = c(-20, 0, 50, 100),
                 xlab = 'Vaccine Effectiveness',
                 plotwidth=unit(20, "cm"),
                 boxsize = .1) %>%
      fp_set_style(txt_gp = fpTxtGp(label = gpar(cex=0.60))) %>%
      fp_decorate_graph(graph.pos = 2) %>%
      fp_set_zebra_style("#f9f9f9") %>%
      fp_set_style(box = c("lightgray", "black") %>% 
                         lapply(function(x) gpar(fill = x, col = "#555555")),
                   default = gpar(vertices = TRUE)) %>%
      fp_insert_row(term = 'Age',  position = 1, is.summary = F) %>%
      fp_insert_row(term = 'Gender',  position = 4, is.summary = F) %>%
      fp_insert_row(term = 'Cancer Diagnosis Time',  position = 7, is.summary = F) %>%
      fp_insert_row(term = 'Metastatic Disease',  position = 10, is.summary = F) %>%
      fp_insert_row(term = 'Lung Cancer',  position = 13, is.summary = F) %>%
      fp_insert_row(term = 'Hemathological Cancer',  position = 16, is.summary = F) %>%
      fp_insert_row(term = 'Variant of Concern',  position = 19, is.summary = F) %>%
      fp_insert_row(term = 'Previous mRNA Vaccine',  position = 22, is.summary = F)


library(grid)
pdf('Results Tidy/forest_plot_subgroups_combined_11062023.pdf', width = 9, height = 5)
grid.newpage()
borderWidth <- unit(1, "pt")
width <- unit(convertX(unit(1, "npc") - borderWidth, unitTo = "npc", valueOnly = TRUE)/2, "npc")
pushViewport(viewport(layout = grid.layout(nrow = 1, 
                                           ncol = 3, 
                                           widths = unit.c(width,
                                                           borderWidth,
                                                           width))))
pushViewport(viewport(layout.pos.row = 1,
                      layout.pos.col = 1))

forest_sg_12

upViewport()
pushViewport(viewport(layout.pos.row = 1,
                      layout.pos.col = 2))
grid.rect(gp = gpar(fill = "white", col = "white"))
upViewport()
pushViewport(viewport(layout.pos.row = 1,
                      layout.pos.col = 3))
forest_sg_3
upViewport(2)
dev.off()
# fp_insert_row('Age', position = 1, is.summary = T) %>%
# fp_insert_row('Gender', position = 4, is.summary = T) %>%
# fp_insert_row('Cancer Diagnosis Time', position = 7, is.summary = T) %>%
# fp_insert_row('Metastatic Disease', position = 10, is.summary = T) %>%
# fp_insert_row('Variant of Concern', position = 13, is.summary = T)


# fp_insert_row('Age', position = 1, is.summary = T) %>%
# fp_insert_row('Gender', position = 4, is.summary = T) %>%
# fp_insert_row('Cancer Diagnosis Time', position = 7, is.summary = T) %>%
# fp_insert_row('Metastatic Disease', position = 10, is.summary = T) %>%
# fp_insert_row('Variant of Concern', position = 13, is.summary = T) %>%
# fp_insert_row('Previous mRNA Vaccine Scheme', position = 16, is.summary = T)      


# fp_insert_row('Number of Outpatient Visits', position = 1, is.summary = T) %>%
# fp_insert_row('Variants of Concern (VOC)', position = 6, is.summary = T) %>%
# fp_insert_row('Previous mRNA Vaccine Scheme', position = 9, is.summary = T)

# fp_insert_row('Variants of Concern (VOC)', position = 1, is.summary = T) %>%
# fp_insert_row('Number of Outpatient Visits', position = 4, is.summary = T)
      
# fp_insert_row('Gastro-Intestinal', position = 1, is.summary = T) %>%
# fp_insert_row('Genito-Urinary', position = 4, is.summary = T) %>%
# fp_insert_row('Thyroid', position = 7, is.summary = T) %>%
# fp_insert_row('Breast', position = 10, is.summary = T) %>%
# fp_insert_row('Sarcomas', position = 13, is.summary = T) %>%
# fp_insert_row('Thorax', position = 16, is.summary = T) %>%
# fp_insert_row('Gynecology', position = 19, is.summary = T) %>%
# fp_insert_row('Head-Neck', position = 22, is.summary = T) %>%
# fp_insert_row('Hemathological', position = 25, is.summary = T) %>%
# fp_insert_row('Skin', position = 28, is.summary = T)


# fp_insert_row('Breast', position = 1, is.summary = T) %>%
# fp_insert_row('Prostate', position = 4, is.summary = T) %>%
# fp_insert_row('Colorectal', position = 7, is.summary = T) %>%
# fp_insert_row('Lung', position = 10, is.summary = T) %>%
# fp_insert_row('Head and Neck', position = 13, is.summary = T) %>%
# fp_insert_row('Endometrium', position = 16, is.summary = T) %>%
# fp_insert_row('Bladder', position = 19, is.summary = T) %>%
# fp_insert_row('Biliary/HCC', position = 22, is.summary = T) %>%
# fp_insert_row('Melanoma', position = 25, is.summary = T) %>%
# fp_insert_row('Pancreas', position = 28, is.summary = T) %>%
# fp_insert_row('Kidney', position = 31, is.summary = T) %>%
# fp_insert_row('Gastric', position = 34, is.summary = T) %>%
# fp_insert_row('Esophagus', position = 37, is.summary = T) %>%
# fp_insert_row('Testicular', position = 40, is.summary = T) %>%
# fp_insert_row('Thyroid', position = 43, is.summary = T) %>%
# fp_insert_row('CNS', position = 46, is.summary = T) %>%
# fp_insert_row('Neuroendocrine', position = 49, is.summary = T) %>%
# fp_insert_row('Sarcomas', position = 52, is.summary = T) %>%
# fp_insert_row('Leukemia', position = 55, is.summary = T) %>%
# fp_insert_row('Myeloma', position = 58, is.summary = T) %>%
# fp_insert_row('Lymphoma', position = 61, is.summary = T)
      

# Cancer Diagnosis Time and Metastatic Tumor
# fp_insert_row('Cancer Diagnosis Time', position = 1, is.summary = T) %>%
# fp_insert_row('Metastatic Tumor', position = 10, is.summary = T)

# Age and Gender
# fp_insert_row('Age', position = 1, is.summary = T) %>%
# fp_insert_row('Gender', position = 14, is.summary = T)



# SMD - Graphic
all_analysis <- list(
      'Results/dose_12/rem_main_analysis',
      'Results/dose_12/sub_group_cancer_strict',
      'Results/dose_12/sub_group_covid_lab',
      'Results/dose_12/sub_group_hosp_3_days',
      'Results/dose_12/sub_group_tested_patients',
      'Results/dose_12/sub_group_not_jansen',
      'Results/dose_3/rem_main_analysis',
      'Results/dose_3/sub_group_cancer_strict',
      'Results/dose_3/sub_group_covid_lab',
      'Results/dose_3/sub_group_hosp_3_days',
      'Results/dose_3/sub_group_tested_patients')
