# ============================================================================ #
# 16. REM Analysis - Article Graphs                                            #
# Author: Felippe Lazar, IDIAP Jordi Gol, 2023 #
# ============================================================================ #

library(tidyverse)
library(here)
library(readxl)
library(ggplot2)
library(lubridate)
require(grid)
library(gridExtra)
library(egg)
library(glue)
library(forestplot)
library(ggsurvfit)
source('utils_tidying.R')

# Setting WD
# mainWD <- '/Users/felippelazarneto/Google Drive (felippe.neto@alumni.usp.br)/SIDIAP Analysis/'

# Figure 1 - Combination of COVID-19 Incidence, COVID-19 VOCs and Vaccine Rollout
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
g_voc <- covidVOC %>%
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
      distinct(week_variant, name_variant, .keep_all=T) %>% 
      filter(date_variant >= dmy('27/12/2020')) %>%
      filter(date_variant <= dmy('30/06/2022')) %>%
      mutate(date_variant = floor_date(date_variant, "weeks", week_start = 1)) %>%
      mutate(name_variant = factor(name_variant, levels = c('Other VOC', 'Delta VOC', 'Omicron VOC'))) %>%
      ggplot(aes(x = as.Date(date_variant), y = prop_week_variant, fill = name_variant)) + 
      geom_col(position = 'fill', width = 8) + theme_bw() + 
      theme(legend.position = 'top') + 
      labs(x = '', y = 'weekly proportion (%)', fill = '') + 
      scale_fill_manual(values = c('#B1746F','#FFB547', '#725663'), 
                        labels = c('Ancestor', 'Delta', 'Omicron')) 

# Cumulative Vaccine Rollout
vac_rollout <- read.table('Results/descriptive_until_2022/vacc_cum_rollout_by_dose.csv', 
                          sep = ';', header = T)

g_vac_rollout <- vac_rollout %>%
      mutate(vac_exposure_date = ymd(vac_exposure_date)) %>%
      mutate(vac_number = factor(vac_number, levels = c('Dose One', 'Dose Two', 'Booster'))) %>%
      ggplot(aes(x = vac_exposure_date, y = cum_sum, fill = vac_number)) + 
      geom_area(alpha = 0.8, position = 'identity', color = 'black') + theme_bw() + 
      theme(legend.position = 'top') + ylim(0, 1) + 
      scale_fill_manual(values = c('#996136', '#D9AF98', '#F2DACE')) +
      labs(y = 'proportion of vaccinated individuals (%)', x = '', fill = '')

# Cumulative COVID-19
covid_cases <- read.table('Results/descriptive_until_2022/covid_frequency.csv', 
                          sep = ';', header = T)

g_covid <- covid_cases %>%
      mutate(covid_date = ymd(covid_date)) %>%
      ggplot(aes(x = covid_date, y = n)) + geom_area(fill = '#996136') +
      labs(x = '', y = 'number of COVID-19 cases (N)', fill = '') + theme_bw() + 
      theme(legend.position = 'none')

tt <- egg::ggarrange(g_covid, g_voc, g_vac_rollout, ncol = 1,
              labels = c('A', 'B', 'C'),
              label.args = list(gp=gpar(fontface='bold', fontsize=20), x=unit(2,"line"), hjust=-0.5, vjust=2))

ggsave("Figures/figure_covid_vax.png", plot = tt, height = 260, width = 2*260/3, units = "mm", dpi = "print")

# Forest Plots
library(meta)
library(forestplot)

# Forest Table 1st and 2nd Dose
file_forest <- paste('Results/dose_12/rem_main_analysis', 'subgroup_outcome_hosp_death_three_periods.csv', sep = '/')
forest_table <- create_forest_table_subgroup(file_forest)

subgroup <- 'age_bin_65|gender|cancer_diagnosis_time_bin_0|CCI_Metastatic_Solid_Tumor|cancer_dx_lung|cancer_group_hemathological|covid_voc|vac_mRNA_12'

forest_sg_12 <- forest_table %>% 
      filter(str_detect(model_interaction_var, subgroup)) %>%
      mutate(term = case_when(
            term == 'Age < 65' ~ '  < 65 years',
            term == 'Age 65+' ~ '  >= 65 years',
            term == 'Other VOC' ~ '  Other',
            term == 'Delta VOC' ~ '  Delta',
            term == 'Omicron VOC' ~ '  Omicron',
            term == 'M' ~ '  Male',
            term == 'F' ~ '  Female',
            term == '< 1y' ~ '  < 1 year',
            term == '1-5yr' ~ '  1 - 5 years',
            model_interaction_var != 'visits_outpatient_cat' & term == '0' ~ '  No',
            model_interaction_var != 'visits_outpatient_cat' & term == '1' ~ '  Yes',
            TRUE ~ term
      ))  %>%
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
      fp_set_style(txt_gp = fpTxtGp(label = gpar(cex=0.4),
                                    xlab  = gpar(cex = 0.5),
                                    ticks = gpar(cex = 0.3))) %>%
      fp_decorate_graph(graph.pos = 2) %>%
      fp_set_zebra_style("#f9f9f9") %>%
      fp_set_style(box = c("black", "chocolate") %>% 
                         lapply(function(x) gpar(fill = x, col = x)),
                   default = gpar(vertices = TRUE)) %>%
      fp_insert_row(term = 'Age',  position = 1, is.summary = F) %>%
      fp_insert_row(term = 'Sex',  position = 4, is.summary = F) %>%
      fp_insert_row(term = 'Cancer Diagnosis Time',  position = 7, is.summary = F) %>%
      fp_insert_row(term = 'Metastatic Disease',  position = 10, is.summary = F) %>%
      fp_insert_row(term = 'Lung Cancer',  position = 13, is.summary = F) %>%
      fp_insert_row(term = 'Hemathological Cancer',  position = 16, is.summary = F) %>%
      fp_insert_row(term = 'Variant of Concern',  position = 19, is.summary = F) %>%
      fp_insert_row(term = 'A. Primary Vaccination Scheme',  position = 1, is.summary = T)

file_forest <- paste('Results/dose_3/rem_main_analysis', 'subgroup_outcome_hosp_death_three_periods.csv', sep = '/')
forest_table <- create_forest_table_subgroup(file_forest)

forest_sg_3 <- forest_table %>% 
      mutate(term = case_when(
            term == 'Age < 65' ~ '  < 65 years',
            term == 'Age 65+' ~ '  >= 65 years',
            term == 'Other VOC' ~ '  Other',
            term == 'Delta VOC' ~ '  Delta',
            term == 'Omicron VOC' ~ '  Omicron',
            term == 'M' ~ '  Male',
            term == 'F' ~ '  Female',
            term == '< 1y' ~ '  < 1 year',
            term == '1-5yr' ~ '  1 - 5 years',
            model_interaction_var != 'visits_outpatient_cat' & term == '0' ~ '  No',
            model_interaction_var != 'visits_outpatient_cat' & term == '1' ~ '  Yes',
            TRUE ~ term
      ))  %>%
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
      fp_set_style(txt_gp = fpTxtGp(label = gpar(cex=0.4),
                                    xlab  = gpar(cex = 0.5),
                                    ticks = gpar(cex = 0.3))) %>%
      fp_decorate_graph(graph.pos = 2) %>%
      fp_set_zebra_style("#f9f9f9") %>%
      fp_set_style(box = c("black", "chocolate") %>% 
                         lapply(function(x) gpar(fill = x, col = x)),
                   default = gpar(vertices = TRUE)) %>%
      fp_insert_row(term = 'Age',  position = 1, is.summary = F) %>%
      fp_insert_row(term = 'Sex',  position = 4, is.summary = F) %>%
      fp_insert_row(term = 'Cancer Diagnosis Time',  position = 7, is.summary = F) %>%
      fp_insert_row(term = 'Metastatic Disease',  position = 10, is.summary = F) %>%
      fp_insert_row(term = 'Lung Cancer',  position = 13, is.summary = F) %>%
      fp_insert_row(term = 'Hemathological Cancer',  position = 16, is.summary = F) %>%
      fp_insert_row(term = 'Variant of Concern',  position = 19, is.summary = F) %>%
      fp_insert_row(term = 'Previous mRNA Vaccine',  position = 22, is.summary = F) %>%
      fp_insert_row(term = 'B. Booster Vaccination',  position = 1, is.summary = T)

# Exporting Combined Graph
pdf('Figures/forest_plot_subgroups_combined.pdf', width = 6, height = 4)
grid.newpage()
# Creating Layout
gridView <- viewport(layout = grid.layout(nrow = 1, ncol = 2))
pushViewport(gridView)

# Creating Plot 1
plotOne <- viewport(layout.pos.row = 1, layout.pos.col = 1)
pushViewport(plotOne)
forest_sg_12
upViewport()

# Creating Plot 2
plotTwo <- viewport(layout.pos.row = 1,layout.pos.col = 2)
pushViewport(plotTwo)
forest_sg_3
upViewport(2)
dev.off()

# Ploting Main Results
files_main_results <- list(
      'outcome_hosp_period_three.csv' = '2 hospitalization',
      'outcome_hosp_severe_period_three.csv' = '3 severe hospitalization',
      'outcome_death_period_three.csv' = '4 death',
      'outcome_hosp_death_period_three.csv' = '51 combined hosp death',
      'outcome_hosp_death_period_all.csv' = '52 combined hosp death')

string_regex_results = paste0('(', paste(names(files_main_results), collapse = '|'), ')')

file_forest <- list.files('Results/dose_12/rem_main_analysis', pattern = string_regex_results, full.names = T)
forest_table <- create_forest_table_main_results(file_forest)

graph_main_12 <- 
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
            term == 'no-vax' ~ '  Unvaccinated',
      )) %>%
      filter(!(term == '  0 - 13 days after dose one' & outcome != '52 combined hosp death')) %>%
      mutate(est_ve.conf.interval = ifelse(mean == 0, 'Reference', est_ve.conf.interval)) %>% 
      forestplot(labeltext = c(term, exposure, n_event, est_ve.conf.interval),
                 vertices = TRUE,
                 clip = c(-20, 100),
                 xlog = F,
                 zero = 0,
                 align = c("l", 'c', 'c', 'c'),
                 xticks = c(-20, 0, 50, 100),
                 xlab = 'VE',
                 boxsize = .1) %>%
      fp_set_style(txt_gp = fpTxtGp(label = gpar(cex=0.4),
                                    xlab  = gpar(cex = 0.4),
                                    ticks = gpar(cex = 0.3))) %>%
      fp_decorate_graph(graph.pos = 4) %>%
      fp_set_zebra_style("#f9f9f9") %>%
      fp_insert_row(term = 'COVID-19 Hospitalization', 
                    exposure = c('Person-Days'),
                    n_event = 'No Events', 
                    est_ve.conf.interval = 'VE',
                    position = 1, is.summary = T) %>%
      fp_insert_row('COVID-19 Severe Hospitalization', position = 5, is.summary = T) %>%
      fp_insert_row('COVID-19 Death', position = 9, is.summary = T) %>%
      fp_insert_row('COVID-19 Hosp. or Death', position = 13, is.summary = T) %>%
      fp_insert_row(term = 'COVID-19 Hosp. or Death - All Periods', position = 17, is.summary = T) %>%
      fp_add_header(term = 'PRIMARY VACCINATION SCHEME',  position = 1, is.summary = T) %>%
      fp_add_lines("lightgray")

pdf('Figures/forest_plot_main_results_12.pdf', width = 5, height = 5)
graph_main_12 
dev.off()

# Figure Main Results Dose 3
file_forest <- list.files('Results/dose_3/rem_main_analysis', pattern = string_regex_results, full.names = T)
forest_table <- create_forest_table_main_results(file_forest)

graph_main_3 <- 
      forest_table %>% 
      arrange(outcome) %>%
      mutate(term = case_when(
            term == 'V3 0-14D' ~ '  0 - 13 days after booster',
            term == 'V3 14-28D' ~ '  14 - 27 days after booster',
            term == 'V3 28-60D' ~ '  28 - 59 days after booster',
            term == 'V3 60-120' ~ '  60 - 119 days after booster',
            term == 'V3 120+' ~ '  120 or more days after booster',
            term == 'V3 14-60D' ~ '  14 - 59 days after booster',
            term == 'V3 60+' ~ '  60 or more days after booster',
            term == 'no-vax' ~ '  Un-boosted',
      )) %>%
      mutate(est_ve.conf.interval = ifelse(mean == 0, 'Reference', est_ve.conf.interval)) %>%
      filter(!(term == '  0 - 13 days after booster' & outcome != '52 combined hosp death')) %>%
      forestplot(labeltext = c(term, exposure, n_event, est_ve.conf.interval),
                 vertices = TRUE,
                 clip = c(-20, 100),
                 xlog = F,
                 zero = 0,
                 align = c("l", 'c', 'c', 'c'),
                 xticks = c(-20, 0, 50, 100),
                 xlab = 'rVE',
                 boxsize = .1) %>%
      fp_set_style(txt_gp = fpTxtGp(label = gpar(cex=0.4),
                                    xlab  = gpar(cex = 0.4),
                                    ticks = gpar(cex = 0.3))) %>%
      fp_decorate_graph(graph.pos = 4) %>%
      fp_set_zebra_style("#f9f9f9") %>%
      fp_insert_row(term = 'COVID-19 Hospitalization', 
                    exposure = 'Person-Days',
                    n_event = 'No Events', 
                    est_ve.conf.interval = 'rVE',
                    position = 1, is.summary = T) %>%
      fp_insert_row('COVID-19 Severe Hospitalization', position = 5, is.summary = T) %>%
      fp_insert_row('COVID-19 Death', position = 9, is.summary = T) %>%
      fp_insert_row('COVID-19 Hosp. or Death', position = 13, is.summary = T) %>%
      fp_insert_row('COVID-19 Hosp. or Death - All Periods', position = 17, is.summary = T) %>%
      fp_add_header(term = 'BOOSTER VACCINATION',  position = 1, is.summary = T) %>%
      fp_add_lines("lightgray")

pdf('Figures/forest_plot_main_results_3.pdf', width = 5, height = 4.5)
graph_main_3
dev.off()

# # Plotting Both Together Main Results
# file_forest_rem_12 <- list.files('Results/dose_12/rem_main_analysis', pattern = string_regex_results, full.names = T)
# forest_table_rem_12 <- create_forest_table_main_results(file_forest_rem_12)
# file_forest_rem_3 <- list.files('Results/dose_3/rem_main_analysis', pattern = string_regex_results, full.names = T)
# forest_table_rem_3 <- create_forest_table_main_results(file_forest_rem_3)
# 
# forest_table <- bind_rows(forest_table_rem_12 %>% arrange(outcome), forest_table_rem_3 %>% arrange(outcome))
# 
# graph_main_123 <- forest_table %>% 
#       mutate(term = case_when(
#             term == 'V1 0-14D' ~ '  0 - 13 days after dose one',
#             term == 'V1 14-59D' ~ '  14 - 59 days after dose one',
#             term == 'V1 60D+' ~ '  60 or more days after dose one',
#             term == 'V1V2 0-13D' ~ '  0 - 13 days after dose two',
#             term == 'V1V2 14-59D' ~ '  14 - 59 days after dose two',
#             term == 'V1V2 60-89D' ~ '  60 - 89 days after dose two',
#             term == 'V1V2 90-120D' ~ '  90 - 119 days after dose two',
#             term == 'V1V2 120D+' ~ '  120 or more days after dose two',
#             term == 'V1 14D+' ~ '  Partially Vaccinated',
#             term == 'V2 7D+' ~ '  Fully Vaccinated',
#             term == 'V3 0-14D' ~ '  0 - 13 days after booster',
#             term == 'V3 14-28D' ~ '  14 - 27 days after booster',
#             term == 'V3 28-60D' ~ '  28 - 59 days after booster',
#             term == 'V3 60-120' ~ '  60 - 119 days after booster',
#             term == 'V3 120+' ~ '  120 or more days after booster',
#             term == 'V3 14-60D' ~ '  14 - 59 days after booster',
#             term == 'V3 60+' ~ '  60 or more days after booster',
#             term == 'no-vax' ~ '  Unvaccinated',
#       )) %>%
#       mutate(est_ve.conf.interval = ifelse(mean == 0, 'Reference', est_ve.conf.interval)) %>%
#       forestplot(labeltext = c(term, n_event, est_ve.conf.interval),
#                  vertices = TRUE,
#                  clip = c(-20, 100),
#                  xlog = F,
#                  zero = 0,
#                  align = c("l", 'c'),
#                  xticks = c(-20, 0, 50, 100),
#                  xlab = 'Vaccine Effectiveness',
#                  boxsize = .1) %>%
#       fp_set_style(txt_gp = fpTxtGp(label = gpar(cex=0.55),
#                                     xlab  = gpar(cex = 0.5),
#                                     ticks = gpar(cex = 0.3))) %>%
#       fp_decorate_graph(graph.pos = 3) %>%
#       fp_set_zebra_style("#f9f9f9") %>%
#       fp_insert_row(term = 'Hospitalization', 
#                     n_event = 'No Events', 
#                     position = 1, is.summary = T) %>%
#       fp_insert_row('Death', position = 5, is.summary = T) %>%
#       fp_insert_row('Hosp. or Death', position = 9, is.summary = T) %>%
#       fp_insert_row('Hosp. or Death - All Periods', position = 13, is.summary = T) %>%
#       fp_add_header(term = 'A. Initial Vaccination Scheme',  position = 1, is.summary = T) %>%
#       fp_insert_row(term = 'Hospitalization', 
#                     n_event = 'No Events', 
#                     position = 24, is.summary = T) %>%
#       fp_insert_row('Death', position = 28, is.summary = T) %>%
#       fp_insert_row('Hosp. or Death', position = 32, is.summary = T) %>%
#       fp_insert_row('Hosp. or Death - All Periods', position = 36, is.summary = T) %>%
#       fp_add_header(term = 'B. Booster Vaccination',  position = 24, is.summary = T) %>%
#       fp_add_header(term = '',  position = 24, is.summary = T) %>%
#       fp_add_lines("lightgray")
# 
# pdf('Figures/forest_plot_main_results_123.pdf', width = 6, height = 7)
# graph_main_123
# dev.off()

# Plotting Sensitivity Analysis
# Creating Main Table Results 
files_sens_results_12 <- list(
      'Results/dose_12/rem_main_analysis/outcome_hosp_death_period_three.csv' = '1.1 main analysis',
      'Results/dose_12/sub_group_cancer_strict/outcome_hosp_death_period_three.csv' = '1.2 tested patients',
      'Results/dose_12/sub_group_tested_patients/outcome_hosp_death_period_three.csv' = '1.3 cancer strict', 
      'Results/dose_12/sub_group_covid_lab/outcome_hosp_death_period_three.csv' = '1.4 covid lab',
      'Results/dose_12/sub_group_hosp_3_days/outcome_hosp_death_period_three.csv' = '1.5 hosp 3 days',
      'Results/dose_12/sub_group_hosp_14_and_3_days/outcome_hosp_death_period_three.csv' = '1.6 hosp 14-3 days',
      'Results/dose_12/sub_group_not_jansen/outcome_hosp_death_period_three.csv' = '1.7 not jansen')

files_sens_results_3 <- list('Results/dose_3/rem_main_analysis/outcome_hosp_death_period_three.csv' = '3.1 main analysis',
      'Results/dose_3/sub_group_cancer_strict/outcome_hosp_death_period_three.csv' = '3.2 tested patients',
      'Results/dose_3/sub_group_tested_patients/outcome_hosp_death_period_three.csv' = '3.3 cancer strict', 
      'Results/dose_3/sub_group_covid_lab/outcome_hosp_death_period_three.csv' = '3.4 covid lab',
      'Results/dose_3/sub_group_hosp_3_days/outcome_hosp_death_period_three.csv' = '3.5 hosp 3 days',
      'Results/dose_3/sub_group_hosp_14_and_3_days/outcome_hosp_death_period_three.csv' = '3.6 hosp 14-3 days')

file_forest_12 <- names(files_sens_results_12)
names(file_forest_12) <- files_sens_results_12

forest_tables <- lapply(file_forest_12, create_forest_table_sens_results)
forest_table <- do.call(bind_rows, forest_tables)

graph_sens_results_12 <- 
      forest_table %>% 
      mutate(term = case_when(
            term == 'V1 14D+' ~ '  Partially Vaccinated',
            term == 'V2 7D+' ~ '  Fully Vaccinated',
            term == 'no-vax' ~ '  Unvaccinated',
      )) %>%
      filter(!is.na(term)) %>%
      mutate(est_ve.conf.interval = ifelse(mean == 0, 'Reference', est_ve.conf.interval)) %>%
      forestplot(labeltext = c(term, exposure, n_event, est_ve.conf.interval),
                 vertices = TRUE,
                 clip = c(-20, 100),
                 xlog = F,
                 zero = 0,
                 align = c("l", 'c', 'c', 'c'),
                 xticks = c(-20, 0, 50, 100),
                 xlab = 'VE',
                 boxsize = .1) %>%
      fp_set_style(txt_gp = fpTxtGp(label = gpar(cex= 0.4),
                                    xlab  = gpar(cex = 0.5),
                                    ticks = gpar(cex = 0.3))) %>%
      fp_decorate_graph(graph.pos = 4) %>%
      fp_set_zebra_style("#f9f9f9") %>%
      fp_insert_row(term = 'Main Results', 
                    exposure = 'Person-Days',
                    n_event = 'No Events', 
                    est_ve.conf.interval = 'VE',
                    position = 1, is.summary = T) %>%
      fp_insert_row('Only Tested Patients', position = 5, is.summary = T) %>%
      fp_insert_row('Strict Cancer Diagnosis', position = 9, is.summary = T) %>%
      fp_insert_row('Laboratory COVID-19', position = 13, is.summary = T) %>%
      fp_insert_row('COVID-19 21D bef. - 3D aft.', position = 17, is.summary = T) %>%
      fp_insert_row('COVID-19 14D bef. - 3D aft.', position = 21, is.summary = T) %>%
      fp_insert_row('Excl. Ad26.COV2.S Vaccine', position = 25, is.summary = T) %>%
      fp_insert_row(term = 'PRIMARY VACCINATION SCHEME',  position = 1, is.summary = T) %>%
      fp_add_lines("lightgray")

pdf('Figures/forest_plot_sens_results_12.pdf', width = 5.5, height = 5)
graph_sens_results_12
dev.off()

file_forest_3 <- names(files_sens_results_3)
names(file_forest_3) <- files_sens_results_3

forest_tables <- lapply(file_forest_3, create_forest_table_sens_results)
forest_table <- do.call(bind_rows, forest_tables)

graph_sens_results_3 <- 
      forest_table %>% 
      mutate(term = case_when(
            term == 'no-vax' ~ '  Un-boosted',
            term == 'V3 14-60D' ~ '  14 - 59 days after booster',
            term == 'V3 60+' ~ '  60 or more days after booster',
      )) %>%
      filter(!is.na(term)) %>%
      mutate(est_ve.conf.interval = ifelse(mean == 0, 'Reference', est_ve.conf.interval)) %>%
      forestplot(labeltext = c(term, exposure, n_event, est_ve.conf.interval),
                 vertices = TRUE,
                 clip = c(-20, 100),
                 xlog = F,
                 zero = 0,
                 align = c("l", 'c', 'c', 'c'),
                 xticks = c(-20, 0, 50, 100),
                 xlab = 'rVE',
                 boxsize = .1) %>%
      fp_set_style(txt_gp = fpTxtGp(label = gpar(cex= 0.4),
                                    xlab  = gpar(cex = 0.5),
                                    ticks = gpar(cex = 0.3))) %>%
      fp_decorate_graph(graph.pos = 4) %>%
      fp_set_zebra_style("#f9f9f9") %>%
      fp_insert_row(term = 'Main Results', 
                    exposure = 'Person-Days',
                    est_ve.conf.interval = 'rVE',
                    n_event = 'No Events', position = 1, is.summary = T) %>%
      fp_insert_row('Only Tested Patients', position = 5, is.summary = T) %>%
      fp_insert_row('Strict Cancer Diagnosis', position = 9, is.summary = T) %>%
      fp_insert_row('Laboratory COVID-19', position = 13, is.summary = T) %>%
      fp_insert_row('COVID-19 21D bef. - 3D aft.', position = 17, is.summary = T) %>%
      fp_insert_row('COVID-19 14D bef. - 3D aft.', position = 21, is.summary = T) %>%
      fp_insert_row(term = 'BOOSTER VACCINATION SCHEME',  position = 1, is.summary = T) %>%
      fp_add_lines("lightgray")

pdf('Figures/forest_plot_sens_results_3.pdf', width = 5.5, height = 4.5)
graph_sens_results_3
dev.off()

# Sensitivity Figures
# Creating Subgroup Analysis Forest Plot with N Events and N Obs
# Forest Table 1st and 2nd Dose
file_forest <- paste('Results/dose_12/rem_main_analysis', 'subgroup_outcome_hosp_death_three_periods.csv', sep = '/')
forest_table <- create_forest_table_subgroup(file_forest)

conLines <- 5
      
forest_sg_12_complete <- 
      forest_table %>% 
      filter(str_detect(model_interaction_var, subgroup)) %>%
      mutate(term = case_when(
            term == 'Age < 65' ~ '  < 65 years',
            term == 'Age 65+' ~ '  >= 65 years',
            term == 'Other VOC' ~ '  Other',
            term == 'Delta VOC' ~ '  Delta',
            term == 'Omicron VOC' ~ '  Omicron',
            term == 'M' ~ '  Male',
            term == 'F' ~ '  Female',
            term == '< 1y' ~ '  < 1 year',
            term == '1-5yr' ~ '  1 - 5 years',
            model_interaction_var != 'visits_outpatient_cat' & term == '0' ~ '  No',
            model_interaction_var != 'visits_outpatient_cat' & term == '1' ~ '  Yes',
            TRUE ~ term
      ))  %>%
      mutate(n_events_control = sprintf('%.0f / %.0f', crtl.n_events, crtl.n_obs),
             n_events_trt = sprintf('%.0f / %.0f', trt.n_events, trt.n_obs)) %>%
      group_by(model_interaction_var) %>%
      mutate(model_p_value = ifelse(coalesce(model_p_value == lag(model_p_value), F), NA, model_p_value)) %>%
      ungroup() %>% 
      arrange(match(model_interaction_var, c('age_bin_65', 'gender_concept_id', 'cancer_diagnosis_time_bin_0',
                                             'CCI_Metastatic_Solid_Tumor', 'cancer_dx_lung', 'cancer_group_hemathological',
                                             'covid_voc', 'vac_mRNA_12'))) %>%
      forestplot(labeltext = c(term, contrast, n_events_control, n_events_trt, est_ve.conf.interval, model_p_value),
                 vertices = TRUE,
                 clip = c(-20, 100),
                 xlog = F,
                 zero = 0,
                 align = c("l", 'l', 'c', 'c', 'c'),
                 xticks = c(-20, 0, 50, 100),
                 xlab = 'Vaccine Effectiveness',
                 plotwidth = unit(20, "cm"),
                 boxsize = .2) %>%
      fp_set_style(txt_gp = fpTxtGp(label = gpar(cex=0.4),
                                    xlab  = gpar(cex = 0.5),
                                    ticks = gpar(cex = 0.3))) %>%
      fp_decorate_graph(graph.pos = 6) %>%
      fp_set_zebra_style("#f9f9f9") %>%
      fp_set_style(box = c('white', rep(c('white', "black", "chocolate", "black", "chocolate"), 7)) %>% 
                         lapply(function(x) gpar(fill = x, col = x)),
                   default = gpar(vertices = TRUE)) %>%
      fp_insert_row(term = 'Age',  position = 1 + 0*nLines, is.summary = F) %>%
      fp_insert_row(term = 'Sex',  position = 1 + 1*nLines, is.summary = F) %>%
      fp_insert_row(term = 'Cancer Diagnosis Time',  position = 1 + 2*nLines, is.summary = F) %>%
      fp_insert_row(term = 'Metastatic Disease',  position = 1 + 3*nLines, is.summary = F) %>%
      fp_insert_row(term = 'Lung Cancer',  position = 1 + 4*nLines, is.summary = F) %>%
      fp_insert_row(term = 'Hemathological Cancer',  position = 1 + 5*nLines, is.summary = F) %>%
      fp_insert_row(term = 'Variant of Concern',  position = 1 + 6*nLines, is.summary = F) %>%
      fp_insert_row(term = '',
                    contrast = 'Vaccination\nStatus',
                    n_events_control = 'Events/Obs\nUnvax',
                    n_events_trt = 'Events/Obs\nVax',
                    est_ve.conf.interval = 'VE (95%CI)',
                    model_p_value = 'Interaction\np-value', position = 1, is.summary = T) %>%
      do_nothing()

pdf('Figures/forest_plot_subgroup_complete_12.pdf', width = 7, height = 7)
forest_sg_12_complete
dev.off()

# Forest Table 3rd Dose
file_forest <- paste('Results/dose_3/rem_main_analysis', 'subgroup_outcome_hosp_death_three_periods.csv', sep = '/')
forest_table <- create_forest_table_subgroup(file_forest)

nLines <- 5

forest_sg_3_complete <- 
      forest_table %>% 
      filter(str_detect(model_interaction_var, subgroup)) %>%
      mutate(term = case_when(
            term == 'Age < 65' ~ '  < 65 years',
            term == 'Age 65+' ~ '  >= 65 years',
            term == 'Other VOC' ~ '  Other',
            term == 'Delta VOC' ~ '  Delta',
            term == 'Omicron VOC' ~ '  Omicron',
            term == 'M' ~ '  Male',
            term == 'F' ~ '  Female',
            term == '< 1y' ~ '  < 1 year',
            term == '1-5yr' ~ '  1 - 5 years',
            model_interaction_var != 'visits_outpatient_cat' & term == '0' ~ '  No',
            model_interaction_var != 'visits_outpatient_cat' & term == '1' ~ '  Yes',
            TRUE ~ term
      ))  %>%
      mutate(n_events_control = sprintf('%.0f / %.0f', crtl.n_events, crtl.n_obs),
             n_events_trt = sprintf('%.0f / %.0f', trt.n_events, trt.n_obs)) %>%
      group_by(model_interaction_var) %>%
      mutate(model_p_value = ifelse(coalesce(model_p_value == lag(model_p_value), F), NA, model_p_value)) %>%
      ungroup() %>% 
      arrange(match(model_interaction_var, c('age_bin_65', 'gender_concept_id', 'cancer_diagnosis_time_bin_0',
                                             'CCI_Metastatic_Solid_Tumor', 'cancer_dx_lung', 'cancer_group_hemathological',
                                             'covid_voc', 'vac_mRNA_12'))) %>%
      forestplot(labeltext = c(term, contrast, n_events_control, n_events_trt, est_ve.conf.interval,model_p_value),
                 vertices = TRUE,
                 clip = c(-20, 100),
                 xlog = F,
                 zero = 0,
                 align = c("l", 'l', 'c', 'c', 'c'),
                 xticks = c(-20, 0, 50, 100),
                 xlab = 'Vaccine Effectiveness',
                 plotwidth = unit(20, "cm"),
                 boxsize = .2) %>%
      fp_set_style(txt_gp = fpTxtGp(label = gpar(cex=0.4),
                                    xlab  = gpar(cex = 0.5),
                                    ticks = gpar(cex = 0.3))) %>%
      fp_decorate_graph(graph.pos = 6) %>%
      fp_set_zebra_style("#f9f9f9") %>%
      fp_set_style(box = c('white', rep(c('white', "black", "chocolate", "black", "chocolate"), 8)) %>% 
                         lapply(function(x) gpar(fill = x, col = x)),
                   default = gpar(vertices = TRUE)) %>%
      fp_insert_row(term = 'Age',  position = 1 + 0*nLines, is.summary = F) %>%
      fp_insert_row(term = 'Sex',  position = 1 + 1*nLines, is.summary = F) %>%
      fp_insert_row(term = 'Cancer Diagnosis Time',  position = 1 + 2*nLines, is.summary = F) %>%
      fp_insert_row(term = 'Metastatic Disease',  position = 1 + 3*nLines, is.summary = F) %>%
      fp_insert_row(term = 'Lung Cancer',  position = 1 + 4*nLines, is.summary = F) %>%
      fp_insert_row(term = 'Hemathological Cancer',  position = 1 + 5*nLines, is.summary = F) %>%
      fp_insert_row(term = 'Variant of Concern',  position = 1 + 6*nLines, is.summary = F) %>%
      fp_insert_row(term = 'Previous mRNA Vaccine',  position = 1 + 7*nLines, is.summary = F) %>%
      fp_insert_row(term = '',
                    contrast = 'Vaccination\nStatus',
                    n_events_control = 'Events/Obs\nUnvax',
                    n_events_trt = 'Events/Obs\nVax',
                    est_ve.conf.interval = 'VE (95%CI)',
                    model_p_value = 'Interaction\np-value', position = 1, is.summary = T) %>%
      do_nothing()

pdf('Figures/forest_plot_subgroup_complete_3.pdf', width = 7, height = 7)
forest_sg_3_complete
dev.off()

# Cumulative Hazards Incidence
df_rem12 <- readRDS(here('Results', 'dose_12', 'rem_main_analysis', 'cuminc_outcome_death.RDS'))$data %>%
      distinct(new_id, .keep_all = T) %>%
      mutate(outcome_death_status = as.numeric(outcome_death_status) - 1) %>%
      mutate(tx_group = ifelse(tx_group == 1, 'Vaccinated', 'Unvaccinated'),
             tx_group = factor(tx_group, levels = c('Unvaccinated', 'Vaccinated')))

g_hosp_death_rem12 <- survfit2(Surv(outcome_hosp_death_time, outcome_hosp_death_status == 2) ~ tx_group, data = df_rem12) %>%
      ggsurvfit(type = 'cumhaz') +
      add_risktable(risktable_stats = c("n.risk", "n.event")) + 
      add_censor_mark() +
      scale_color_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_fill_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_x_continuous(breaks = seq(0, 180, 60)) +
      add_confidence_interval() +
      coord_cartesian(xlim = c(0, 180), ylim = c(0, 0.04))

df_rem3 <- readRDS(here('Results', 'dose_3', 'rem_main_analysis', 'cuminc_outcome_death.RDS'))$data %>%
      distinct(new_id, .keep_all = T) %>%
      mutate(outcome_death_status = as.numeric(outcome_death_status) - 1) %>%
      mutate(tx_group = ifelse(tx_group == 1, 'Boosted', 'Un-boosted'),
             tx_group = factor(tx_group, levels = c('Un-boosted', 'Boosted')))

g_hosp_death_rem3 <- survfit2(Surv(outcome_hosp_death_time, outcome_hosp_death_status == 2) ~ tx_group, data = df_rem3) %>%
      ggsurvfit(type = 'cumhaz') +
      add_risktable(risktable_stats = c("n.risk", "n.event")) + 
      add_censor_mark() +
      scale_color_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_fill_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_x_continuous(breaks = seq(0, 180, 60)) +
      add_confidence_interval() +
      coord_cartesian(xlim = c(0, 180), ylim = c(0, 0.04))

fig_cumhaz_alltime <- cowplot::plot_grid(ggsurvfit_build(g_hosp_death_rem12), 
                                         ggsurvfit_build(g_hosp_death_rem3), 
                                         ncol = 2, 
                                         labels = c("A", "B"))

# ggsave('Figures/fig_hosp_death_combined.pdf', height = 5*1.1, width = 10*1.2, units = 'in')

# 0 - 30 days
g_hosp_death_rem12_030 <- survfit2(Surv(outcome_hosp_death_time, outcome_hosp_death_status == 2) ~ tx_group, data = df_rem12) %>%
      ggsurvfit(type = 'cumhaz') +
      add_risktable(risktable_stats = c("n.risk")) + 
      add_censor_mark() +
      scale_color_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_fill_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_x_continuous(breaks = seq(0, 180, 10)) +
      add_confidence_interval() + 
      coord_cartesian(xlim = c(0, 30), ylim = c(0, 0.04))

g_hosp_death_rem3_030 <- survfit2(Surv(outcome_hosp_death_time, outcome_hosp_death_status == 2) ~ tx_group, data = df_rem3) %>%
      ggsurvfit(type = 'cumhaz') +
      add_risktable(risktable_stats = c("n.risk")) + 
      add_censor_mark() +
      scale_color_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_fill_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_x_continuous(breaks = seq(0, 180, 10)) +
      add_confidence_interval() + 
      coord_cartesian(xlim = c(0, 30), ylim = c(0, 0.04))

fig_cumhaz_030 <- cowplot::plot_grid(ggsurvfit_build(g_hosp_death_rem12_030), 
                                         ggsurvfit_build(g_hosp_death_rem3_030), 
                                         ncol = 2, 
                                         labels = c("A", "B"))

# ggsave('Figures/fig_hosp_death_combined_030.pdf', height = 5*1.1, width = 10*1.2, units = 'in')

# Any Cause Hospitalization
g_any_hosp_rem12 <- survfit2(Surv(outcome_any_hosp_time, outcome_any_hosp_status == 2) ~ tx_group, data = df_rem12) %>%
      ggsurvfit(type = 'cumhaz') +
      add_risktable(risktable_stats = c("n.risk", "n.event")) + 
      add_censor_mark() +
      scale_color_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_fill_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_x_continuous(breaks = seq(0, 180, 60)) +
      add_confidence_interval() +
      coord_cartesian(xlim = c(0, 180), ylim = c(0, 0.2))

g_any_hosp_rem3 <- survfit2(Surv(outcome_any_hosp_time, outcome_any_hosp_status == 2) ~ tx_group, data = df_rem3) %>%
      ggsurvfit(type = 'cumhaz') +
      add_risktable(risktable_stats = c("n.risk", "n.event")) + 
      add_censor_mark() +
      scale_color_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_fill_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_x_continuous(breaks = seq(0, 180, 60)) +
      add_confidence_interval() +
      coord_cartesian(xlim = c(0, 180), ylim = c(0, 0.2))

fig_cumhaz_any_hosp <- cowplot::plot_grid(ggsurvfit_build(g_any_hosp_rem12), 
                                     ggsurvfit_build(g_any_hosp_rem3), 
                                     ncol = 2, 
                                     labels = c("A", "B"))

# ggsave('Figures/fig_any_hosp_combined.pdf', height = 5*1.1, width = 10*1.2, units = 'in')

# Restrictive Matching Cohort Comparison
df_sens12 <- readRDS(here('Results', 'dose_12', 'rem_sens_analysis', 'cuminc_outcome_death.RDS'))$data %>%
      distinct(new_id, .keep_all = T) %>%
      mutate(outcome_death_status = as.numeric(outcome_death_status) - 1) %>%
      mutate(tx_group = ifelse(tx_group == 1, 'Vaccinated', 'Unvaccinated'),
             tx_group = factor(tx_group, levels = c('Unvaccinated', 'Vaccinated')))

df_sens3 <- readRDS(here('Results', 'dose_3', 'rem_sens_analysis', 'cuminc_outcome_death.RDS'))$data %>%
      distinct(new_id, .keep_all = T) %>%
      mutate(outcome_death_status = as.numeric(outcome_death_status) - 1) %>%
      mutate(tx_group = ifelse(tx_group == 1, 'Boosted', 'Un-boosted'),
             tx_group = factor(tx_group, levels = c('Un-boosted', 'Boosted')))

# Creating Graphs Sens Analysis - 
# COVID-19 Outcome
g_hosp_death_sens3 <- survfit2(Surv(outcome_hosp_death_time, outcome_hosp_death_status == 2) ~ tx_group, data = df_sens3) %>%
      ggsurvfit(type = 'cumhaz') +
      add_risktable(risktable_stats = c("n.risk", "n.event")) + 
      add_censor_mark() +
      scale_color_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_fill_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_x_continuous(breaks = seq(0, 180, 60)) +
      add_confidence_interval() +
      coord_cartesian(xlim = c(0, 180), ylim = c(0, 0.04))

g_hosp_death_sens12 <- survfit2(Surv(outcome_hosp_death_time, outcome_hosp_death_status == 2) ~ tx_group, data = df_sens12) %>%
      ggsurvfit(type = 'cumhaz') +
      add_risktable(risktable_stats = c("n.risk", "n.event")) + 
      add_censor_mark() +
      scale_color_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_fill_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_x_continuous(breaks = seq(0, 180, 60)) +
      add_confidence_interval() +
      coord_cartesian(xlim = c(0, 180), ylim = c(0, 0.04))

# Any Hospitalization
g_any_hosp_sens12 <- survfit2(Surv(outcome_any_hosp_time, outcome_any_hosp_status == 2) ~ tx_group, data = df_sens12) %>%
      ggsurvfit(type = 'cumhaz') +
      add_risktable(risktable_stats = c("n.risk", "n.event")) + 
      add_censor_mark() +
      scale_color_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_fill_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_x_continuous(breaks = seq(0, 180, 60)) +
      add_confidence_interval() +
      coord_cartesian(xlim = c(0, 180), ylim = c(0, 0.2))

g_any_hosp_sens3 <- survfit2(Surv(outcome_any_hosp_time, outcome_any_hosp_status == 2) ~ tx_group, data = df_sens3) %>%
      ggsurvfit(type = 'cumhaz') +
      add_risktable(risktable_stats = c("n.risk", "n.event")) + 
      add_censor_mark() +
      scale_color_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_fill_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_x_continuous(breaks = seq(0, 180, 60)) +
      add_confidence_interval() +
      coord_cartesian(xlim = c(0, 180), ylim = c(0, 0.2))

# Non-COVID-19 Death
g_noncovid_death_sens3 <- survfit2(Surv(outcome_death_time, outcome_hosp_death_status == 1) ~ tx_group, data = df_sens3) %>%
      ggsurvfit(type = 'cumhaz') +
      add_risktable(risktable_stats = c("n.risk", "n.event")) + 
      add_censor_mark() +
      scale_color_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_fill_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_x_continuous(breaks = seq(0, 180, 60)) +
      add_confidence_interval() +
      coord_cartesian(xlim = c(0, 180), ylim = c(0, 0.1))

g_noncovid_death_sens12 <- survfit2(Surv(outcome_death_time, outcome_hosp_death_status == 1) ~ tx_group, data = df_sens12) %>%
      ggsurvfit(type = 'cumhaz') +
      add_risktable(risktable_stats = c("n.risk", "n.event")) + 
      add_censor_mark() +
      scale_color_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_fill_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_x_continuous(breaks = seq(0, 180, 60)) +
      add_confidence_interval() +
      coord_cartesian(xlim = c(0, 180), ylim = c(0, 0.1))

g_noncovid_death_rem3 <- survfit2(Surv(outcome_death_time, outcome_hosp_death_status == 1) ~ tx_group, data = df_rem3) %>%
      ggsurvfit(type = 'cumhaz') +
      add_risktable(risktable_stats = c("n.risk", "n.event")) + 
      add_censor_mark() +
      scale_color_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_fill_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_x_continuous(breaks = seq(0, 180, 60)) +
      add_confidence_interval() +
      coord_cartesian(xlim = c(0, 180), ylim = c(0, 0.1))

g_noncovid_death_rem12 <- survfit2(Surv(outcome_death_time, outcome_hosp_death_status == 1) ~ tx_group, data = df_rem12) %>%
      ggsurvfit(type = 'cumhaz') +
      add_risktable(risktable_stats = c("n.risk", "n.event")) + 
      add_censor_mark() +
      scale_color_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_fill_manual(values = c('#0073C2FF', '#EFC000FF')) +
      scale_x_continuous(breaks = seq(0, 180, 60)) +
      add_confidence_interval() +
      coord_cartesian(xlim = c(0, 180), ylim = c(0, 0.1))

# Cohort A Figure
fig_cumhaz_12_comparison <- cowplot::plot_grid(ggsurvfit_build(g_any_hosp_rem12), 
                                          ggsurvfit_build(g_any_hosp_sens12), 
                                          ggsurvfit_build(g_noncovid_death_rem12), 
                                          ggsurvfit_build(g_noncovid_death_sens12), 
                                          ggsurvfit_build(g_hosp_death_rem12), 
                                          ggsurvfit_build(g_hosp_death_sens12), 
                                          ncol = 2, 
                                          labels = c("A", "", "B", "", "C", ""))

ggsave('Figures/fig_comparison_rem_sens_12_combined.png', height = 10*1.5, width = 8*1.5, units = 'in')

# Cohort B Figure
fig_cumhaz_3_comparison <- cowplot::plot_grid(ggsurvfit_build(g_any_hosp_rem3), 
                                               ggsurvfit_build(g_any_hosp_sens3), 
                                               ggsurvfit_build(g_noncovid_death_rem3), 
                                               ggsurvfit_build(g_noncovid_death_sens3), 
                                               ggsurvfit_build(g_hosp_death_rem3), 
                                               ggsurvfit_build(g_hosp_death_sens3), 
                                               ncol = 2, 
                                               labels = c("A", "", "B", "", "C", ""))

ggsave('Figures/fig_comparison_rem_sens_3_combined.png', height = 10*1.5, width = 8*1.5, units = 'in')
