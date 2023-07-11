library(tidyverse)
library(openxlsx)
library(here)
library(readxl)
library(ggplot2)
library(lubridate)
require(grid)
library(gridExtra)
source('utils.R')

# Setting WD
mainWD <- '/Users/felippelazarneto/Google Drive (felippe.neto@alumni.usp.br)/SIDIAP Analysis/'

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
      scale_fill_manual(values = c('#B1746F','#FFB547', '#725663')) 

# Cumulative Vaccine Rollout
vac_rollout <- read.table(glue(mainWD, 'Results/descriptive_until_2022/vacc_cum_rollout_by_dose.csv'), 
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
covid_cases <- read.table(glue(mainWD, 'Results/descriptive_until_2022/covid_frequency.csv'), 
                          sep = ';', header = T)

g_covid <- covid_cases %>%
      mutate(covid_date = ymd(covid_date)) %>%
      ggplot(aes(x = covid_date, y = n)) + geom_area(fill = '#996136') +
      labs(x = '', y = 'number of COVID-19 cases (N)', fill = '') + theme_bw() + 
      theme(legend.position = 'none')

lay = rbind(c(1), c(2), c(3))

tt <- grid.arrange(g_covid, g_voc, g_vac_rollout, 
                   layout_matrix = lay)

ggsave("Figures/figure_covid_vax.png", plot = tt, height = 260, width = 2*260/3, units = "mm", dpi = "print")

# Forest Plots
library(meta)
library(forestplot)

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
            mutate(model_p_value = sprintf('%.3f', model_p_value)) %>%
            mutate(contrast = case_when(
                  contrast == '(V1 14D+) - (no-vax)' ~ 'Partially Vaccinated',
                  contrast == '(V2 7D+) - (no-vax)' ~ 'Fully Vaccinated',
                  contrast == '(V3 14-60D) - (no-vax)' ~ 'Booster 14D-60D',
                  contrast == '(V3 60+) - (no-vax)' ~ 'Booster 60D+'
            ))
      
      return(forest_table)
}

subgroup <- 'age_bin_65|gender|cancer_diagnosis_time_bin_0|CCI_Metastatic_Solid_Tumor|cancer_dx_lung|cancer_group_hemathological|covid_voc|vac_mRNA_12'

# Forest Table 1st and 2nd Dose
file_forest <- paste(mainWD, 'Results/dose_12/rem_main_analysis', 'subgroup_outcome_hosp_death_three_periods.csv', sep = '/')
forest_table <- create_forest_table_subgroup(file_forest)

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
      fp_set_style(txt_gp = fpTxtGp(label = gpar(cex=0.60))) %>%
      fp_decorate_graph(graph.pos = 2) %>%
      fp_set_zebra_style("#f9f9f9") %>%
      fp_set_style(box = c("black", "chocolate") %>% 
                         lapply(function(x) gpar(fill = x, col = x)),
                   default = gpar(vertices = TRUE)) %>%
      fp_insert_row(term = 'Age',  position = 1, is.summary = F) %>%
      fp_insert_row(term = 'Gender',  position = 4, is.summary = F) %>%
      fp_insert_row(term = 'Cancer Diagnosis Time',  position = 7, is.summary = F) %>%
      fp_insert_row(term = 'Metastatic Disease',  position = 10, is.summary = F) %>%
      fp_insert_row(term = 'Lung Cancer',  position = 13, is.summary = F) %>%
      fp_insert_row(term = 'Hemathological Cancer',  position = 16, is.summary = F) %>%
      fp_insert_row(term = 'Variant of Concern',  position = 19, is.summary = F)


file_forest <- paste(mainWD, 'Results/dose_3/rem_main_analysis', 'subgroup_outcome_hosp_death_three_periods.csv', sep = '/')
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
      fp_set_style(txt_gp = fpTxtGp(label = gpar(cex=0.60))) %>%
      fp_decorate_graph(graph.pos = 2) %>%
      fp_set_zebra_style("#f9f9f9") %>%
      fp_set_style(box = c("black", "chocolate") %>% 
                         lapply(function(x) gpar(fill = x, col = x)),
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

# Exporting Combined Graph
pdf('Figures/forest_plot_subgroups_combined.pdf', width = 9, height = 5)
grid.newpage()
borderWidth <- unit(1, "pt")
width <- unit(convertX(unit(1, "npc") - borderWidth, unitTo = "npc", valueOnly = TRUE)/2, "npc")
pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 3, widths = unit.c(width, borderWidth, width))))
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
forest_sg_12
upViewport()
pushViewport(viewport(layout.pos.row = 1,layout.pos.col = 2))
grid.rect(gp = gpar(fill = "white", col = "white"))
upViewport()
pushViewport(viewport(layout.pos.row = 1,layout.pos.col = 3))
forest_sg_3
upViewport(2)
dev.off()

# Creating Main Table Results 
create_forest_table_main_results <- function(files_forest){
      
      # Getting Files Results
      temp_tables <- lapply(files_forest , function(x) read.csv(x, row.names=NULL, header = T, sep = ';') %>% 
                                  mutate(analysis = files_main_results[[str_extract(x, '([^/]*)$')]], .before=term))
      
      # Binding Tables
      temp_table <- do.call(bind_rows, temp_tables)
      temp_table <- temp_table %>%
            separate(analysis, into = c('outcome', 'periods'), sep = '-') %>%
            mutate(term = str_replace(term, 'period', '')) %>%
            mutate(estimate = ifelse(reference_row == TRUE, 1, estimate),
                   conf.low = ifelse(reference_row == TRUE, 1, conf.low),
                   conf.high = ifelse(reference_row == TRUE, 1, conf.high)) %>%
            mutate(est.conf.interval = sprintf('%.2f (%.2f - %.2f)', estimate, conf.low, conf.high)) %>%
            mutate(estimate_ve = if_else(estimate>1, 
                                         -(1-(1/estimate))*100, (1-estimate)*100),
                   conf.low_ve = if_else(conf.low>1, 
                                         -(1-(1/conf.low))*100, (1-conf.low)*100),
                   conf.high_ve= if_else(conf.high>1, 
                                         -(1-(1/conf.high))*100, (1-conf.high)*100)) %>%
            mutate(est_ve.conf.interval = sprintf('%.1f%% (%.1f - %.1f)', estimate_ve, conf.high_ve, conf.low_ve))
      
      forest_table <- temp_table %>%
            select(outcome, periods, n_event,
                   term, estimate, conf.low, conf.high, p.value, est.conf.interval,
                   estimate_ve, conf.low_ve, conf.high_ve, est_ve.conf.interval) %>%
            rename(mean = estimate_ve,
                   lower = conf.high_ve,
                   upper = conf.low_ve) %>%
            mutate(p.value = sprintf('%.3f', p.value))
      
      
      return(forest_table)
}

files_main_results <- list(
      'outcome_hosp_period_three.csv' = '2 hospitalization',
      'outcome_death_period_three.csv' = '3 death',
      'outcome_hosp_death_period_three.csv' = '41 combined hosp death',
      'outcome_hosp_death_period_all.csv' = '42 combined hosp death')

string_regex_results = paste0('(', paste(names(files_main_results), collapse = '|'), ')')

file_forest <- list.files(paste0(mainWD, 'Results/dose_12/rem_main_analysis', sep='/'), pattern = string_regex_results, full.names = T)
forest_table <- create_forest_table_main_results(file_forest)

graph_main_12 <- forest_table %>% 
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
      forestplot(labeltext = c(term, n_event, est_ve.conf.interval),
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
      fp_decorate_graph(graph.pos = 3) %>%
      fp_set_zebra_style("#f9f9f9") %>%
      fp_insert_row(term = 'Hospitalization', 
                    n_event = 'No Events', 
                    position = 1, is.summary = T) %>%
      fp_insert_row('Death', position = 5, is.summary = T) %>%
      fp_insert_row('Hosp. or Death', position = 9, is.summary = T) %>%
      fp_insert_row('Hosp. or Death - All Periods', position = 13, is.summary = T)

file_forest <- list.files(paste0(mainWD, 'Results/dose_3/rem_main_analysis', sep='/'), pattern = string_regex_results, full.names = T)
forest_table <- create_forest_table_main_results(file_forest)

graph_main_3 <- forest_table %>% 
      arrange(outcome) %>%
      mutate(term = case_when(
            term == 'V3 0-14D' ~ '  0 - 13 days after booster',
            term == 'V3 14-28D' ~ '  14 - 27 days after booster',
            term == 'V3 28-60D' ~ '  28 - 59 days after booster',
            term == 'V3 60-120' ~ '  60 - 119 days after booster',
            term == 'V3 120+' ~ '  120 or more days after booster',
            term == 'V3 14-60D' ~ '  14 - 59 days after booster',
            term == 'V3 60+' ~ '  60 or more days after booster',
            term == 'no-vax' ~ '  Unvaccinated',
      )) %>%
      forestplot(labeltext = c(term, n_event, est_ve.conf.interval),
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
      fp_decorate_graph(graph.pos = 3) %>%
      fp_set_zebra_style("#f9f9f9") %>%
      fp_insert_row(term = 'Hospitalization', 
                    n_event = 'No Events', 
                    position = 1, is.summary = T) %>%
      fp_insert_row('Death', position = 5, is.summary = T) %>%
      fp_insert_row('Hosp. or Death', position = 9, is.summary = T) %>%
      fp_insert_row('Hosp. or Death - All Periods', position = 13, is.summary = T)

# Exporting Combined Graph
pdf('Figures/forest_plot_main_groups_combined.pdf', width = 6, height = 9)
grid.newpage()
borderWidth <- unit(0.001, "pt")
width <- unit(convertX(unit(1, "npc") - borderWidth, unitTo = "npc", valueOnly = TRUE)/2, "npc")
pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 1, widths = unit.c(width, borderWidth, width))))
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
graph_main_12
upViewport()
pushViewport(viewport(layout.pos.row = 2,layout.pos.col = 1))
graph_main_3
upViewport(1)
dev.off()

# Plotting Sensitivity Analysis
