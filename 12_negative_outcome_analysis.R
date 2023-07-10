library(EmpiricalCalibration)
library(here)
library(glue)
library(tidyverse)
library(openxlsx)

# Setting Temporary WD
resultsWD <- '/Users/felippelazarneto/Google Drive (felippe.neto@alumni.usp.br)/SIDIAP Analysis/Results/'
resultsTidyWD <- '/Users/felippelazarneto/Google Drive (felippe.neto@alumni.usp.br)/SIDIAP Analysis/Results Tidy/'

# Loading Table of Negative Outcome Results
getNegativeOutcomesResults <- function(concept_id, file_location){
      
      file_full_path = glue(file_location, 'outcome_three_periods', concept_id, '.csv') 
      
      return(read.table(file_full_path, sep=';', header = T) %>%
            select(term, estimate, std.error, conf.low, conf.high, n_event) %>%
            mutate(ConceptId = concept_id))
}

NCO <- read_csv(file=here("NCO.csv"), show_col_types = FALSE) # File with the conditions chosen to assess residual confounding

negOutcomes12 <- lapply(NCO$ConceptId, getNegativeOutcomesResults, glue(resultsWD, 'dose_12/negative outcomes/'))
negOutcomes12 <- do.call(bind_rows, negOutcomes12)
negOutcomes12 <- negOutcomes12 %>% left_join(NCO)

negOutcomes3 <- lapply(NCO$ConceptId, getNegativeOutcomesResults, glue(resultsWD, 'dose_3/negative outcomes/'))
negOutcomes3 <- do.call(bind_rows, negOutcomes3)
negOutcomes3 <- negOutcomes3 %>% left_join(NCO) 

# Plot of Negative Control Outcome Estimates
ggplot(bind_rows(negOutcomes12, negOutcomes3) %>% drop_na(), 
                 aes(y=reorder(OutcomeName, n_event), x=estimate, size=n_event)) +
      geom_errorbarh(aes(xmax = conf.high, xmin = conf.low), size = .5, height = .2, color = "gray50") +
      geom_point(aes(group=term), alpha = 0.8) + scale_size(range = c(0, 3)) +
      geom_vline(xintercept=1, size = .25, linetype = "dashed") +
      facet_grid(.~term) + scale_x_log10() + coord_cartesian(xlim=c(0.5,3)) + 
      theme_minimal() +
      theme(panel.grid.minor = element_blank(), text = element_text(family='Helvetica'),
            legend.position = "bottom", strip.background =element_rect(fill="lightgrey")) +
      labs(x = 'Hazard Ratio', y = 'Outcomes', size = 'Number of Events')

ggsave('Figures/NegOutcomesPlot_Figure.png', height = 150, width = 250, units = "mm", dpi = "print")

# Plotting Calibration
negOutcomes12_toPlot <- negOutcomes12 %>% filter(term == 'periodV1 14D+') %>%
      select(estimate, std.error) %>% drop_na()

v1_14 <- plotCiCalibrationEffect(log(negOutcomes12_toPlot$estimate), negOutcomes12_toPlot$std.error, rep(0,nrow(negOutcomes12_toPlot)))

negOutcomes12_toPlot <- negOutcomes12 %>% filter(term == 'periodV2 7D+') %>%
      select(estimate, std.error) %>% drop_na()

v2_7 <- plotCiCalibrationEffect(log(negOutcomes12_toPlot$estimate), negOutcomes12_toPlot$std.error, rep(0,nrow(negOutcomes12_toPlot)))

negOutcomes3_toPlot <- negOutcomes3 %>% filter(term == 'periodV3 14-60D') %>%
      select(estimate, std.error) %>% drop_na()

v3_14 <- plotCiCalibrationEffect(log(negOutcomes3_toPlot$estimate), negOutcomes3_toPlot$std.error, rep(0,nrow(negOutcomes3_toPlot)))

negOutcomes3_toPlot <- negOutcomes3 %>% filter(term == 'periodV3 60+') %>%
      select(estimate, std.error) %>% drop_na()

v3_60 <- plotCiCalibrationEffect(log(negOutcomes3_toPlot$estimate), negOutcomes3_toPlot$std.error, rep(0,nrow(negOutcomes3_toPlot)))

lay = rbind(c(1, 3), c(2, 4))

tt <- grid.arrange(v1_14, v2_7, v3_14, v3_60, 
                   layout_matrix = lay)

ggsave("Figures/NegOutcomes_CalibrationPlot.png", plot = tt, height = 260, width = 260, units = "mm", dpi = "print")

# Loading Main Outcome Results
getCalibratedResults <- function(neg_outcomes_dataframe, main_results_dataframe, term_to_filter){
      neg_outcomes_dataframe <- neg_outcomes_dataframe %>%
            filter(term == term_to_filter) %>% drop_na()
      main_results_dataframe <- main_results_dataframe %>%
            filter(term == term_to_filter) %>% drop_na()
      
      model <- fitSystematicErrorModel(log(neg_outcomes_dataframe$estimate), main_results_dataframe$std.error, rep(0,nrow(neg_outcomes_dataframe))) # use NCO to fit model
      result_full <- calibrateConfidenceInterval(log(main_results_dataframe$estimate), main_results_dataframe$std.error, model, ciWidth = 0.95) # use model to calibrate treatment estimates
      
      result_full <- result_full %>%
            mutate(term = term_to_filter) %>%
            mutate(estimate = exp(logRr), 
                   conf.low = exp(logLb95Rr),
                   conf.high = exp(logUb95Rr),
                   std.error = seLogRr,
                   type = 'calibrated_estimated') %>%
            select(term, estimate, std.error, conf.low, conf.high, type)
      
      return(result_full)
}

outcomeHR12 <- read.table(glue(resultsWD, 'dose_12/rem_main_analysis/outcome_hosp_death_period_three.csv'),  sep=';', header = T) %>%
      mutate(OutcomeName = 'COVID-19 Hosp. or Death')

outcomeHR3 <- read.table(glue(resultsWD, 'dose_3/rem_main_analysis/outcome_hosp_death_period_three.csv'),  sep=';', header = T) %>%
      mutate(OutcomeName = 'COVID-19 Hosp. or Death')

tidy_results <- bind_rows(getCalibratedResults(negOutcomes12, outcomeHR12, 'periodV1 14D+'),
          getCalibratedResults(negOutcomes12, outcomeHR12, 'periodV2 7D+'),
          getCalibratedResults(negOutcomes3, outcomeHR3, 'periodV3 14-60D'),
          getCalibratedResults(negOutcomes3, outcomeHR3, 'periodV3 60+')) %>%
      mutate(estimate_ve = if_else(estimate>1, -(1-(1/estimate))*100, (1-estimate)*100),
             conf.high_ve = if_else(conf.low>1, -(1-(1/conf.low))*100, (1-conf.low)*100),
             conf.low_ve= if_else(conf.high>1, -(1-(1/conf.high))*100, (1-conf.high)*100))

# Exporting To Excel
wb <- createWorkbook()
addWorksheet(wb, "negative_outcomes")
writeData(wb, sheet = 1, x = tidy_results)
saveWorkbook(wb, glue(resultsTidyWD, 'neg_outcomes_tidy.xlsx'), overwrite = TRUE)

             