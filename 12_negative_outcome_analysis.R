library(EmpiricalCalibration)
library(here)
library(glue)
library(tidyverse)

# Setting Temporary WD
resultsWD <- '/Users/felippelazarneto/Google Drive (felippe.neto@alumni.usp.br)/SIDIAP Analysis/Results/'

# Creating Table of Negative Outcome Results
getNegativeOutcomesResults <- function(concept_id, file_location){
      
      file_full_path = glue(file_location, 'outcome_three_periods', concept_id, '.csv') 
      
      return(read.table(file_full_path, sep=';', header = T) %>%
            select(term, estimate, std.error, n_event) %>%
            mutate(ConceptId = concept_id))
}
# Getting Negative Outcomes Table
NCO <- read_csv(file=here("NCO.csv"), show_col_types = FALSE) # File with the conditions chosen to assess residual confounding

negOutcomes12 <- lapply(NCO$ConceptId, getNegativeOutcomesResults, glue(resultsWD, 'dose_12/negative outcomes/'))
negOutcomes12 <- do.call(bind_rows, negOutcomes12)
negOutcomes12 <- negOutcomes12 %>% left_join(NCO)
negOutcomes12_toPlot <- negOutcomes12 %>%
      filter(term == 'periodV2 7D+') %>%
      select(estimate, std.error) %>% drop_na()

negOutcomes3 <- lapply(NCO$ConceptId, getNegativeOutcomesResults, glue(resultsWD, 'dose_3/negative outcomes/'))
negOutcomes3 <- do.call(bind_rows, negOutcomes3)
negOutcomes3 <- negOutcomes3 %>% left_join(NCO) 
negOutcomes3_toPlot <- negOutcomes3 %>%
      filter(term == 'periodV3 14-60D') %>%
      select(estimate, std.error) %>% drop_na()

outcomeHR12 <- read.table(glue(resultsWD, 'dose_12/rem_main_analysis/outcome_hosp_death_period_three.csv'),  sep=';', header = T) %>%
      filter(term == 'periodV2 7D+') %>%
      select(estimate, std.error) %>% drop_na()

outcomeHR3 <- read.table(glue(resultsWD, 'dose_3/rem_main_analysis/outcome_hosp_death_period_three.csv'),  sep=';', header = T) %>%
      filter(term == 'periodV3 14-60D') %>%
      select(estimate, std.error) %>% drop_na()

# Estimate HR for this conditions on your population (after adjusting for observed bias). 
# They are in the table condition ocurrence. Get first ocurrence after index date (if there are)

# Names of the NCO conditions
# names <- colnames(data_NCO)[10:length(colnames(data_NCO))]

# Save survival analysis results for NCO in these tables (update RR and SD for each NCO)
toPlot_NCO  <- tibble(Outcome = names, RelativeRisk = NA, SE = NA)

write.csv(toPlot_NCO,"./Data/NCO_estimates.csv")

# Function to plot NCO results:
plotCiCalibrationEffect_NMB <- function(logRr,          # log(toPlot_NCO_before$RelativeRisk)
                                        seLogRr,        # toPlot_NCO_before$SE
                                        trueLogRr,      # rep(0,nrow(toPlot_NCO_before)
                                        legacy = FALSE,
                                        model = NULL,
                                        xLabel = "Relative risk",
                                        title,
                                        fileName = NULL) {
  alpha <- 0.05
  if (is.null(model)) {
    model <- fitSystematicErrorModel(
      logRr = logRr,
      seLogRr = seLogRr,
      trueLogRr = trueLogRr,
      estimateCovarianceMatrix = FALSE,
      legacy = legacy
    )
  } else {
    legacy <- (names(model)[3] == "logSdIntercept")
  }
  d <- data.frame(
    logRr = logRr,
    seLogRr = seLogRr,
    trueLogRr = trueLogRr,
    trueRr = exp(trueLogRr),
    logCi95lb = logRr + qnorm(0.025) * seLogRr,
    logCi95ub = logRr + qnorm(0.975) * seLogRr
  )
  d <- d[!is.na(d$logRr), ]
  d <- d[!is.na(d$seLogRr), ]
  if (nrow(d) == 0) {
    return(NULL)
  }
  d$Group <- as.factor(d$trueRr)
  d$Significant <- d$logCi95lb > d$trueLogRr | d$logCi95ub < d$trueLogRr
  
  temp1 <- aggregate(Significant ~ trueRr, data = d, length)
  temp2 <- aggregate(Significant ~ trueRr, data = d, mean)
  temp1$nLabel <- paste0(formatC(temp1$Significant, big.mark = ","), " estimates")
  temp1$Significant <- NULL
  temp2$meanLabel <- paste0(
    formatC(100 * (1 - temp2$Significant), digits = 1, format = "f"),
    "% of CIs includes ",
    temp2$trueRr
  )
  temp2$Significant <- NULL
  dd <- merge(temp1, temp2)
  
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  theme <- ggplot2::element_text(colour = "#000000", size = 10)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 10, hjust = 1)
  
  d$Group <- paste("True", tolower(xLabel), "=", d$trueRr)
  dd$Group <- paste("True", tolower(xLabel), "=", dd$trueRr)
  
  x <- seq(log(0.1), log(10), by = 0.01)
  calBounds <- data.frame()
  for (i in 1:nrow(dd)) {
    mu <- model[1] + model[2] * log(dd$trueRr[i])
    if (legacy) {
      sigma <- exp(model[3] + model[4] * log(dd$trueRr[i]))
    } else {
      sigma <- model[3] + model[4] * abs(log(dd$trueRr[i]))
    }
    calBounds <- rbind(
      calBounds,
      data.frame(
        logRr = x,
        seLogRr = logRrtoSE(x, alpha, mu, sigma),
        Group = dd$Group[i]
      )
    )
  }
  plot <- ggplot2::ggplot(d, ggplot2::aes(x = .data$logRr, y = .data$seLogRr)) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "#AAAAAA", lty = 1, size = 0.5) +
    ggplot2::geom_area(
      fill = rgb(1, 0.5, 0, alpha = 0.5),
      color = rgb(1, 0.5, 0),
      size = 1,
      alpha = 0.5, data = calBounds
    ) +
    ggplot2::geom_abline(ggplot2::aes(intercept = (-log(.data$trueRr)) / qnorm(0.025), slope = 1 / qnorm(0.025)), colour = rgb(0, 0, 0), linetype = "dashed", size = 1, alpha = 0.5, data = dd) +
    ggplot2::geom_abline(ggplot2::aes(intercept = (-log(.data$trueRr)) / qnorm(0.975), slope = 1 / qnorm(0.975)), colour = rgb(0, 0, 0), linetype = "dashed", size = 1, alpha = 0.5, data = dd) +
    ggplot2::geom_point(
      shape = 16,
      size = 2,
      alpha = 0.5,
      color = rgb(0, 0, 0.8)
    ) +
    ggplot2::geom_hline(yintercept = 0) +
    # ggplot2::geom_label(x = log(0.15), y = 0.95, alpha = 1, hjust = "left", ggplot2::aes(label = .data$nLabel), size = 3.5, data = dd) +
    # ggplot2::geom_label(x = log(0.15), y = 0.8, alpha = 1, hjust = "left", ggplot2::aes(label = .data$meanLabel), size = 3.5, data = dd) +
    ggplot2::scale_x_continuous(xLabel, limits = log(c(0.1, 10)), breaks = log(breaks), labels = breaks) +
    ggplot2::scale_y_continuous("Standard Error") +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::facet_grid(. ~ Group) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = themeRA,
      axis.text.x = theme,
      axis.title = theme,
      legend.key = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      strip.text.x = theme,
      strip.text.y = theme,
      strip.background = ggplot2::element_blank(),
      legend.position = "none"
    )
  
  if (!is.null(fileName)) {
    ggsave(width = 7, height = 4, dpi = 600,
           filename = fileName)
  }
  return(plot)
}


# EMPIRICAL CALIBRATION WITH NCO ----
toPlot_NCO # survival for NCO
outcomeHR  # survival for outcome of interest

# Calibration for 1st and 2nd dose
model <- fitSystematicErrorModel(log(negOutcomes12_toPlot$estimate), negOutcomes12_toPlot$std.error, rep(0,nrow(negOutcomes12_toPlot))) # use NCO to fit model
plotCiCalibrationEffect(log(negOutcomes12_toPlot$estimate), negOutcomes12_toPlot$std.error, rep(0,nrow(negOutcomes12_toPlot)))
result_full <- calibrateConfidenceInterval(log(outcomeHR12$estimate), outcomeHR12$std.error, model, ciWidth = 0.95) # use model to calibrate treatment estimates
outcomeHR_full_cal12 <- outcomeHR12[,1:2] %>% cbind(exp(result_full))
colnames(outcomeHR_full_cal12)[3:6] <- c("HR","low95", "upper95","SE")

# Calibration for 3rd dose
model <- fitSystematicErrorModel(log(negOutcomes3_toPlot$estimate), negOutcomes3_toPlot$std.error, rep(0,nrow(negOutcomes3_toPlot))) # use NCO to fit model
plotCiCalibrationEffect(log(negOutcomes3_toPlot$estimate), negOutcomes3_toPlot$std.error, rep(0,nrow(negOutcomes3_toPlot)))
result_full <- calibrateConfidenceInterval(log(outcomeHR3$estimate), outcomeHR3$std.error, model, ciWidth = 0.95) # use model to calibrate treatment estimates
outcomeHR_full_cal3 <- outcomeHR3[,1:2] %>% cbind(exp(result_full))
colnames(outcomeHR_full_cal3)[3:6] <- c("HR","low95", "upper95","SE")

# Results from "calibrateConfidenceInterval" are: logRr logLb95Rr logUb95Rr   seLogRr
# To get usual estimates compute the exponent (exp())
# write.csv(outcomeHR_full_cal, file = paste0('./s01_RiskEstimates/s01_calibrated_', cens, '_full.csv'))

