library(tidyverse)
library(openxlsx)
library(here)
library(readxl)

#-- Creating Formatting Options
arial_whitebg_bordered <- createStyle(fontName = 'Arial', fontSize = 10,
                                      valign = 'center', halign = 'center', 
                                      fgFill = '#FFFFFF', 
                                      border = "TopBottomLeftRight",
                                      wrapText = T)

header_gray_bordered <- createStyle(fontName = 'Arial', fontSize = 12, textDecoration = "bold",
                                    valign = 'center', halign = 'center', 
                                    fgFill = '#D3D3D3', 
                                    border = "TopBottomLeftRight",
                                    wrapText = T, )

whitebg_bordered <- createStyle(fontName = 'Arial', fontSize = 11,
                                valign = 'center', halign = 'center', 
                                fgFill = '#FFFFFF', 
                                border = "TopBottomLeftRight",
                                numFmt = "0.000",
                                wrapText = T)

lightgraybg_bordered <- createStyle(fontName = 'Arial', fontSize = 11,
                                    valign = 'center', halign = 'center', 
                                    fgFill = '#e4e9f2', 
                                    border = "TopBottomLeftRight",
                                    numFmt = "0.000",
                                    wrapText = T)

whitebg_bordered_left <- createStyle(fontName = 'Arial', fontSize = 11,
                                valign = 'center', halign = 'left', 
                                fgFill = '#FFFFFF', 
                                border = "TopBottomLeftRight",
                                numFmt = "0.000",
                                wrapText = T)

lightgraybg_bordered_left <- createStyle(fontName = 'Arial', fontSize = 11,
                                    valign = 'center', halign = 'left', 
                                    fgFill = '#e4e9f2', 
                                    border = "TopBottomLeftRight",
                                    numFmt = "0.000",
                                    wrapText = T)


adjust_names <- function(var_name, var_names_recode){
  string_regex = paste0('(', paste(names(var_names_recode), collapse = '|'), ')')
  if(is.na(var_name)){return(var_name)}
  if(str_detect(var_name, string_regex)){
    old_var <- str_extract(var_name, string_regex, group = 1)
    return(var_names_recode[[old_var]])
  } else (return(var_name))
}

max_char <- function(nchar_vec){
  max_char <- max(nchar(nchar_vec), na.rm = T)
  return(max_char)}

get_width <- function(max_char){
  nmax <- 40
  if(max_char > nmax){return(nmax*1)}
  else(return(max_char*1.1))}

addStyledSheet <- function(workbook, sheet_name, temp_table, new_label = NULL, adjust_table = NULL){
      
      if(!is.null(new_label)){
            temp_table <- data.frame(var_names = row.names(temp_table), temp_table)
      }
      
      if(!is.null(new_label)){
            temp_table <- as.data.frame(do.call(cbind, lapply(temp_table, map_chr, adjust_names, new_label)))
      }
      
      # Getting Dimensions
      n_row = dim(temp_table)[1]
      n_col = dim(temp_table)[2]
      
      # Creating Worksheet
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
      
      # Getting Col Widths
      col_widths <- temp_table %>% sapply(max_char) %>% sapply(get_width)
      col_numbers <- seq_along(temp_table)
      
      # Setting ColWidths and RowHeights
      setColWidths(workbook, sheet_name, cols = col_numbers, widths = col_widths)
      setRowHeights(workbook, sheet_name, rows = 2:max_row, heights = 18)
      
      return(workbook)
      
}
      
create_forest_table_subgroup <- function(file_forest){
      
      temp_table <- read_delim(file_forest, delim = ';', locale = locale(decimal_mark = "."))
      temp_table <- temp_table %>% select(model_interaction_var, term, contrast) %>%
            cbind(temp_table %>% select(-model_interaction_var, -term, -contrast) %>%
                        mutate_all(as.numeric))
      
      forest_table <- temp_table %>%
            mutate(estimate = exp(estimate),
                   asymp.UCL = exp(asymp.UCL),
                   asymp.LCL = exp(asymp.LCL)) %>%
            mutate(estimate_ve = if_else(estimate > 1, 
                                         -(1-(1/estimate))*100, (1-estimate )*100),
                   conf.low_ve = if_else(asymp.LCL  > 1, 
                                         -(1-(1/asymp.LCL))*100, (1-asymp.LCL)*100),
                   conf.high_ve= if_else(asymp.UCL > 1, 
                                         -(1-(1/asymp.UCL))*100, (1-asymp.UCL)*100)) %>%
            mutate(est_ve.conf.interval = sprintf('%.1f%% (%.1f - %.1f)', estimate_ve, conf.low_ve, conf.high_ve)) %>%
            rename(mean = estimate_ve,
                   upper = conf.low_ve,
                   lower = conf.high_ve) %>%
            mutate(model_p_value = sprintf('%.3f', model_p_value)) %>%
            mutate(contrast = case_when(
                  contrast == '(V1 14D+) - (no-vax)' ~ 'Partially Vaccinated',
                  contrast == '(V2 7D+) - (no-vax)' ~ 'Fully Vaccinated',
                  contrast == '(V3 14-60D) - (no-vax)' ~ 'Booster 14 - 60 days',
                  contrast == '(V3 60+) - (no-vax)' ~ 'Booster 60 days+'
            )) %>%
            filter(!is.na(contrast))
      
      return(forest_table)
}

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
            select(outcome, periods, n_event, exposure,
                   term, estimate, conf.low, conf.high, p.value, est.conf.interval,
                   estimate_ve, conf.low_ve, conf.high_ve, est_ve.conf.interval) %>%
            rename(mean = estimate_ve,
                   lower = conf.high_ve,
                   upper = conf.low_ve) %>%
            mutate(p.value = sprintf('%.3f', p.value)) %>%
            mutate(exposure = format(exposure, big.mark=","))
      
      
      return(forest_table)
}

do_nothing <- function(obj){return(obj)}

create_forest_table_sens_results <- function(files_forest){
      
      # Getting Files Results
      temp_table <- read.csv(files_forest, row.names=NULL, header = T, sep = ';') %>% 
            mutate(analysis = files_forest)
      
      # Binding Tables
      temp_table <- temp_table %>%
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
            select(analysis, n_event, exposure,
                   term, estimate, conf.low, conf.high, p.value, est.conf.interval,
                   estimate_ve, conf.low_ve, conf.high_ve, est_ve.conf.interval) %>%
            rename(mean = estimate_ve,
                   lower = conf.high_ve,
                   upper = conf.low_ve) %>%
            mutate(p.value = sprintf('%.3f', p.value)) %>%
            mutate(exposure = format(exposure, big.mark=","))
      
      
      return(forest_table)
}
      