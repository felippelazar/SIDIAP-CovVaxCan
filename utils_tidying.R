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
      
      