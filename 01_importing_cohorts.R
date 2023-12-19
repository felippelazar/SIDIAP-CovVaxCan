# ============================================================================ #
# 1. Importing Cohorts  #
# Author: Felippe Lazar, IDIAP Jordi Gol, 2023 #
# ============================================================================ #

# Setting Up Database Connection
# Installing Packages (for all analysis)
pckgs_installed <- installed.packages()[, 'Package'] |> unname()
pckgs_needed <- c("DBI", "RPostgres", "CDMConnector", "tidyverse", "readxl", "here", 
                   "gtsummary", "tidylog", "survminer", "tableone", "survival", "remotes",
                  "broom.helpers", "emmeans", "tidycmprsk", "ggsurvfit", "glue", 
                  "EmpiricalCalibration", "openxlsx", "ggplot2", "lubridate", "grid", "gridExtra", 
                  "egg", "forestplot")

pckgsToInstall <- pckgs_needed[!pckgs_needed %in% pckgs_installed]
install.packages(pckgsToInstall)
if(!'CirceR' %in% pckgs_installed){remotes::install_github("ohdsi/CirceR")}

# Connecting Database 
library(DBI)
library(RPostgres)
library(CDMConnector)
library(tidyverse)
library(readxl)
library(here)
library(CirceR)

# Creating Variables in Environment
# usethis::edit_r_environ()

# Setting Connection - Credentials and Database 
db <- dbConnect(RPostgres::Postgres(),
                dbname = Sys.getenv('DB_SERVER_DATABASE'),
                port = Sys.getenv('DB_PORT'),
                host = Sys.getenv('DB_HOST'),
                user = Sys.getenv('DB_USER'),
                password = Sys.getenv('DB_PASSWORD'))

# Setting Up Connection and Retrieving Tables and Data
cdm_database_schema <-"omop22t2_cmbd"
vocabulary_database_schema <-"omop22t2_cmbd"
results_database_schema <- "results22t2_cmbd"


# Setting Dialect Variable for Future Use
cdm <- CDMConnector::cdm_from_con(con = db,
                                  cdm_schema = cdm_database_schema,
                                  write_schema = results_database_schema)

# Reading JSON Files
json_files <- readCohortSet(here('Cohort Definitions', 'JSON'))

# Creating Table Name for Exports
tableResults <- 'covcanvac_cohorts'

cdm <- generateCohortSet(cdm,
                         json_files,
                         name = tableResults,
                         overwrite = TRUE
                         ) 

# cdm <- CDMConnector::cdm_from_con(con = db,
#                                   cdm_schema = cdm_database_schema,
#                                   write_schema = results_database_schema,
#                                   cohort_tables = tableResults)

cdm[[tableResults]]

DBI::dbDisconnect(db)
