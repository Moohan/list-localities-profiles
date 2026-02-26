##################### LOCALITY PROFILES UNSCHEDULED CARE: DATA EXTRACTION ######################.

# Original author: Will Clayton
# Updated Oct/Nov 2022 by Adam Rennie to use Global Script functions
# Last edits December 2022 by C Puech to improve data quality and process
# All indicators for which MSG files are available now directly use MSG data

####################### SECTION 1: Packages, file paths, lookups, etc #########################

## Manually set year that the profiles are being run (extract year)
ext_year <- 2024

## Manually set the name of the latest MSG folder
latest_msg_folder <- "2024-12 December"

# Set locality profiles file path
# lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

## Packages
library(lubridate)
library(odbc)

## Functions
source("Master RMarkdown Document & Render Code/Global Script.R")

# Read/write permissions
# Sys.umask("006")

# Sys.getenv("R_ZIPCMD", "zip")

# Folder to export to
exportfolder <- paste0(lp_path, "Unscheduled Care/DATA ", ext_year, "/")


## Lookups ----

# Postcodes
postcodes <- read_in_postcodes() %>%
  rename(postcode = pc8)

# Localities/Datazones
datazones <- read_in_localities(dz_level = TRUE)
localities <- read_in_localities()


########################## SECTION 2: MSG Data ###############################

source("Unscheduled Care/1a. MSG Extraction.R")
source("Unscheduled Care/1b. SMR Extraction.R")
