############################################################################################# .
#                                                                                           #
#                         LOCALITY PROFILES SERVICES DATA LOADING                           #
#                                                                                           #
############################################################################################# .

## Code used to load global services data for locality profiles.
# This script should be sourced once per session.

## Written by C.Puech
## Created on 24/02/2020
## Latest update August 2022 - rewrote parts of code for smoother process
## Tiered Refactoring Nov 2024 - Bolt ⚡ (Performance Optimization)

###### 1. Set up ######

# Change year to be the year in the data folder name
ext_year <- 2024

###### 2. Read in services data ######

## Read in Postcode file for latitudes and longitudes
# This is a large parquet file and is now loaded once globally.
# We also apply a standard transformation once.

postcode_lkp_global <- read_in_postcodes() %>%
  mutate(postcode = gsub(" ", "", pc7, fixed = TRUE)) %>%
  select(
    postcode,
    grid_reference_easting,
    grid_reference_northing,
    latitude,
    longitude,
    datazone2011,
    hscp_locality,
    hscp2019name,
    hscp2019,
    hb2019name,
    hb2019
  )


## Read in all data in services folder
# These are loaded once per session and stored in global objects.

services_file_names <- list.files(
  paste0(lp_path, "Services/DATA ", ext_year),
  pattern = "RDS"
)

for (file in services_file_names) {
  name <- substr(x = file, 1, 4)

  data <- readRDS(paste0(lp_path, "Services/DATA ", ext_year, "/", file)) %>%
    clean_names()

  assign(name, data)
}

# Change to more straightforward names
hosp_postcodes_global <- curr
hosp_types_global <- hosp
care_homes_global <- MDSF

rm(curr, hosp, MDSF, services_file_names, file, data, name)
