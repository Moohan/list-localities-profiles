############################################################################################# .
#                                                                                           #
#                         LOCALITY PROFILES SERVICES DATA LOADING                           #
#                                                                                           #
############################################################################################# .

## Code used to load global services data for locality profiles.
## This script is intended to be run ONCE globally.

###### 1. Set up ######

# Change year to be the year in the data folder name
ext_year <- 2024

###### 2. Read in services data ######

## Read in all data in services folder

services_file_names <- list.files(
  paste0(lp_path, "Services/DATA ", ext_year),
  pattern = "RDS"
)

for (file in services_file_names) {
  name <- substr(x = file, 1, 4)

  data <- readRDS(paste0(lp_path, "Services/DATA ", ext_year, "/", file)) |>
    janitor::clean_names()

  assign(name, data, envir = .GlobalEnv)
}

# Change to more straightforward names
hosp_postcodes <- curr
hosp_types <- hosp
care_homes <- MDSF

rm(curr, hosp, MDSF)

## Read in Postcode file for latitudes and longitudes
postcode_lkp <- read_in_postcodes() |>
  dplyr::mutate(postcode = gsub(" ", "", pc7, fixed = TRUE)) |>
  dplyr::select(
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
