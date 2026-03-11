# LOCALITY PROFILES SERVICES DATA LOADING
# This script loads global services data used across all partnerships and localities.
# It is intended to be sourced once at the beginning of the reporting process.

# 1. Set up ----
ext_year <- 2024

# 2. Read in services data ----

## Read in Postcode file for latitudes and longitudes
# This uses the memoised function from Global Script.R
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

## Read in all data in services folder
# This loop loads several RDS files and assigns them to objects
# based on the first 4 chars of the filename.
services_file_names <- list.files(
  fs::path(lp_path, paste0("Services/DATA ", ext_year)),
  pattern = "RDS"
)

for (file in services_file_names) {
  name <- substr(x = file, 1, 4)

  data <- readRDS(fs::path(
    lp_path,
    paste0("Services/DATA ", ext_year),
    file
  )) |>
    janitor::clean_names()

  assign(name, data)
}

# Change to more straightforward names
hosp_postcodes <- curr
hosp_types <- hosp
care_homes <- MDSF

# Clean up temporary loading objects
rm(curr, hosp, MDSF, data, file, services_file_names)
