# Services data loading (Global level)
# This script loads the raw services data and postcodes once per session.

ext_year <- 2024

# Postcode lookup with pre-sanitized postcodes from Global Script.R
postcode_lkp <- read_in_postcodes() %>%
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

# Read in all data in services folder
services_file_names <- list.files(
  paste0(lp_path, "Services/DATA ", ext_year),
  pattern = "RDS"
)

for (file in services_file_names) {
  name <- substr(x = file, 1, 4)
  data <- readRDS(paste0(lp_path, "Services/DATA ", ext_year, "/", file)) %>%
    clean_names()
  assign(name, data, envir = .GlobalEnv)
}

# Change to more straightforward names
hosp_postcodes <- curr
hosp_types <- hosp
care_homes <- MDSF

rm(curr, hosp, MDSF)
