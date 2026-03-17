# LOCALITY PROFILES SERVICES: DATA LOADING (Global Level)
# This script loads global services data once per session.

# ext_year should be defined in the main render script (Build Profiles.R or excel_output.R)
# lp_path should also be defined in the main render script

# Read in Postcode file for latitudes and longitudes
postcode_lkp <- read_in_postcodes() |>
  dplyr::mutate(postcode = stringr::str_replace_all(pc7, stringr::fixed(" "), "")) |>
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

# Read in all data in services folder
services_file_names <- fs::dir_ls(
  fs::path(lp_path, "Services", paste0("DATA ", ext_year)),
  regexp = "RDS$"
)

for (file in services_file_names) {
  name <- stringr::str_sub(fs::path_file(file), 1, 4)
  data <- readr::read_rds(file) |>
    janitor::clean_names()

  # Assigning to names based on original logic
  if (name == "curr") {
    hosp_postcodes <- data
  } else if (name == "hosp") {
    hosp_types <- data
  } else if (name == "MDSF") {
    care_homes <- data
  } else {
    assign(name, data)
  }
}

rm(data, name, file, services_file_names)
