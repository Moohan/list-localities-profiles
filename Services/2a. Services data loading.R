# LOCALITY PROFILES SERVICES DATA LOADING
# Run once per session

# Change year to be the year in the data folder name
ext_year <- 2024

# Postcode file for latitudes and longitudes
postcode_lkp <- read_in_postcodes() |>
  dplyr::mutate(
    postcode = stringr::str_replace_all(pc7, stringr::fixed(" "), "")
  ) |>
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
# Use (?i) to ensure case-insensitivity for .rds vs .RDS
services_file_names <- fs::dir_ls(
  fs::path(lp_path, "Services", paste0("DATA ", ext_year)),
  regexp = "(?i)\\.rds$"
)

for (file in services_file_names) {
  name <- substr(fs::path_file(file), 1, 4)

  data <- readRDS(file) |>
    janitor::clean_names()

  assign(name, data)
}

# Change to more straightforward names
hosp_postcodes <- curr
hosp_types <- hosp
care_homes <- MDSF

rm(curr, hosp, MDSF, file, name, data, services_file_names)
