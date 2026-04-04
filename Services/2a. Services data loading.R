# Services data loading (Global)
# Runs once at the start of the session

# Set year
ext_year <- 2025

# 1. Read in Postcode file for latitudes and longitudes ----
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

# 2. Read in all data in services folder ----
services_file_names <- fs::dir_ls(
  fs::path(lp_path, "Services", paste0("DATA ", ext_year)),
  regexp = "(?i)\\.rds$"
)

for (file in services_file_names) {
  name <- substr(x = fs::path_file(file), 1, 4)

  data <- readRDS(file) |>
    janitor::clean_names()

  assign(name, data)
}

# Change to more straightforward names
hosp_postcodes <- curr
hosp_types <- hosp
care_homes <- MDSF

rm(curr, hosp, MDSF, data, file, services_file_names, name)
gc()
