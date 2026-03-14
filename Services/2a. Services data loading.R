# LOCALITY PROFILES SERVICES DATA LOADING
# Part of the decomposition of Services/2. Services data manipulation & table.R

# This script loads global data required for the Services chapter.
# It should be run once per session (hoisted to global level).

ext_year <- 2024

# 1. Postcode lookup ----
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

# 2. Services data ----
services_file_names <- list.files(
  fs::path(lp_path, "Services", paste0("DATA ", ext_year)),
  pattern = "RDS"
)

for (file in services_file_names) {
  name <- substr(x = file, 1, 4)

  data <- readRDS(
    fs::path(lp_path, "Services", paste0("DATA ", ext_year), file)
  ) |>
    janitor::clean_names()

  assign(name, data)
}

# Standardized names
hosp_postcodes <- curr
hosp_types <- hosp
care_homes <- MDSF

rm(curr, hosp, MDSF, data, file, services_file_names)
