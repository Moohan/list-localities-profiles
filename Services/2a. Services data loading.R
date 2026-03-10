# Services/2a. Services data loading.R

# This script loads global services datasets once per session.
# It is hoisted to the top level of Build Profiles.R to avoid redundant I/O.

ext_year <- 2024

# Prepare postcode lookup with standardized postcodes for joining
postcode_lkp <- read_in_postcodes() |>
  dplyr::mutate(postcode = stringr::str_replace_all(toupper(pc7), " ", "")) |>
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

# Load RDS files from the versioned data folder
services_data_path <- fs::path(lp_path, "Services", paste0("DATA ", ext_year))
services_files <- fs::dir_ls(services_data_path, regexp = "\\.RDS$")

for (f in services_files) {
  # Use first 4 chars of filename as object name (e.g., 'curr', 'hosp', 'MDSF')
  obj_name <- substr(fs::path_file(f), 1, 4)

  data <- readRDS(f) |>
    janitor::clean_names()

  assign(obj_name, data)
}

# Rename to more descriptive objects
hosp_postcodes <- curr
hosp_types <- hosp
care_homes <- MDSF

# Clean up temporary assignment objects
rm(curr, hosp, MDSF)
