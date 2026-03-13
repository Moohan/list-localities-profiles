# Services data loading (Global level)
# This script loads the raw services data once per session.

ext_year <- 2024

# Read in Postcode file for latitudes and longitudes
# We apply the postcode cleaning once here to avoid repeating it in loops.
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

# Read in all data in services folder
services_file_names <- list.files(
  fs::path(lp_path, "Services", paste0("DATA ", ext_year)),
  pattern = "RDS"
)

for (file in services_file_names) {
  name <- substr(x = file, 1, 4)

  data <- readRDS(fs::path(lp_path, "Services", paste0("DATA ", ext_year), file)) |>
    janitor::clean_names()

  # Assign to global environment so they persist across loop iterations
  assign(name, data, envir = .GlobalEnv)
}

# Change to more straightforward names
hosp_postcodes <- get("curr")
hosp_types <- get("hosp")
care_homes <- get("MDSF")

rm(curr, hosp, MDSF, data, file, name, services_file_names)
