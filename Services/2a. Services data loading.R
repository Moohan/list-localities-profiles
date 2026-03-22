# LOCALITY PROFILES SERVICES DATA LOADING
# Run once per session (Global level)

# Load Postcode file for latitudes and longitudes
postcode_lkp <- read_in_postcodes() |>
  mutate(postcode = gsub(" ", "", pc7, fixed = TRUE)) |>
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
# Ensure ext_year and lp_path are defined in the environment
services_file_names <- list.files(
  path(lp_path, "Services", paste0("DATA ", ext_year)),
  pattern = "(?i)\\.rds$"
)

for (file in services_file_names) {
  name <- substr(x = file, 1, 4)

  data <- readRDS(path(lp_path, "Services", paste0("DATA ", ext_year), file)) |>
    clean_names()

  assign(name, data, envir = .GlobalEnv)
}

# Change to more straightforward names
hosp_postcodes <- curr
hosp_types <- hosp
care_homes <- MDSF

# Clean up temporary names
rm(curr, hosp, MDSF)
