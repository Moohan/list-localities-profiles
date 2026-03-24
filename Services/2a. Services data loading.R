# Services data loading
# Loaded once per session in the outer loop (e.g., in Build Profiles.R)

# Change year to be the year in the data folder name
ext_year <- 2024

## Read in Postcode file for latitudes and longitudes
postcode_lkp <- read_in_postcodes() |>
  mutate(postcode = toupper(gsub(" ", "", pc7, fixed = TRUE))) |>
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

## Read in all data in services folder
services_file_names <- fs::dir_ls(
  fs::path(lp_path, "Services", glue("DATA {ext_year}")),
  regexp = "(?i)\\.rds$"
)

for (file in services_file_names) {
  name <- substr(x = fs::path_file(file), 1, 4)

  data <- readr::read_rds(file) |>
    janitor::clean_names()

  assign(name, data)
}

# Change to more straightforward names
hosp_postcodes <- curr
hosp_types <- hosp
care_homes <- MDSF

rm(curr, hosp, MDSF)
