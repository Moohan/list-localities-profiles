# LOCALITY PROFILES SERVICES DATA LOADING
# Part of the Services chapter refactoring to improve performance

# 1. Set up ----

# Change year to be the year in the data folder name
ext_year <- 2024

# 2. Read in services data ----

## Read in Postcode file for latitudes and longitudes
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
services_file_names <- list.files(
  paste0(lp_path, "Services/DATA ", ext_year),
  pattern = "RDS"
)

for (file in services_file_names) {
  name <- substr(x = file, 1, 4)

  data <- readRDS(paste0(lp_path, "Services/DATA ", ext_year, "/", file)) |>
    janitor::clean_names()

  assign(name, data)
}

# Change to more straightforward names
hosp_postcodes <- curr
hosp_types <- hosp
care_homes <- MDSF

# 3. Initial manipulation ----

prac <- prac |>
  dplyr::select(practice_code, gp_practice_name, practice_list_size, postcode) |>
  dplyr::mutate(postcode = gsub(" ", "", postcode, fixed = TRUE))

# create hospital lookup table
hosp_lookup <- hosp_types |>
  dplyr::filter(status == "Open") |>
  dplyr::select(
    name = treatment_location_name,
    location = treatment_location_code,
    type = current_department_type
  ) |>
  dplyr::left_join(
    dplyr::select(hosp_postcodes |> dplyr::rename(location = hospital_code), location, postcode),
    by = dplyr::join_by(location)
  ) |>
  dplyr::mutate(postcode = gsub(" ", "", postcode, fixed = TRUE)) |>
  dplyr::left_join(postcode_lkp, by = "postcode")

# Cleanup global data loading objects
rm(curr, hosp, MDSF, data, file, services_file_names)
