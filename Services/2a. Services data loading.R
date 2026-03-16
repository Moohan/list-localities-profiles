############################################################################################# .
#                                                                                           #
#                         LOCALITY PROFILES SERVICES DATA LOADING                           #
#                                                                                           #
############################################################################################# .

## Code used to load services data for locality profiles.
## This script is intended to be run once per session (Global level).

# Change year to be the year in the data folder name
ext_year <- 2024

###### 1. Read in services data ######

## Read in Postcode file for latitudes and longitudes
# Transformation: remove spaces, ensure uppercase to match service data postcodes
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


## Read in all data in services folder

services_file_names <- list.files(
  fs::path(lp_path, "Services", paste0("DATA ", ext_year)),
  pattern = "RDS"
)

for (file in services_file_names) {
  name <- substr(x = file, 1, 4)

  data <- readRDS(fs::path(lp_path, "Services", paste0("DATA ", ext_year), file)) |>
    janitor::clean_names()

  assign(name, data)
}

# Change to more straightforward names
hosp_postcodes <- curr
hosp_types <- hosp
care_homes <- MDSF

rm(curr, hosp, MDSF, data, file, services_file_names)

###### 2. Global Preparations ######

## GP Practices ----

prac <- prac |>
  dplyr::select(practice_code, gp_practice_name, practice_list_size, postcode) |>
  dplyr::mutate(postcode = stringr::str_replace_all(toupper(postcode), " ", ""))

## Hospital Lookup (EDs and MIUs) ----

# rename for hospital_code to location
hosp_postcodes <- dplyr::rename(hosp_postcodes, location = hospital_code)

# create hospital lookup table
hosp_lookup <- hosp_types |>
  dplyr::filter(status == "Open") |>
  dplyr::select(
    name = treatment_location_name,
    location = treatment_location_code,
    type = current_department_type
  ) |>
  dplyr::left_join(
    dplyr::select(hosp_postcodes, location, postcode),
    by = dplyr::join_by(location)
  ) |>
  dplyr::mutate(postcode = stringr::str_replace_all(toupper(postcode), " ", "")) |>
  dplyr::left_join(postcode_lkp, by = "postcode")
