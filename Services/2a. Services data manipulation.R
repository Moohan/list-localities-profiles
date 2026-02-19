# LOCALITY PROFILES SERVICES DATA MANIPULATION CODE
# Code used to manipulate services data for locality profiles.

# 0. Set up ----

# Change year to be the year in the data folder name
ext_year <- 2024

# Geographical lookups and objects ----

# Locality lookup
lookup <- read_in_localities(dz_level = TRUE)

# Lookup without datazones
lookup2 <- read_in_localities()

# Determine HSCP if not already defined (useful for testing)
if (!exists("HSCP") && exists("LOCALITY")) {
  HSCP <- lookup2 |>
    dplyr::filter(hscp_locality == LOCALITY) |>
    dplyr::pull(hscp2019name) |>
    as.character()
}

# Get number of localities in HSCP
n_loc <- count_localities(lookup2, HSCP)

# 1. Read in services data ----

# Read in Postcode file for latitudes and longitudes
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
services_data_path <- fs::path(lp_path, "Services", paste0("DATA ", ext_year))
services_file_names <- fs::dir_ls(services_data_path, regexp = "\\.RDS$")

for (file_path in services_file_names) {
  file_name <- fs::path_file(file_path)
  name <- substr(file_name, 1, 4)

  data <- readRDS(file_path) |>
    janitor::clean_names()

  assign(name, data)
}

# Change to more straightforward names
hosp_postcodes <- curr
hosp_types <- hosp
care_homes <- MDSF

rm(curr, hosp, MDSF)

# 2. Manipulate services data ----

## GP Practices ----

prac <- prac |>
  dplyr::select(practice_code, gp_practice_name, practice_list_size, postcode) |>
  dplyr::mutate(postcode = gsub(" ", "", postcode, fixed = TRUE))

# Merge practice data with postcode and locality lookups
markers_gp <- dplyr::left_join(prac, postcode_lkp, by = "postcode") |>
  dplyr::mutate(type = "GP Practice") |>
  # filter out HSCP for map
  dplyr::filter(hscp2019name == HSCP)

## Emergency Departments and MIUs ----

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
  dplyr::mutate(postcode = gsub(" ", "", postcode, fixed = TRUE)) |>
  dplyr::left_join(postcode_lkp, by = "postcode")

# MIUs
markers_miu <- hosp_lookup |>
  dplyr::filter(type == "Minor Injury Unit or Other") |>
  dplyr::filter(hscp2019name == HSCP)

# EDs
markers_emergency_dep <- hosp_lookup |>
  dplyr::filter(type == "Emergency Department") |>
  dplyr::filter(hscp2019name == HSCP)

Clacks_Royal <- dplyr::filter(hosp_lookup, name == "Forth Valley Royal Hospital")

# Ninewells hospital is incorrectly mapped even though postcode ok - so corrected coords here
if (HSCP == "Dundee City") {
  markers_emergency_dep <- markers_emergency_dep |>
    dplyr::mutate(
      latitude = dplyr::if_else(latitude == 56.4617, 56.4659308, latitude),
      longitude = dplyr::if_else(longitude == -2.991432, -3.0378506, longitude)
    )
}

if (HSCP == "Clackmannanshire & Stirling") {
  markers_emergency_dep <- dplyr::bind_rows(markers_emergency_dep, Clacks_Royal)
}

## Care Homes ----

markers_care_home <- care_homes |>
  dplyr::select(
    type = care_service,
    subtype,
    name = service_name,
    service_postcode
  ) |>
  dplyr::filter(type == "Care Home Service") |>
  dplyr::filter(subtype == "Older People") |>
  dplyr::mutate(postcode = gsub(" ", "", service_postcode, fixed = TRUE)) |>
  dplyr::left_join(postcode_lkp, by = "postcode") |>
  dplyr::filter(hscp2019name == HSCP)

# Subset care which is not Elderly care for table
other_care_type_hscp <- care_homes |>
  dplyr::select(
    type = care_service,
    subtype,
    name = service_name,
    service_postcode
  ) |>
  dplyr::filter(type == "Care Home Service") |>
  dplyr::filter(subtype != "Older People") |>
  dplyr::mutate(postcode = gsub(" ", "", service_postcode, fixed = TRUE)) |>
  dplyr::left_join(postcode_lkp, by = "postcode") |>
  dplyr::filter(hscp2019name == HSCP)

# Cleanup Intermediate Objects ----
rm(
  care_homes,
  Clacks_Royal,
  data,
  hosp_lookup,
  hosp_postcodes,
  hosp_types,
  postcode_lkp,
  prac,
  services_file_names,
  services_data_path,
  file_path,
  file_name,
  name
)
gc()
