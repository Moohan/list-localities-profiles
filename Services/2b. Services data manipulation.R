# LOCALITY PROFILES SERVICES DATA MANIPULATION
# This script prepares partnership-level service markers for maps and tables.
# It is intended to be sourced once per HSCP in the outer rendering loop.

# 1. Set up ----

# Lookup without datazones
lookup2 <- read_in_localities()

# If HSCP is not already defined (e.g. in testing), derive it from LOCALITY
if (!exists("HSCP") && exists("LOCALITY")) {
  HSCP <- lookup2 |>
    dplyr::filter(hscp_locality == LOCALITY) |>
    dplyr::pull(hscp2019name) |>
    as.character()
}

# Get number of localities in HSCP
n_loc <- count_localities(lookup2, HSCP)

# 2. Manipulate services data ----

## GP Practices ----
prac_filtered <- prac |>
  dplyr::select(practice_code, gp_practice_name, practice_list_size, postcode) |>
  dplyr::mutate(postcode = gsub(" ", "", postcode, fixed = TRUE))

# Merge practice data with postcode and locality lookups
markers_gp <- dplyr::left_join(prac_filtered, postcode_lkp, by = "postcode") |>
  dplyr::mutate(type = "GP Practice") |>
  # filter out HSCP for map
  dplyr::filter(hscp2019name == HSCP)

## Emergency Departments and MIUs ----

# create hospital lookup table
hosp_lookup <- hosp_types |>
  dplyr::filter(status == "Open") |>
  dplyr::select(
    name = treatment_location_name,
    location = treatment_location_code,
    type = current_department_type
  ) |>
  dplyr::left_join(
    dplyr::select(hosp_postcodes, location = hospital_code, postcode),
    by = "location"
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
  markers_emergency_dep <- rbind(markers_emergency_dep, Clacks_Royal)
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

# Clean up partnership-level intermediate objects
rm(prac_filtered, hosp_lookup, Clacks_Royal)
