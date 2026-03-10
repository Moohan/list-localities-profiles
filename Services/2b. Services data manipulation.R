# Services/2b. Services data manipulation.R

# This script prepares service markers at the HSCP level.
# Hoisted to the HSCP loop in Build Profiles.R to run once per partnership.

# Defensive check for HSCP (supporting create_testing_chapter)
if (!exists("HSCP") && exists("LOCALITY")) {
  lookup2 <- read_in_localities()
  HSCP <- as.character(
    dplyr::filter(lookup2, hscp_locality == LOCALITY)$hscp2019name
  )
}

# Ensure lookup2 and n_loc are available for mapping
lookup2 <- read_in_localities()
n_loc <- count_localities(lookup2, HSCP)

## GP Practices ----
# prac is loaded in 2a
markers_gp <- prac |>
  dplyr::select(
    practice_code,
    gp_practice_name,
    practice_list_size,
    postcode
  ) |>
  dplyr::mutate(
    postcode = stringr::str_replace_all(toupper(postcode), " ", "")
  ) |>
  dplyr::left_join(postcode_lkp, by = "postcode") |>
  dplyr::mutate(type = "GP Practice") |>
  dplyr::filter(hscp2019name == HSCP)

## Emergency Departments and MIUs ----
hosp_lookup <- hosp_types |>
  dplyr::filter(status == "Open") |>
  dplyr::select(
    name = treatment_location_name,
    location = treatment_location_code,
    type = current_department_type
  ) |>
  dplyr::left_join(
    dplyr::select(hosp_postcodes, hospital_code, postcode),
    by = dplyr::join_by(location == hospital_code)
  ) |>
  dplyr::mutate(
    postcode = stringr::str_replace_all(toupper(postcode), " ", "")
  ) |>
  dplyr::left_join(postcode_lkp, by = "postcode")

# MIUs
markers_miu <- hosp_lookup |>
  dplyr::filter(type == "Minor Injury Unit or Other") |>
  dplyr::filter(hscp2019name == HSCP)

# EDs
markers_emergency_dep <- hosp_lookup |>
  dplyr::filter(type == "Emergency Department") |>
  dplyr::filter(hscp2019name == HSCP)

Clacks_Royal <- dplyr::filter(
  hosp_lookup,
  name == "Forth Valley Royal Hospital"
)

# Specific fixes
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
  dplyr::mutate(
    postcode = stringr::str_replace_all(toupper(service_postcode), " ", "")
  ) |>
  dplyr::left_join(postcode_lkp, by = "postcode") |>
  dplyr::filter(hscp2019name == HSCP)
