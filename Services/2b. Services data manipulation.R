# LOCALITY PROFILES SERVICES DATA MANIPULATION
# Part of the decomposition of Services/2. Services data manipulation & table.R

# This script filters service data to the partnership (HSCP) level.
# It should be run once per HSCP (hoisted to HSCP loop).

# 1. Geographical lookups ----
lookup <- read_in_localities(dz_level = TRUE)
lookup2 <- read_in_localities()

# Determine HSCP if not already defined (useful for testing single locality)
if (!exists("HSCP")) {
  HSCP <- as.character(
    dplyr::filter(lookup2, hscp_locality == LOCALITY)[["hscp2019name"]]
  )
}

n_loc <- count_localities(lookup2, HSCP)

# 2. GP Practices ----
prac <- prac |>
  dplyr::select(
    practice_code,
    gp_practice_name,
    practice_list_size,
    postcode
  ) |>
  dplyr::mutate(postcode = gsub(" ", "", postcode, fixed = TRUE))

markers_gp <- dplyr::left_join(prac, postcode_lkp, by = "postcode") |>
  dplyr::mutate(type = "GP Practice") |>
  dplyr::filter(hscp2019name == HSCP)

# 3. Emergency Departments and MIUs ----
hosp_postcodes <- dplyr::rename(hosp_postcodes, location = hospital_code)

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

markers_miu <- hosp_lookup |>
  dplyr::filter(type == "Minor Injury Unit or Other") |>
  dplyr::filter(hscp2019name == HSCP)

markers_emergency_dep <- hosp_lookup |>
  dplyr::filter(type == "Emergency Department") |>
  dplyr::filter(hscp2019name == HSCP)

Clacks_Royal <- dplyr::filter(
  hosp_lookup,
  name == "Forth Valley Royal Hospital"
)

if (HSCP == "Dundee City") {
  markers_emergency_dep <- markers_emergency_dep |>
    dplyr::mutate(
      latitude = dplyr::if_else(
        latitude == 56.4617, 56.4659308, latitude
      ),
      longitude = dplyr::if_else(
        longitude == -2.991432, -3.0378506, longitude
      )
    )
}

if (HSCP == "Clackmannanshire & Stirling") {
  markers_emergency_dep <- rbind(markers_emergency_dep, Clacks_Royal)
}

# 4. Care Homes ----
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
