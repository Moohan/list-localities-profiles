# LOCALITY PROFILES SERVICES HSCP DATA MANIPULATION
# Part of the Services chapter refactoring to improve performance

# 1. HSCP specific manipulation ----

# Ensure HSCP is defined if not already (e.g. for testing)
if (!exists("HSCP")) {
  HSCP <- as.character(dplyr::filter(read_in_localities(), hscp_locality == LOCALITY)$hscp2019name)
}

# Get number of localities in HSCP
lookup2 <- read_in_localities()
n_loc <- count_localities(lookup2, HSCP)

## GP Practices ----

# Merge practice data with postcode and locality lookups
markers_gp <- dplyr::left_join(prac, postcode_lkp, by = "postcode") |>
  dplyr::mutate(type = "GP Practice") |>
  # filter out HSCP for map
  dplyr::filter(hscp2019name == HSCP)

## Emergency Departments and MIUs ----

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
      latitude = if_else(latitude == 56.4617, 56.4659308, latitude),
      longitude = if_else(longitude == -2.991432, -3.0378506, longitude)
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
