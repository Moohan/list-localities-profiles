# Services data manipulation and mapping (HSCP level)
# Runs once per HSCP in the outer loop

# 1. Determine HSCP context ----
if (!exists("HSCP")) {
  HSCP <- as.character(
    dplyr::filter(lookup, hscp_locality == LOCALITY)$hscp2019name
  )
}

# Lookup without datazones
lookup2 <- read_in_localities()

# Get number of localities in HSCP
n_loc <- count_localities(lookup2, HSCP)

# 2. Manipulate services data ----

## GP Practices ----
prac_clean <- prac |>
  dplyr::select(
    practice_code,
    gp_practice_name,
    practice_list_size,
    postcode
  ) |>
  dplyr::mutate(postcode = gsub(" ", "", postcode, fixed = TRUE))

# Merge practice data with postcode and locality lookups
markers_gp <- dplyr::left_join(
  prac_clean,
  postcode_lkp,
  by = "postcode",
  relationship = "many-to-one"
) |>
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
    by = "location",
    relationship = "many-to-many"
  ) |>
  dplyr::mutate(postcode = gsub(" ", "", postcode, fixed = TRUE)) |>
  dplyr::left_join(
    postcode_lkp,
    by = "postcode",
    relationship = "many-to-many"
  )

# MIUs
markers_miu <- hosp_lookup |>
  dplyr::filter(
    type == "Minor Injury Unit or Other",
    hscp2019name == HSCP
  )

# EDs
markers_emergency_dep <- hosp_lookup |>
  dplyr::filter(
    type == "Emergency Department",
    hscp2019name == HSCP
  )

clacks_royal_marker <- hosp_lookup |>
  dplyr::filter(name == "Forth Valley Royal Hospital")

# Ninewells hospital is incorrectly mapped even though postcode ok - so corrected coords here
if (HSCP == "Dundee City") {
  markers_emergency_dep <- markers_emergency_dep |>
    dplyr::mutate(
      latitude = dplyr::if_else(latitude == 56.4617, 56.4659308, latitude),
      longitude = dplyr::if_else(
        longitude == -2.991432,
        -3.0378506,
        longitude
      )
    )
}

if (HSCP == "Clackmannanshire & Stirling") {
  markers_emergency_dep <- dplyr::bind_rows(
    markers_emergency_dep,
    clacks_royal_marker
  )
}

## Care Homes ----
markers_care_home <- care_homes |>
  dplyr::select(
    type = care_service,
    subtype,
    name = service_name,
    service_postcode
  ) |>
  dplyr::filter(
    type == "Care Home Service",
    subtype == "Older People"
  ) |>
  dplyr::mutate(postcode = gsub(" ", "", service_postcode, fixed = TRUE)) |>
  dplyr::left_join(
    postcode_lkp,
    by = "postcode",
    relationship = "many-to-one"
  ) |>
  dplyr::filter(hscp2019name == HSCP)

# 3. Create map ----
# This logic is hoisted from script 3.
source("Services/3. Service HSCP map.R")

# Cleanup HSCP-level intermediate objects
rm(prac_clean, hosp_lookup, clacks_royal_marker)
gc()
