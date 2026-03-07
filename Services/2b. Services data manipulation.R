############################################################################################# .
#                                                                                           #
#                         LOCALITY PROFILES SERVICES DATA MANIPULATION                      #
#                                                                                           #
############################################################################################# .

## Code used to manipulate services data for locality profiles.
## Run once per HSCP.

# Ensure lookup2 (non-dz level lookup) is available
lookup2 <- read_in_localities()

## Determine HSCP
if (!exists("HSCP") && exists("LOCALITY")) {
  HSCP <- as.character(dplyr::filter(lookup2, hscp_locality == LOCALITY)[[
    "hscp2019name"
  ]])
}

# Get number of localities in HSCP
n_loc <- count_localities(lookup2, HSCP)

###### 3. Manipulate services data ######

## GP Practices ----

prac_markers <- prac |>
  dplyr::select(
    practice_code,
    gp_practice_name,
    practice_list_size,
    postcode
  ) |>
  dplyr::mutate(postcode = gsub(" ", "", postcode, fixed = TRUE))

# Merge practice data with postcode and locality lookups
markers_gp <- dplyr::left_join(prac_markers, postcode_lkp, by = "postcode") |>
  dplyr::mutate(type = "GP Practice") |>
  # filter out HSCP for map
  dplyr::filter(hscp2019name == HSCP)

## Emergency Departments and MIUs ----

# rename for hospital_code to location
hosp_postcodes_markers <- dplyr::rename(
  hosp_postcodes,
  location = hospital_code
)

# create hospital lookup table
hosp_lookup <- hosp_types |>
  dplyr::filter(status == "Open") |>
  dplyr::select(
    name = treatment_location_name,
    location = treatment_location_code,
    type = current_department_type
  ) |>
  dplyr::left_join(
    dplyr::select(hosp_postcodes_markers, location, postcode),
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

Clacks_Royal <- dplyr::filter(
  hosp_lookup,
  name == "Forth Valley Royal Hospital"
)

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

rm(prac_markers, hosp_postcodes_markers, hosp_lookup, Clacks_Royal)
