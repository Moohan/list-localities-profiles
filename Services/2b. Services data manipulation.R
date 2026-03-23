############################################################################################# .
#                                                                                           #
#                         LOCALITY PROFILES SERVICES HSCP MANIPULATION                      #
#                                                                                           #
############################################################################################# .

## Code used to manipulate services data for a specific HSCP in locality profiles.
# This script should be sourced once per HSCP in the loop.

## Written by C.Puech
## Created on 24/02/2020
## Latest update August 2022 - rewrote parts of code for smoother process
## Tiered Refactoring Nov 2024 - Bolt ⚡ (Performance Optimization)

###### 1. Set up ######

# Note: HSCP and lookup2 (HSCP/Locality lookup) must be defined before sourcing this script.

# Get number of localities in HSCP
n_loc <- count_localities(lookup2, HSCP)

###### 2. Manipulate services data ######

## GP Practices ----

prac_hscp <- prac %>%
  select(practice_code, gp_practice_name, practice_list_size, postcode) %>%
  mutate(postcode = gsub(" ", "", postcode, fixed = TRUE))

# Merge practice data with postcode and locality lookups
markers_gp <- left_join(prac_hscp, postcode_lkp_global, by = "postcode") %>%
  mutate(type = "GP Practice") %>%
  # filter out HSCP for map
  filter(hscp2019name == HSCP)

## Emergency Departments and MIUs ----

# rename for hospital_code to location
hosp_postcodes_hscp <- rename(hosp_postcodes_global, location = hospital_code)

# create hospital lookup table
hosp_lookup <- hosp_types_global %>%
  filter(status == "Open") %>%
  select(
    name = treatment_location_name,
    location = treatment_location_code,
    type = current_department_type
  ) %>%
  left_join(
    select(hosp_postcodes_hscp, location, postcode),
    by = join_by(location)
  ) %>%
  mutate(postcode = gsub(" ", "", postcode, fixed = TRUE)) %>%
  left_join(postcode_lkp_global, by = "postcode")

# MIUs
markers_miu <- hosp_lookup %>%
  filter(type == "Minor Injury Unit or Other") %>%
  filter(hscp2019name == HSCP)

# EDs
markers_emergency_dep <- hosp_lookup %>%
  filter(type == "Emergency Department") %>%
  filter(hscp2019name == HSCP)

Clacks_Royal <- filter(hosp_lookup, name == "Forth Valley Royal Hospital")

# Ninewells hospital is incorrectly mapped even though postcode ok - so corrected coords here

if (HSCP == "Dundee City") {
  markers_emergency_dep <- markers_emergency_dep %>%
    mutate(
      latitude = if_else(latitude == 56.4617, 56.4659308, latitude),
      longitude = if_else(longitude == -2.991432, -3.0378506, longitude)
    )
}

if (HSCP == "Clackmannanshire & Stirling") {
  markers_emergency_dep <- rbind(markers_emergency_dep, Clacks_Royal)
}

## Care Homes ----

markers_care_home <- care_homes_global %>%
  select(
    type = care_service,
    subtype,
    name = service_name,
    service_postcode
  ) %>%
  filter(type == "Care Home Service") %>%
  filter(subtype == "Older People") %>%
  mutate(postcode = gsub(" ", "", service_postcode, fixed = TRUE)) %>%
  left_join(postcode_lkp_global, by = "postcode") %>%
  filter(hscp2019name == HSCP)

# Clean up temporary HSCP objects
rm(
  prac_hscp,
  hosp_postcodes_hscp,
  hosp_lookup,
  Clacks_Royal
)
