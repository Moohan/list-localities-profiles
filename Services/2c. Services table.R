############################################################################################# .
#                                                                                           #
#                         LOCALITY PROFILES SERVICES LOCALITY TABLE                         #
#                                                                                           #
############################################################################################# .

## Code used to generate locality-level services table.
# This script should be sourced once per locality in the loop.

## Written by C.Puech
## Created on 24/02/2020
## Latest update August 2022 - rewrote parts of code for smoother process
## Tiered Refactoring Nov 2024 - Bolt ⚡ (Performance Optimization)

###### 1. Set up ######

# Note: LOCALITY, markers_gp, markers_miu, markers_emergency_dep, and markers_care_home
# must be defined before sourcing this script.

###### 2. Table generation ######

# Subset care which is not Elderly care for table
other_care_type <- care_homes_global %>%
  select(
    type = care_service,
    subtype,
    name = service_name,
    service_postcode
  ) %>%
  filter(type == "Care Home Service") %>%
  filter(subtype != "Older People") %>%
  mutate(postcode = gsub(" ", "", service_postcode, fixed = TRUE)) %>%
  left_join(postcode_lkp_global, by = "postcode") %>%
  filter(hscp_locality == LOCALITY)

# Create table
services_tibble <- tibble(
  Type = c("Primary Care", "A&E", "", "Care Home", ""),
  Service = c(
    "GP Practice",
    "Emergency Department",
    "Minor Injuries Unit",
    "Elderly Care",
    "Other"
  ),
  Number = c(
    sum(markers_gp[["hscp_locality"]] == LOCALITY),
    sum(markers_emergency_dep[["hscp_locality"]] == LOCALITY),
    sum(markers_miu[["hscp_locality"]] == LOCALITY),
    sum(markers_care_home[["hscp_locality"]] == LOCALITY),
    nrow(other_care_type)
  )
)

# Clean up temporary locality objects
rm(other_care_type)
