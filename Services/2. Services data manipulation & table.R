############################################################################################# .
#                                                                                           #
#                   LOCALITY PROFILES SERVICES MAP & TABLE CODE (Locality)                  #
#                                                                                           #
############################################################################################# .

## Code used to manipulate services data for locality profiles.
# Also produces a table of what services are in the locality.
# The map is created in script "3. Services HSCP Map" - this is so that it does not have to run
# for every locality

## Written by C.Puech
## Created on 24/02/2020
## Latest update August 2022 - rewrote parts of code for smoother process
## Refactored by Bolt - 2024-05-20 - Moved HSCP-level logic to outer loop.

###### 1. Set up ######

# This script now assumes that the HSCP-level data has already been loaded
# by "1. Services data manipulation HSCP.R"

# Determine HSCP name for cases where it is not consistent
HSCP <- as.character(filter(lookup2, hscp_locality == LOCALITY)$hscp2019name)

###### 2. Table ######

# Subset care which is not Elderly care for table
other_care_type <- care_homes %>%
  select(
    type = care_service,
    subtype,
    name = service_name,
    service_postcode
  ) %>%
  filter(type == "Care Home Service") %>%
  filter(subtype != "Older People") %>%
  mutate(postcode = gsub(" ", "", service_postcode, fixed = TRUE)) %>%
  left_join(postcode_lkp, by = "postcode") %>%
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

# Housekeeping ----
# These objects are left over after the script is run
# but don't appear to be used in any 'downstream' process:
# Main markdown, Summary Table, Excel data tables, SDC output.
# TODO: Investigate if these can be removed earlier or not created at all.
rm(
  other_care_type
)
gc()
