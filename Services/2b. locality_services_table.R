############################################################################################# .
#                                                                                           #
#                         LOCALITY PROFILES SERVICES MAP & TABLE CODE                       #
#                                                                                           #
############################################################################################# .

## Code used to produce a table of what services are in the locality.
# The data manipulation is done in script "2a. hscp_services_data_manipulation.R".
# The map is created in script "3. Services HSCP Map".

## Written by C.Puech
## Created on 24/02/2020
## Latest update August 2022 - rewrote parts of code for smoother process

###### 1. Set up ######

## Set Locality (for testing only)
# LOCALITY <- "Falkirk West"

## Determine HSCP (for testing only)
# HSCP <- as.character(filter(lookup2, hscp_locality == LOCALITY)$hscp2019name)

# This script assumes that 'LOCALITY', 'care_homes', 'markers_gp',
# 'markers_emergency_dep', 'markers_miu', 'markers_care_home', and 'postcode_lkp'
# are objects available in the environment.

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
# but don't appear to be used in any 'downstream' process.
rm(
  other_care_type
)
gc()
