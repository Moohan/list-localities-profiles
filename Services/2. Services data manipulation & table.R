############################################################################################# .
#                                                                                           #
#                         LOCALITY PROFILES SERVICES MAP & TABLE CODE                       #
#                                                                                           #
############################################################################################# .

## Code used to manipulate services data for locality profiles.
# This script produces a table of what services are in the locality.
# It is sourced in the inner (locality) loop of `Locality Profiles Render Code.R`.
# It relies on objects created in `1. Services data loading.R`.

###### 1. Set up ######

## Determine HSCP
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
  Type = c("**Primary Care**", "**A&E**", "", "**Care Home**", ""),
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
