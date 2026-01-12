############################################################################################# .
#                                                                                           #
#                   LOCALITY PROFILES SERVICES TABLE CODE (LOCALITY)                        #
#                                                                                           #
############################################################################################# .

# This script produces a table of what services are in a specific locality.
# It depends on the `markers_*` and `care_homes` data frames created by the HSCP-level
# script: `2a. Services data manipulation (HSCP).R`.
# It is designed to be run inside the inner (locality) loop.

###### 1. Table Creation ######

# The `LOCALITY`, `markers_*`, `care_homes`, and `postcode_lkp` variables are expected
# to be available from the calling environment.

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

###### 2. Housekeeping ######
rm(
  other_care_type
)
gc()
