############################################################################################# .
#                                                                                           #
#                         LOCALITY PROFILES SERVICES TABLE                                  #
#                                                                                           #
############################################################################################# .

## Code used to generate the services table for a specific locality.
# This script assumes that "2a. Services data manipulation" has already been run
# and that the "markers_*" objects are available in the environment.

# 1. Set up ----

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

# 2. Create table ----

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

# 3. Housekeeping ----
# Objects that are locality-specific and can be removed after the table is created
# (They will also be caught by the loop_env cleanup in Build Profiles.R)
rm(other_care_type)
