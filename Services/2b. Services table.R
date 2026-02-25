############################################################################################# .
#                                                                                           #
#                         LOCALITY PROFILES SERVICES TABLE CODE                             #
#                                                                                           #
############################################################################################# .

## Code used to produce a table of what services are in the locality.
## Requires Services/2a. Services data manipulation.R to be run first at the HSCP level.

# 1. Subset other care types for table ----
# Subset care which is not Elderly care for the specific locality
other_care_locality <- other_care_hscp %>%
  filter(hscp_locality == LOCALITY)

# 2. Create services tibble ----
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
    nrow(other_care_locality)
  )
)

# 3. Housekeeping ----
# Note: we don't clean up markers_* here as they are needed for other localities
# or the map if run in a different order.
# In Build Profiles.R, they are cleaned up at the end of the HSCP loop.
rm(other_care_locality)
gc()
