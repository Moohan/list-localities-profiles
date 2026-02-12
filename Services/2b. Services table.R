############################################################################################# .
#                                                                                           #
#                         LOCALITY PROFILES SERVICES TABLE                                  #
#                                                                                           #
############################################################################################# .

## Code used to produce a table of what services are in the locality.
# This script is intended to be run at the locality level.
# It depends on objects created by "2a. Services data manipulation.R"

###### 4. Table ######

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
    sum(markers_gp[["hscp_locality"]] == LOCALITY, na.rm = TRUE),
    sum(markers_emergency_dep[["hscp_locality"]] == LOCALITY, na.rm = TRUE),
    sum(markers_miu[["hscp_locality"]] == LOCALITY, na.rm = TRUE),
    sum(markers_care_home[["hscp_locality"]] == LOCALITY, na.rm = TRUE),
    sum(markers_other_care[["hscp_locality"]] == LOCALITY, na.rm = TRUE)
  )
)
