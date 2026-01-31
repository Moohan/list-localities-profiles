############################################################################################# .
#                                                                                           #
#                         LOCALITY PROFILES SERVICES TABLE CODE                             #
#                                                                                           #
############################################################################################# .

## This script contains locality-level logic and should be sourced in the inner loop.
## It depends on markers_* and other_care_type_hscp being available in the environment.

## Refactored by Bolt Oct 2024 to optimize performance in loops

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
    sum(other_care_type_hscp[["hscp_locality"]] == LOCALITY, na.rm = TRUE)
  )
)
