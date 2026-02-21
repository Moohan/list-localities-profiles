############################################################################################# .
#                                                                                           #
#                         LOCALITY PROFILES SERVICES TABLE CODE                             #
#                                                                                           #
############################################################################################# .

## Code used to produce a table of what services are in the locality.
# This script depends on objects created in "2a. Services data manipulation"

###### 4. Table ######

# Subset care which is not Elderly care for table (Locality level)
other_care_type <- markers_other_care |>
  dplyr::filter(hscp_locality == LOCALITY)

# Create table
services_tibble <- tibble::tibble(
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
    nrow(other_care_type)
  )
)
