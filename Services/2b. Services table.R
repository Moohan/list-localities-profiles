# LOCALITY PROFILES SERVICES TABLE CODE
# Creates a table of what services are in the locality.

# 1. Prepare data for locality ----

# Subset care which is not Elderly care for table
other_care_type <- other_care_type_hscp |>
  dplyr::filter(hscp_locality == LOCALITY)

# 2. Create table ----

services_tibble <- dplyr::tibble(
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

# Cleanup ----
rm(other_care_type)
