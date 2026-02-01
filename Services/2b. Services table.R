# Locality-level Services Table
# This script produces a table of what services are in the locality.
# It is sourced once per LOCALITY in Build Profiles.R.
# It depends on markers prepared in "2a. Services data manipulation.R".

# Subset care which is not Elderly care for table
# Uses the pre-calculated HSCP-level data from 2a
other_care_type <- all_other_care_type %>%
  filter(hscp_locality == LOCALITY)

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

# Housekeeping ----
rm(other_care_type)
