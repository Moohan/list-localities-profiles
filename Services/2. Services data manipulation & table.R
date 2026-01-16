# LOCALITY PROFILES SERVICES - LOCALITY TABLE
# This script produces a table of what services are in a given locality.
# It depends on data prepared by '1. hscp_services_data_preparation.R'.

# This script expects 'LOCALITY' to be defined in the calling environment.
# It also expects 'care_homes', 'markers_gp', 'markers_emergency_dep',
# 'markers_miu', and 'markers_care_home' to be available from the
# HSCP-level script.

# Set up ----
# Load required packages
library(dplyr)
library(tibble)

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
# but don't appear to be used in any 'downstream' process:
# Main markdown, Summary Table, Excel data tables, SDC output.
# TODO: Investigate if these can be removed earlier or not created at all.
rm(
  care_homes,
  Clacks_Royal,
  data,
  file,
  hosp_lookup,
  hosp_postcodes,
  hosp_types,
  name,
  other_care_type,
  postcode_lkp,
  prac,
  services_file_names
)
gc()
