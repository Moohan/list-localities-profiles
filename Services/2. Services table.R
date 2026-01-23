# LOCALITY PROFILES SERVICES TABLE CODE
#
# This script produces a table ('services_tibble') summarizing the count
# of various services within a specific LOCALITY.
#
# ⚡ BOLT OPTIMIZATION: This script is the second part of a refactoring.
# The expensive, HSCP-level data preparation (e.g., loading and processing
# 'markers' data) has been moved to a new script:
# 'Services/1. Services data manipulation HSCP.R'.
#
# This script now only performs the quick, locality-specific filtering and
# table creation, making it much more efficient to run inside the inner
# locality loop of 'Build Profiles.R'.
#
# Original script: 'Services/2. Services data manipulation & table.R'
# Corresponding HSCP script: 'Services/1. Services data manipulation HSCP.R'
#
# Written by C.Puech, refactored by Bolt ⚡

###### 1. Table Creation ######

# Subset care which is not Elderly care for table
# This depends on the 'care_homes' and 'postcode_lkp' objects
# which are created in the HSCP-level script. The 'care_homes' object is intentionally
# not removed in the HSCP script so it can be used here.
other_care_type <- care_homes |>
  select(
    type = care_service,
    subtype,
    name = service_name,
    service_postcode
  ) |>
  filter(type == "Care Home Service") |>
  filter(subtype != "Older People") |>
  mutate(postcode = gsub(" ", "", service_postcode, fixed = TRUE)) |>
  left_join(postcode_lkp, by = "postcode") |>
  filter(hscp_locality == LOCALITY)

# Create table using the pre-computed 'markers' data frames from the HSCP script.
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
    nrow(other_care_type)
  )
)

# Housekeeping ----
rm(
  other_care_type
)
gc()
