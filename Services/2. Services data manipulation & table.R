# LOCALITY PROFILES SERVICES TABLE CODE
# This script creates a summary table of the services available in a specific locality.
# It is designed to be sourced in the inner loop of the `Build Profiles.R` script
# after the HSCP-level data has been loaded by `Services/1. Services HSCP data.R`.

# The LOCALITY variable is inherited from the inner loop in `Build Profiles.R`.

# 1. Table Creation ----

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

# 2. Housekeeping ----
# Clean up objects that are no longer needed.
rm(other_care_type)
gc()
