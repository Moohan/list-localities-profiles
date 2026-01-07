############################################################################################# .
#                                                                                           #
#                  LOCALITY PROFILES SERVICES DATA MANIPULATION (LOCALITY)                  #
#                                                                                           #
############################################################################################# .

# This script uses the pre-processed HSCP-level data to create a
# summary table of services for the specific locality.

###### 1. Set up ######

# Note that the HSCP for the locality is determined in the calling script
# "Locality Profiles Render Code.R", and the HSCP-level data is loaded in
# "2a. Services data manipulation - HSCP.R".

# The following objects are assumed to be in the environment:
# - LOCALITY: the current locality
# - HSCP: the current HSCP
# - care_homes: data frame with care home information
# - markers_gp: data frame with GP practice information
# - markers_emergency_dep: data frame with emergency department information
# - markers_miu: data frame with minor injury unit information


###### 2. Table ######

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
  Type = c("**Primary Care**", "**A&E**", "", "**Care Home**", ""),
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
rm(
  other_care_type
)
gc()
