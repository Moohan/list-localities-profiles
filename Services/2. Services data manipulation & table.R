############################################################################################# .
#                                                                                           #
#                         LOCALITY PROFILES SERVICES MAP & TABLE CODE                       #
#                                                                                           #
############################################################################################# .

## Code used to manipulate services data for locality profiles.
# Also produces a table of what services are in the locality.
# The map is created in script "3. Services HSCP Map" - this is so that it does not have to run
# for every locality

## Written by C.Puech
## Created on 24/02/2020
## Latest update August 2022 - rewrote parts of code for smoother process

###### 1. Set up ######

## Set Locality (for testing only)
# LOCALITY <- "Falkirk West"

###### 4. Table ######

# Subset care which is not Elderly care for table
other_care_type <- markers_care_home %>%
  filter(subtype != "Older People") %>%
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
    sum(
      markers_care_home[["hscp_locality"]] == LOCALITY &
        markers_care_home[["subtype"]] == "Older People"
    ),
    nrow(other_care_type)
  )
)

# Housekeeping ----
rm(
  other_care_type
)
gc()
