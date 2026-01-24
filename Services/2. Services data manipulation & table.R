############################################################################################# .
#                                                                                           #
#                         LOCALITY PROFILES SERVICES MAP & TABLE CODE                       #
#                                                                                           #
############################################################################################# .

## Code used to produce a table of what services are in the locality.
## The data manipulation is done in script "1. Services data preparation".
## The map is created in script "3. Services HSCP Map".

###### 1. Set up ######

# Note that the following objects are created in the "1. ..." script:
# markers_gp, markers_emergency_dep, markers_miu, markers_care_home, care_homes, postcode_lkp

lookup2 <- read_in_localities()
hscp <- as.character(filter(lookup2, hscp_locality == LOCALITY)$hscp2019name)


###### 2. Table ######

# Subset care which is not Elderly care for table
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
rm(
  other_care_type,
  lookup2
)
gc()
