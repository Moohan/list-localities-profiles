# LOCALITY PROFILES SERVICES DATA MANIPULATION (HSCP LEVEL)
#
# This script reads in and manipulates services data at the HSCP level.
# It prepares the 'markers' data frames containing information about GPs,
# care homes, emergency departments, and MIUs for an entire HSCP.
#
# ⚡ BOLT OPTIMIZATION: This logic was moved from the inner locality loop
# to the outer HSCP loop in 'Build Profiles.R'. Previously, this expensive
# data preparation was redundantly executed for every locality. Now, it runs
# only once per HSCP, significantly improving performance.
#
# Original script: 'Services/2. Services data manipulation & table.R'
# Corresponding locality script: 'Services/2. Services table.R'
#
# Written by C.Puech, refactored by Bolt ⚡

###### 1. Set up ######

# Change year to be the year in the data folder name
ext_year <- 2024

### Geographical lookups and objects ----

# Locality lookup (without datazones) for HSCP name consistency
lookup2 <- read_in_localities()

# Get number of localities in HSCP
n_loc <- count_localities(lookup2, HSCP)

###### 2. Read in services data ######

## Read in Postcode file for latitudes and longitudes
postcode_lkp <- read_in_postcodes() |>
  mutate(postcode = gsub(" ", "", pc7, fixed = TRUE)) |>
  select(
    postcode,
    grid_reference_easting,
    grid_reference_northing,
    latitude,
    longitude,
    datazone2011,
    hscp_locality,
    hscp2019name,
    hscp2019,
    hb2019name,
    hb2019
  )

## Read in all data in services folder
services_file_names <- fs::dir_ls(
  path(lp_path, glue("Services/DATA {ext_year}")),
  regexp = "\\.RDS$"
)

for (file in services_file_names) {
  name <- substr(x = fs::path_file(file), 1, 4)
  data <- readRDS(file) |>
    clean_names()
  assign(name, data)
}

# Change to more straightforward names
hosp_postcodes <- curr
hosp_types <- hosp
care_homes <- MDSF

rm(curr, hosp, MDSF)

###### 3. Manipulate services data ######

## GP Practices ----
prac <- prac |>
  select(practice_code, gp_practice_name, practice_list_size, postcode) |>
  mutate(postcode = gsub(" ", "", postcode, fixed = TRUE))

# Merge practice data with postcode and locality lookups
markers_gp <- left_join(prac, postcode_lkp, by = "postcode") |>
  mutate(type = "GP Practice") |>
  # filter out HSCP for map
  filter(hscp2019name == HSCP)

## Emergency Departments and MIUs ----
# rename for hospital_code to location
hosp_postcodes <- rename(hosp_postcodes, location = hospital_code)

# create hospital lookup table
hosp_lookup <- hosp_types |>
  filter(status == "Open") |>
  select(
    name = treatment_location_name,
    location = treatment_location_code,
    type = current_department_type
  ) |>
  left_join(
    select(hosp_postcodes, location, postcode),
    by = join_by(location)
  ) |>
  mutate(postcode = gsub(" ", "", postcode, fixed = TRUE)) |>
  left_join(postcode_lkp, by = "postcode")

# MIUs
markers_miu <- hosp_lookup |>
  filter(type == "Minor Injury Unit or Other") |>
  filter(hscp2019name == HSCP)

# EDs
markers_emergency_dep <- hosp_lookup |>
  filter(type == "Emergency Department") |>
  filter(hscp2019name == HSCP)

Clacks_Royal <- filter(hosp_lookup, name == "Forth Valley Royal Hospital")

# Ninewells hospital is incorrectly mapped even though postcode ok - so corrected coords here
if (HSCP == "Dundee City") {
  markers_emergency_dep <- markers_emergency_dep |>
    mutate(
      latitude = if_else(latitude == 56.4617, 56.4659308, latitude),
      longitude = if_else(longitude == -2.991432, -3.0378506, longitude)
    )
}

if (HSCP == "Clackmannanshire & Stirling") {
  markers_emergency_dep <- rbind(markers_emergency_dep, Clacks_Royal)
}

## Care Homes ----
markers_care_home <- care_homes |>
  select(
    type = care_service,
    subtype,
    name = service_name,
    service_postcode
  ) |>
  filter(type == "Care Home Service") |>
  filter(subtype == "Older People") |>
  mutate(postcode = gsub(" ", "", service_postcode, fixed = TRUE)) |>
  left_join(postcode_lkp, by = "postcode") |>
  filter(hscp2019name == HSCP)

# Housekeeping ----
# These objects are cleaned up in the parent script's loop
# to ensure they are available for the map script but removed before the next iteration.
rm(
  Clacks_Royal,
  data,
  file,
  hosp_lookup,
  hosp_postcodes,
  hosp_types,
  name,
  prac,
  services_file_names
)
gc()
