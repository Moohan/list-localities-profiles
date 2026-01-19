# LOCALITY PROFILES SERVICES DATA FOR HSCP
# This script reads and manipulates all the necessary services data for a given HSCP
# (Health and Social Care Partnership).
# It is designed to be sourced in the outer loop of the `Build Profiles.R` script
# to avoid reprocessing the same data for every locality.

# 1. Set up ----
# The ext_year variable should be set before this script is sourced.
# The HSCP variable is inherited from the outer loop in `Build Profiles.R`.

# Geographical lookups and objects
# These lookups are used for joining and filtering data.
lookup <- read_in_localities(dz_level = TRUE)
lookup2 <- read_in_localities()

# Get number of localities in the current HSCP
n_loc <- count_localities(lookup2, HSCP)

# 2. Read in services data ----

# Read in Postcode file for latitudes and longitudes
# This provides geographical information for the service locations.
postcode_lkp <- read_in_postcodes() %>%
  mutate(postcode = gsub(" ", "", pc7, fixed = TRUE)) %>%
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

# Read in all data in services folder for the specified year
services_file_names <- list.files(
  paste0(lp_path, "Services/DATA ", ext_year),
  pattern = "RDS"
)

for (file in services_file_names) {
  name <- substr(x = file, 1, 4)
  data <- readRDS(paste0(lp_path, "Services/DATA ", ext_year, "/", file)) %>%
    clean_names()
  assign(name, data)
}

# Assign more descriptive names to the data frames
hosp_postcodes <- curr
hosp_types <- hosp
care_homes <- MDSF

rm(curr, hosp, MDSF)


# 3. Manipulate services data ----

# GP Practices
# Process GP practice data, linking it with geographical information.
prac <- prac %>%
  select(practice_code, gp_practice_name, practice_list_size, postcode) %>%
  mutate(postcode = gsub(" ", "", postcode, fixed = TRUE))

# Merge practice data with postcode and locality lookups and filter for the current HSCP
markers_gp <- left_join(prac, postcode_lkp, by = "postcode") %>%
  mutate(type = "GP Practice") %>%
  filter(hscp2019name == HSCP)

# Emergency Departments and MIUs
# Process hospital data to identify Emergency Departments and Minor Injury Units.
hosp_postcodes <- rename(hosp_postcodes, location = hospital_code)

hosp_lookup <- hosp_types %>%
  filter(status == "Open") %>%
  select(
    name = treatment_location_name,
    location = treatment_location_code,
    type = current_department_type
  ) %>%
  left_join(
    select(hosp_postcodes, location, postcode),
    by = join_by(location)
  ) %>%
  mutate(postcode = gsub(" ", "", postcode, fixed = TRUE)) %>%
  left_join(postcode_lkp, by = "postcode")

# Filter for Minor Injury Units within the current HSCP
markers_miu <- hosp_lookup %>%
  filter(type == "Minor Injury Unit or Other") %>%
  filter(hscp2019name == HSCP)

# Filter for Emergency Departments within the current HSCP
markers_emergency_dep <- hosp_lookup %>%
  filter(type == "Emergency Department") %>%
  filter(hscp2019name == HSCP)

# Specific data corrections for certain HSCPs
Clacks_Royal <- filter(hosp_lookup, name == "Forth Valley Royal Hospital")

if (HSCP == "Dundee City") {
  markers_emergency_dep <- markers_emergency_dep %>%
    mutate(
      latitude = if_else(latitude == 56.4617, 56.4659308, latitude),
      longitude = if_else(longitude == -2.991432, -3.0378506, longitude)
    )
}

if (HSCP == "Clackmannanshire & Stirling") {
  markers_emergency_dep <- rbind(markers_emergency_dep, Clacks_Royal)
}

# Care Homes
# Process care home data, filtering for elderly care homes within the current HSCP.
markers_care_home <- care_homes %>%
  select(
    type = care_service,
    subtype,
    name = service_name,
    service_postcode
  ) %>%
  filter(type == "Care Home Service") %>%
  filter(subtype == "Older People") %>%
  mutate(postcode = gsub(" ", "", service_postcode, fixed = TRUE)) %>%
  left_join(postcode_lkp, by = "postcode") %>%
  filter(hscp2019name == HSCP)

# 4. Housekeeping ----
# Clean up intermediate objects that are not needed for downstream processes.
rm(
  data,
  file,
  hosp_lookup,
  name,
  prac,
  services_file_names
)
gc()
