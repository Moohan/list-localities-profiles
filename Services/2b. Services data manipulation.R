# Services data manipulation
# Run once per HSCP in the loop (e.g., in Build Profiles.R)

# Ensure HSCP is defined, or get it from LOCALITY if only one locality is being run
if (!exists("HSCP")) {
  HSCP <- as.character(
    dplyr::filter(read_in_localities(), hscp_locality == LOCALITY)$hscp2019name
  )
}

# Locality lookup for mapping (DZ level)
lookup_dz <- read_in_localities(dz_level = TRUE)

# Lookup without datazones
lookup2 <- read_in_localities()

# Get number of localities in HSCP
n_loc <- count_localities(lookup2, HSCP)

###### 3. Manipulate services data ######

## GP Practices ----
prac_hscp <- prac |>
  select(practice_code, gp_practice_name, practice_list_size, postcode) |>
  mutate(postcode = toupper(gsub(" ", "", postcode, fixed = TRUE)))

# Merge practice data with postcode and locality lookups
markers_gp <- left_join(prac_hscp, postcode_lkp, by = "postcode") |>
  mutate(type = "GP Practice") |>
  # filter out HSCP for map
  filter(hscp2019name == HSCP)

## Emergency Departments and MIUs ----

# create hospital lookup table
hosp_lookup <- hosp_types |>
  filter(status == "Open") |>
  select(
    name = treatment_location_name,
    location = treatment_location_code,
    type = current_department_type
  ) |>
  left_join(
    select(hosp_postcodes, location = hospital_code, postcode),
    by = join_by(location)
  ) |>
  mutate(postcode = toupper(gsub(" ", "", postcode, fixed = TRUE))) |>
  left_join(postcode_lkp, by = "postcode")

# MIUs
markers_miu <- hosp_lookup |>
  filter(type == "Minor Injury Unit or Other") |>
  filter(hscp2019name == HSCP)

# EDs
markers_emergency_dep <- hosp_lookup |>
  filter(type == "Emergency Department") |>
  filter(hscp2019name == HSCP)

clacks_royal_marker <- filter(
  hosp_lookup,
  name == "Forth Valley Royal Hospital"
)

# Ninewells hospital is incorrectly mapped even though postcode ok - so corrected coords here
if (HSCP == "Dundee City") {
  markers_emergency_dep <- markers_emergency_dep |>
    mutate(
      latitude = if_else(latitude == 56.4617, 56.4659308, latitude),
      longitude = if_else(longitude == -2.991432, -3.0378506, longitude)
    )
}

if (HSCP == "Clackmannanshire & Stirling") {
  markers_emergency_dep <- rbind(markers_emergency_dep, clacks_royal_marker)
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
  mutate(postcode = toupper(gsub(" ", "", service_postcode, fixed = TRUE))) |>
  left_join(postcode_lkp, by = "postcode") |>
  filter(hscp2019name == HSCP)

# Cleaning up intermediate objects not needed downstream
rm(prac_hscp, hosp_lookup)
