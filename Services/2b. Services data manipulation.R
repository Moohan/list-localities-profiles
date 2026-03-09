# HSCP-level services data manipulation

# Lookup without datazones (needed for HSCP determination if not already set)
if (!exists("lookup2")) {
  lookup2 <- read_in_localities()
}

# Determine HSCP if not provided (defensive logic for testing scripts)
if (!exists("HSCP")) {
  HSCP <- as.character(filter(lookup2, hscp_locality == LOCALITY)$hscp2019name)
}

# Get number of localities in HSCP
if (!exists("n_loc")) {
  n_loc <- count_localities(lookup2, HSCP)
}

###### 3. Manipulate services data ######

## GP Practices ----

prac_proc <- prac %>%
  select(practice_code, gp_practice_name, practice_list_size, postcode) %>%
  mutate(postcode = gsub(" ", "", postcode, fixed = TRUE))

# Merge practice data with postcode and locality lookups
markers_gp <- left_join(prac_proc, postcode_lkp, by = "postcode") %>%
  mutate(type = "GP Practice") %>%
  # filter out HSCP for map
  filter(hscp2019name == HSCP)

## Emergency Departments and MIUs ----

# rename for hospital_code to location
hosp_postcodes_proc <- rename(hosp_postcodes, location = hospital_code)

# create hospital lookup table
hosp_lookup_proc <- hosp_types %>%
  filter(status == "Open") %>%
  select(
    name = treatment_location_name,
    location = treatment_location_code,
    type = current_department_type
  ) %>%
  left_join(
    select(hosp_postcodes_proc, location, postcode),
    by = join_by(location)
  ) %>%
  mutate(postcode = gsub(" ", "", postcode, fixed = TRUE)) %>%
  left_join(postcode_lkp, by = "postcode")

# MIUs
markers_miu <- hosp_lookup_proc %>%
  filter(type == "Minor Injury Unit or Other") %>%
  filter(hscp2019name == HSCP)

# EDs
markers_emergency_dep <- hosp_lookup_proc %>%
  filter(type == "Emergency Department") %>%
  filter(hscp2019name == HSCP)

Clacks_Royal <- filter(hosp_lookup_proc, name == "Forth Valley Royal Hospital")

# Ninewells hospital is incorrectly mapped even though postcode ok - so corrected coords here

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

## Care Homes ----

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

rm(prac_proc, hosp_postcodes_proc, hosp_lookup_proc)
