# Services data manipulation (HSCP level)
# This script prepares marker objects for a specific HSCP once per partnership loop.

# Determine HSCP if not set
if (!exists("HSCP") && exists("LOCALITY")) {
  lookup2 <- read_in_localities()
  HSCP <- as.character(filter(lookup2, hscp_locality == LOCALITY)$hscp2019name)
  n_loc <- count_localities(lookup2, HSCP)
}

# Practice data manipulation
markers_gp <- prac %>%
  select(practice_code, gp_practice_name, practice_list_size, postcode) %>%
  mutate(postcode = gsub(" ", "", postcode, fixed = TRUE)) %>%
  left_join(postcode_lkp, by = "postcode") %>%
  mutate(type = "GP Practice") %>%
  filter(hscp2019name == HSCP)

# Hospital data manipulation
hosp_lookup <- hosp_types %>%
  filter(status == "Open") %>%
  select(
    name = treatment_location_name,
    location = treatment_location_code,
    type = current_department_type
  ) %>%
  left_join(
    select(hosp_postcodes, location = hospital_code, postcode),
    by = join_by(location)
  ) %>%
  mutate(postcode = gsub(" ", "", postcode, fixed = TRUE)) %>%
  left_join(postcode_lkp, by = "postcode")

# MIUs and EDs
markers_miu <- hosp_lookup %>%
  filter(type == "Minor Injury Unit or Other") %>%
  filter(hscp2019name == HSCP)

markers_emergency_dep <- hosp_lookup %>%
  filter(type == "Emergency Department") %>%
  filter(hscp2019name == HSCP)

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
