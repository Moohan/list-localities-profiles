# Services Data Manipulation (HSCP Level)

# Subset data for the specific HSCP to be used by all localities in the HSCP.
# This avoids redundant filtering for every locality.

# Ensure HSCP is defined if this is run outside the loop
if (!exists("HSCP") && exists("LOCALITY")) {
  HSCP <- as.character(filter(read_in_localities(), hscp_locality == LOCALITY)[[
    "hscp2019name"
  ]])
}

# Lookup without datazones
lookup2 <- read_in_localities()

# Get number of localities in HSCP
n_loc <- count_localities(lookup2, HSCP)

# GP Practices
markers_gp <- filter(prac, hscp2019name == HSCP)

# MIUs
markers_miu <- hosp_lookup |>
  filter(type == "Minor Injury Unit or Other") |>
  filter(hscp2019name == HSCP)

# EDs
markers_emergency_dep <- hosp_lookup |>
  filter(type == "Emergency Department") |>
  filter(hscp2019name == HSCP)

# Forth Valley Royal Hospital Special Case
clacks_royal_marker <- filter(
  hosp_lookup,
  name == "Forth Valley Royal Hospital"
)

# Ninewells hospital correction
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

# Care Homes
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
