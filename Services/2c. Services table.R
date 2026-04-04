# Services locality table (Locality level)
# Runs once per locality in the inner loop

# Subset care which is not Elderly care for table
other_care_type <- care_homes |>
  dplyr::select(
    type = care_service,
    subtype,
    name = service_name,
    service_postcode
  ) |>
  dplyr::filter(
    type == "Care Home Service",
    subtype != "Older People"
  ) |>
  dplyr::mutate(postcode = gsub(" ", "", service_postcode, fixed = TRUE)) |>
  dplyr::left_join(
    postcode_lkp,
    by = "postcode",
    relationship = "many-to-one"
  ) |>
  dplyr::filter(hscp_locality == LOCALITY)

# Create table
services_tibble <- tibble::tibble(
  Type = c("Primary Care", "A&E", "", "Care Home", ""),
  Service = c(
    "GP Practice",
    "Emergency Department",
    "Minor Injuries Unit",
    "Elderly Care",
    "Other"
  ),
  Number = c(
    sum(markers_gp[["hscp_locality"]] == LOCALITY, na.rm = TRUE),
    sum(markers_emergency_dep[["hscp_locality"]] == LOCALITY, na.rm = TRUE),
    sum(markers_miu[["hscp_locality"]] == LOCALITY, na.rm = TRUE),
    sum(markers_care_home[["hscp_locality"]] == LOCALITY, na.rm = TRUE),
    nrow(other_care_type)
  )
)

# Housekeeping
rm(other_care_type)
gc()
