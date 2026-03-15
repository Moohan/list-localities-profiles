# Services data loading script (Global session level)

# 1. Configuration ----
ext_year <- 2024

# 2. Read in services data ----

## Read in Postcode file for latitudes and longitudes
# This is a large file, so we load it once and keep it in memory
postcode_lkp <- read_in_postcodes() |>
  dplyr::mutate(postcode = stringr::str_replace_all(pc7, " ", "")) |>
  dplyr::select(
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
services_file_names <- list.files(
  fs::path(lp_path, "Services", glue::glue("DATA {ext_year}")),
  pattern = "RDS"
)

for (file in services_file_names) {
  name <- substr(x = file, 1, 4)

  data <- readRDS(
    fs::path(lp_path, "Services", glue::glue("DATA {ext_year}"), file)
  ) |>
    janitor::clean_names()

  assign(name, data)
}

# Change to more straightforward names
hosp_postcodes <- curr |> dplyr::rename(location = hospital_code)
hosp_types <- hosp
care_homes <- MDSF

rm(curr, hosp, MDSF, data, file, name, services_file_names)
