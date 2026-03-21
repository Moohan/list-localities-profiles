# Services data loading (Global level)

# Change year to be the year in the data folder name
ext_year <- 2024

## Read in Postcode file for latitudes and longitudes
postcode_lkp <- read_in_postcodes() |>
  dplyr::mutate(postcode = gsub(" ", "", pc7, fixed = TRUE)) |>
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
  fs::path(lp_path, "Services", paste0("DATA ", ext_year)),
  pattern = "(?i)\\.rds$"
)

for (file in services_file_names) {
  name <- substr(x = file, 1, 4)

  data <- readRDS(fs::path(
    lp_path,
    "Services",
    paste0("DATA ", ext_year),
    file
  )) |>
    clean_names()

  assign(name, data)
}

# Change to more straightforward names
hosp_postcodes <- curr
hosp_types <- hosp
care_homes <- MDSF

rm(curr, hosp, MDSF, data, file, services_file_names)
