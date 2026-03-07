############################################################################################# .
#                                                                                           #
#                         LOCALITY PROFILES SERVICES DATA LOADING                           #
#                                                                                           #
############################################################################################# .

## Code used to load services data for locality profiles.
## Run once per session (Global level).

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
  pattern = "RDS"
)

for (file in services_file_names) {
  name <- str_sub(string = file, start = 1, end = 4)

  data <- readRDS(fs::path(
    lp_path,
    "Services",
    paste0("DATA ", ext_year),
    file
  )) |>
    janitor::clean_names()

  assign(name, data)
}

# Change to more straightforward names
hosp_postcodes <- curr
hosp_types <- hosp
care_homes <- MDSF

rm(curr, hosp, MDSF, file, name, data, services_file_names)
