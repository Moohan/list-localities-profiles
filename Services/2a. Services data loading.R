# Services Data Loading (Global Session Level)

# Change year to be the year in the data folder name
ext_year <- 2024

## Read in Postcode file for latitudes and longitudes
# This is a large file and transformation is expensive, so we do it once.
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
  path(lp_path, "Services", paste0("DATA ", ext_year)),
  regexp = "RDS$"
)

for (file in services_file_names) {
  name <- substr(x = basename(file), 1, 4)

  data <- readRDS(file) |>
    clean_names()

  assign(name, data)
}

# Change to more straightforward names
hosp_postcodes <- curr
hosp_types <- hosp
care_homes <- MDSF

rm(curr, hosp, MDSF, data, file, services_file_names)

# create hospital lookup table
hosp_lookup <- hosp_types |>
  filter(status == "Open") |>
  select(
    name = treatment_location_name,
    location = treatment_location_code,
    type = current_department_type
  ) |>
  left_join(
    select(hosp_postcodes, hospital_code, postcode),
    by = join_by(location == hospital_code)
  ) |>
  mutate(postcode = gsub(" ", "", postcode, fixed = TRUE)) |>
  left_join(postcode_lkp, by = "postcode")

# Prepare GP practices base data
prac <- prac |>
  select(practice_code, gp_practice_name, practice_list_size, postcode) |>
  mutate(postcode = gsub(" ", "", postcode, fixed = TRUE)) |>
  left_join(postcode_lkp, by = "postcode") |>
  mutate(type = "GP Practice")
