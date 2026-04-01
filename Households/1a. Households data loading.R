################################################################################ .
#                                                                              #
#                   LOCALITY PROFILES: HOUSEHOLDS DATA LOADING                 #
#                                                                              #
################################################################################ .
# This script loads raw household data and calculates Scotland-level statistics.
# It is intended to be run once per session (hoisted out of loops).

# load in required packages
library(readxl)
library(reshape2)

# Update Data Year (this is the maximum year available for both housing data sets from NRS)
max_year_housing <- 2024
# Update Publication Year (the year marked on the Data folder)
ext_year <- 2025

# 1. Households Data Loading ----

# get historic housing data, each year is on a separate sheet so map over it
house_raw_dat <- map(
  2014L:max_year_housing,
  \(year) {
    read_excel(
      path(
        lp_path,
        "Households",
        glue("Data {ext_year}"),
        "household_estimates.xlsx"
      ),
      col_types = c(rep("text", 4L), rep("numeric", 8L), rep("skip", 7L)),
      sheet = as.character(year),
      skip = 3L,
      .name_repair = make_clean_names # janitor::make_clean_names()
    ) |>
      mutate(year = year, .before = everything())
  }
) |>
  list_rbind()

# 2. Council Tax Band Data Loading ----

# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/households/household-estimates/small-area-statistics-on-households-and-dwellings

house_raw_dat2 <- read_excel(
  paste0(lp_path, "Households/", "Data ", ext_year, "/council_tax.xlsx"),
  sheet = as.character(max_year_housing),
  skip = 4
) |>
  clean_names()

# 3. Global Lookups ----

# DataZone-level lookup for all localities
lookup_dz_all <- read_in_localities(dz_level = TRUE) |>
  select(datazone2011, hscp_locality, hscp2019name)

# 4. Scotland Level Statistics ----

scot_n_houses <- format_number_for_text(sum(
  filter(house_raw_dat, year == max(year))[["total_number_of_dwellings"]],
  na.rm = TRUE
))
scot_perc_discount <- format_number_for_text(
  sum(
    filter(
      house_raw_dat,
      year == max(year)
    )[["dwellings_with_a_single_adult_council_tax_discount"]],
    na.rm = TRUE
  ) /
    sum(
      filter(house_raw_dat, year == max(year))[["total_number_of_dwellings"]],
      na.rm = TRUE
    ) *
    100
)

scot_perc_housesAC <- format_number_for_text(
  sum(
    house_raw_dat2[["council_tax_band_a"]],
    house_raw_dat2[["council_tax_band_b"]],
    house_raw_dat2[["council_tax_band_c"]],
    na.rm = TRUE
  ) /
    sum(house_raw_dat2[["total_number_of_dwellings"]], na.rm = TRUE) *
    100
)
scot_perc_housesFH <- format_number_for_text(
  sum(
    house_raw_dat2[["council_tax_band_f"]],
    house_raw_dat2[["council_tax_band_g"]],
    house_raw_dat2[["council_tax_band_h"]],
    na.rm = TRUE
  ) /
    sum(house_raw_dat2[["total_number_of_dwellings"]], na.rm = TRUE) *
    100
)
