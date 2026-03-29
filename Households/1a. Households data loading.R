################################################################################ .
#                                                                              #
#                   LOCALITY PROFILES: HOUSEHOLDS DATA LOADING                 #
#                                                                              #
################################################################################ .
###
### The purpose of this code is to load the raw household data once per session.
### This includes Scotland-level aggregations which do not change between HSCPs.
###

# Load in required packages
library(readxl)
library(purrr)
library(dplyr)
library(fs)
library(glue)
library(janitor)

# Update Data Year (this is the maximum year available for both housing data sets from NRS)
max_year_housing <- 2023
# Update Publication Year (the year marked on the Data folder)
ext_year <- 2024

## 1. Households Data (estimates) ----

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


## 2. Council Tax Band Data ----

# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/households/household-estimates/small-area-statistics-on-households-and-dwellings

house_raw_dat2 <- read_excel(
  path(lp_path, "Households", glue("Data {ext_year}"), "council_tax.xlsx"),
  sheet = as.character(max_year_housing),
  skip = 4L
) |>
  clean_names()


## 3. Scotland-level aggregates ----

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
