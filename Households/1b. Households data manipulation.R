################################################################################ .
#                                                                              #
#                 LOCALITY PROFILES: HOUSEHOLDS DATA MANIPULATION              #
#                                                                              #
################################################################################ .
###
### The purpose of this code is to perform HSCP-level data manipulation and
### calculations for households. This script should be sourced once per HSCP.
###

# Load required packages
library(dplyr)
library(tidyr)
library(phsmethods)

## 1. HSCP Lookups ----

# Global Script Function to read in Localities Lookup (DZ level)
hscp_dz_all <- read_in_localities(dz_level = TRUE) |>
  filter(hscp2019name == HSCP)


## 2. All Localities Aggregates (within HSCP) ----

# This pre-calculates data for all localities in the HSCP to avoid redundant
# filtering and joining in the inner loop.

house_dat_all_locs <- house_raw_dat |>
  inner_join(hscp_dz_all, by = c("data_zone_code" = "datazone2011")) |>
  filter(year == max(year)) |>
  group_by(hscp_locality) |>
  summarise(
    total_dwellings = sum(total_number_of_dwellings),
    tax_discount = sum(dwellings_with_a_single_adult_council_tax_discount)
  ) |>
  ungroup() |>
  mutate(
    tax_discount_perc = round_half_up(tax_discount / total_dwellings * 100, 1),
    tot_dwellings_chr = formatC(total_dwellings, format = "d", big.mark = ",")
  )

house_dat2_all_locs <- house_raw_dat2 |>
  inner_join(hscp_dz_all, by = c("data_zone_code" = "datazone2011")) |>
  group_by(hscp_locality) |>
  summarise(
    total_number_of_dwellings = sum(total_number_of_dwellings),
    band_a = sum(council_tax_band_a),
    band_b = sum(council_tax_band_b),
    band_c = sum(council_tax_band_c),
    band_f = sum(council_tax_band_f),
    band_g = sum(council_tax_band_g),
    band_h = sum(council_tax_band_h)
  ) |>
  ungroup() |>
  mutate(
    perc_houses_AC = round_half_up(
      (band_a + band_b + band_c) / total_number_of_dwellings * 100,
      1
    ),
    perc_houses_FH = round_half_up(
      (band_f + band_g + band_h) / total_number_of_dwellings * 100,
      1
    )
  )


## 3. HSCP Aggregates ----

house_dat_hscp <- house_raw_dat |>
  inner_join(hscp_dz_all, by = c("data_zone_code" = "datazone2011")) |>
  filter(year == max(year)) |>
  summarise(
    total_dwellings = sum(total_number_of_dwellings),
    tax_discount = sum(dwellings_with_a_single_adult_council_tax_discount)
  ) |>
  mutate(perc_discount = round_half_up(tax_discount / total_dwellings * 100, 1))

hscp_n_houses <- format_number_for_text(house_dat_hscp[["total_dwellings"]])
hscp_perc_discount <- house_dat_hscp[["perc_discount"]]

house_dat2_hscp <- house_raw_dat2 |>
  inner_join(hscp_dz_all, by = c("data_zone_code" = "datazone2011")) |>
  summarise(
    total_dwellings = sum(total_number_of_dwellings),
    band_a = sum(council_tax_band_a),
    band_b = sum(council_tax_band_b),
    band_c = sum(council_tax_band_c),
    band_f = sum(council_tax_band_f),
    band_g = sum(council_tax_band_g),
    band_h = sum(council_tax_band_h)
  ) |>
  mutate(
    perc_houses_AC = round_half_up(
      (band_a + band_b + band_c) / total_dwellings * 100,
      1
    ),
    perc_houses_FH = round_half_up(
      (band_f + band_g + band_h) / total_dwellings * 100,
      1
    )
  )

hscp_perc_housesAC <- house_dat2_hscp[["perc_houses_AC"]]
hscp_perc_housesFH <- house_dat2_hscp[["perc_houses_FH"]]


# Housekeeping ----
rm(
  hscp_dz_all,
  house_dat_hscp,
  house_dat2_hscp
)
