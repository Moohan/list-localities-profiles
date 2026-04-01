################################################################################ .
#                                                                              #
#               LOCALITY PROFILES: HOUSEHOLDS HSCP MANIPULATION                #
#                                                                              #
################################################################################ .
# This script performs HSCP-level aggregations and calculates partnership-wide
# statistics once per HSCP.

# 1. HSCP Level Data Preparation ----

# Filter lookup for current HSCP
hscp_dz <- lookup_dz_all |>
  filter(hscp2019name == HSCP)

# Count localities in this HSCP
n_loc <- length(unique(hscp_dz[["hscp_locality"]]))

# 2. Partnership (HSCP) Totals ----

house_dat_hscp <- house_raw_dat |>
  inner_join(hscp_dz, by = c("data_zone_code" = "datazone2011")) |>
  filter(year == max(year)) |>
  summarise(
    total_dwellings = sum(total_number_of_dwellings),
    tax_discount = sum(dwellings_with_a_single_adult_council_tax_discount)
  ) |>
  mutate(perc_discount = round_half_up(tax_discount / total_dwellings * 100, 1))

hscp_n_houses <- format_number_for_text(house_dat_hscp[["total_dwellings"]])
hscp_perc_discount <- house_dat_hscp[["perc_discount"]]

house_dat2_hscp <- house_raw_dat2 |>
  inner_join(hscp_dz, by = c("data_zone_code" = "datazone2011")) |>
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

# 3. All Localities in this HSCP (for summary table "Other Localities") ----

# Pre-calculate data for ALL localities in this HSCP.
# We will subset this in 1c to exclude the current LOCALITY.

house_dat_all_locs <- house_raw_dat |>
  inner_join(hscp_dz, by = c("data_zone_code" = "datazone2011")) |>
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
  inner_join(hscp_dz, by = c("data_zone_code" = "datazone2011")) |>
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

rm(hscp_dz, house_dat_hscp, house_dat2_hscp)
