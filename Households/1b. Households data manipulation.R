################################################################################ .
#                                                                              #
#                 LOCALITY PROFILES: HOUSEHOLDS (MANIPULATION)                 #
#                                                                              #
################################################################################ .

# Global Script Function to read in Localities Lookup (Locality level)
lookup_hscp <- read_in_localities(dz_level = FALSE)

# 2. HSCP
hscp_dz <- lookup_dz_all %>%
  dplyr::select(datazone2011, hscp2019name) %>%
  dplyr::filter(hscp2019name == HSCP)

house_dat_hscp_raw <- house_raw_dat %>%
  dplyr::inner_join(hscp_dz, by = c("data_zone_code" = "datazone2011")) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    total_dwellings = sum(total_number_of_dwellings),
    tax_discount = sum(dwellings_with_a_single_adult_council_tax_discount)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(perc_discount = janitor::round_half_up(tax_discount / total_dwellings * 100, 1))

hscp_n_houses <- format_number_for_text(house_dat_hscp_raw$total_dwellings)
hscp_perc_discount <- house_dat_hscp_raw$perc_discount

house_dat2_hscp_raw <- house_raw_dat2 %>%
  dplyr::inner_join(hscp_dz, by = c("data_zone_code" = "datazone2011")) %>%
  dplyr::group_by(hscp2019name) %>%
  dplyr::summarise(
    total_dwellings = sum(total_number_of_dwellings),
    band_a = sum(council_tax_band_a),
    band_b = sum(council_tax_band_b),
    band_c = sum(council_tax_band_c),
    band_f = sum(council_tax_band_f),
    band_g = sum(council_tax_band_g),
    band_h = sum(council_tax_band_h)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    perc_houses_AC = janitor::round_half_up(
      (band_a + band_b + band_c) / total_dwellings * 100,
      1
    ),
    perc_houses_FH = janitor::round_half_up(
      (band_f + band_g + band_h) / total_dwellings * 100,
      1
    )
  )

hscp_perc_housesAC <- house_dat2_hscp_raw$perc_houses_AC
hscp_perc_housesFH <- house_dat2_hscp_raw$perc_houses_FH

# 1. Pre-calculate for all localities in HSCP (for "other localities" logic)
hscp_localities_dz <- lookup_dz_all %>%
  dplyr::filter(hscp2019name == HSCP) %>%
  dplyr::select(datazone2011, hscp_locality)

all_locs_dat <- house_raw_dat %>%
  dplyr::inner_join(hscp_localities_dz, by = c("data_zone_code" = "datazone2011")) %>%
  dplyr::group_by(year, hscp_locality) %>%
  dplyr::summarise(
    total_dwellings = sum(total_number_of_dwellings),
    occupied_dwellings = sum(occupied_dwellings),
    vacant_dwellings = sum(vacant_dwellings),
    second_homes = sum(second_homes),
    tax_exempt = sum(occupied_dwellings_exempt_from_paying_council_tax),
    tax_discount = sum(dwellings_with_a_single_adult_council_tax_discount)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    total_dwellings_perc = 100,
    occupied_dwellings_perc = 100 * occupied_dwellings / total_dwellings,
    vacant_dwellings_perc = 100 * vacant_dwellings / total_dwellings,
    second_homes_perc = 100 * second_homes / total_dwellings,
    tax_exempt_perc = 100 * tax_exempt / total_dwellings,
    tax_discount_perc = 100 * tax_discount / total_dwellings
  )

all_locs_dat2 <- house_raw_dat2 %>%
  dplyr::inner_join(hscp_localities_dz, by = c("data_zone_code" = "datazone2011")) %>%
  dplyr::group_by(hscp_locality) %>%
  dplyr::summarise(
    total_number_of_dwellings = sum(total_number_of_dwellings),
    council_tax_band_a = sum(council_tax_band_a),
    council_tax_band_b = sum(council_tax_band_b),
    council_tax_band_c = sum(council_tax_band_c),
    council_tax_band_d = sum(council_tax_band_d),
    council_tax_band_e = sum(council_tax_band_e),
    council_tax_band_f = sum(council_tax_band_f),
    council_tax_band_g = sum(council_tax_band_g),
    council_tax_band_h = sum(council_tax_band_h)
  ) %>%
  dplyr::ungroup()

# Clean up
rm(hscp_dz, house_dat_hscp_raw, house_dat2_hscp_raw, hscp_localities_dz)
