################################################################################ .
#                                                                              #
#                 LOCALITY PROFILES: HOUSEHOLDS MANIPULATION                   #
#                                                                              #
################################################################################ .

# 1. HSCP Data Zones ----

hscp_dz <- read_in_localities(dz_level = TRUE) %>%
  dplyr::select(datazone2011, hscp2019name, hscp_locality) %>%
  dplyr::filter(hscp2019name == HSCP)

# 2. HSCP Stats ----

house_dat_hscp <- house_raw_dat %>%
  dplyr::inner_join(hscp_dz, by = c("data_zone_code" = "datazone2011")) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    total_dwellings = sum(total_number_of_dwellings),
    tax_discount = sum(dwellings_with_a_single_adult_council_tax_discount)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    perc_discount = phsmethods::round_half_up(
      tax_discount / total_dwellings * 100,
      1
    )
  )

hscp_n_houses <- format_number_for_text(house_dat_hscp$total_dwellings)
hscp_perc_discount <- house_dat_hscp$perc_discount

house_dat2_hscp <- house_raw_dat2 %>%
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
    perc_houses_AC = phsmethods::round_half_up(
      (band_a + band_b + band_c) / total_dwellings * 100,
      1
    ),
    perc_houses_FH = phsmethods::round_half_up(
      (band_f + band_g + band_h) / total_dwellings * 100,
      1
    )
  )

hscp_perc_housesAC <- house_dat2_hscp$perc_houses_AC
hscp_perc_housesFH <- house_dat2_hscp$perc_houses_FH

# 3. Pre-calculate Locality Stats for the entire HSCP ----

house_dat_all_locs <- house_raw_dat %>%
  dplyr::inner_join(hscp_dz, by = c("data_zone_code" = "datazone2011")) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::group_by(hscp_locality) %>%
  dplyr::summarise(
    total_dwellings = sum(total_number_of_dwellings),
    tax_discount = sum(dwellings_with_a_single_adult_council_tax_discount)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    tax_discount_perc = phsmethods::round_half_up(
      tax_discount / total_dwellings * 100,
      1
    )
  )

house_dat2_all_locs <- house_raw_dat2 %>%
  dplyr::inner_join(hscp_dz, by = c("data_zone_code" = "datazone2011")) %>%
  dplyr::group_by(hscp_locality) %>%
  dplyr::summarise(
    total_number_of_dwellings = sum(total_number_of_dwellings),
    band_a = sum(council_tax_band_a),
    band_b = sum(council_tax_band_b),
    band_c = sum(council_tax_band_c),
    band_f = sum(council_tax_band_f),
    band_g = sum(council_tax_band_g),
    band_h = sum(council_tax_band_h)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    perc_houses_AC = phsmethods::round_half_up(
      (band_a + band_b + band_c) / total_number_of_dwellings * 100,
      1
    ),
    perc_houses_FH = phsmethods::round_half_up(
      (band_f + band_g + band_h) / total_number_of_dwellings * 100,
      1
    )
  )
