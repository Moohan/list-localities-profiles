############################################################################################# .
#                                                                                           #
#                     LOCALITY PROFILES GENERAL HEALTH DATA MANIPULATION                    #
#                                                                                           #
############################################################################################# .

## This script performs HSCP-level data manipulation for the General Health chapter
#  of the locality profiles. It should be sourced once per HSCP in the render loop.

# Determine HSCP and HB if not already set (defensive for testing)
if (!exists("HSCP") && exists("LOCALITY")) {
  HSCP <- as.character(dplyr::filter(lookup, hscp_locality == LOCALITY)$hscp2019name)
}
if (!exists("HB") && exists("LOCALITY")) {
  HB <- as.character(dplyr::filter(lookup, hscp_locality == LOCALITY)$hb2019name)
}

# Find number of locs per partnership
n_loc <- count_localities(lookup, HSCP)

# Determine other localities based on HSCP
all_locs_in_hscp <- lookup |>
  dplyr::select(hscp_locality, hscp2019name) |>
  dplyr::distinct()

# Extract SLF adjusted populations for HSCP
slf_pops_hscp <- slf_pops_all |>
  dplyr::filter(hscp2019name == HSCP)

ltc_pops_total_hscp <- sum(slf_pops_hscp$slf_adj_pop)

# HSCP-level life expectancy summary
if (HSCP == "Clackmannanshire and Stirling") {
  hscp_life_exp_male <- NA_real_
  hscp_life_exp_fem <- NA_real_
} else {
  hscp_life_exp_male <- life_exp |>
    dplyr::filter(
      year == latest_year_life_exp_otherareas,
      area_name == HSCP,
      area_type == "HSCP",
      sex == "Male"
    ) |>
    dplyr::pull(measure) |>
    phsmethods::round_half_up(digits = 1)

  hscp_life_exp_fem <- life_exp |>
    dplyr::filter(
      year == latest_year_life_exp_otherareas,
      area_name == HSCP,
      area_type == "HSCP",
      sex == "Female"
    ) |>
    dplyr::pull(measure) |>
    phsmethods::round_half_up(digits = 1)
}

hscp_deaths_15_44 <- deaths_15_44 |>
  dplyr::filter(
    year == max(deaths_15_44$year),
    area_name == HSCP,
    area_type == "HSCP"
  ) |>
  dplyr::pull(measure) |>
  phsmethods::round_half_up(digits = 1)

hscp_cancer <- cancer_reg |>
  dplyr::filter(
    year == max(cancer_reg$year),
    area_name == HSCP,
    area_type == "HSCP"
  ) |>
  dplyr::pull(measure) |>
  phsmethods::round_half_up(digits = 1)

hscp_adp <- adp_presc |>
  dplyr::filter(
    year == max(adp_presc$year),
    area_name == HSCP,
    area_type == "HSCP"
  ) |>
  dplyr::pull(measure) |>
  phsmethods::round_half_up(digits = 1)

# Top 5 HSCP LTCs
ltc_totals_hscp <- ltc |>
  dplyr::filter(hscp2019name == HSCP) |>
  dplyr::select(-year, -hscp_locality, -hscp2019name, -total_ltc, -slf_adj_pop) |>
  dplyr::group_by(age_group) |>
  dplyr::summarise(dplyr::across(everything(), sum), .groups = "drop") |>
  dplyr::select(-age_group, -people) |>
  dplyr::summarise(dplyr::across(everything(), sum)) |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "topltc",
    values_to = "value"
  ) |>
  dplyr::slice_max(n = 5, order_by = value, with_ties = FALSE) |>
  dplyr::mutate(percent = phsmethods::round_half_up((value / ltc_pops_total_hscp) * 100, 2))

top5ltc_hscp <- ltc_totals_hscp |>
  dplyr::left_join(ltc_colours_global, by = dplyr::join_by(topltc)) |>
  dplyr::mutate(Prevalence = stringr::str_c(topltc, paste(percent, "%"), sep = "\n"))

# Pre-calculate LTC percentage for HSCP for summary table
ltc_hscp_people <- sum(dplyr::filter(ltc, hscp2019name == HSCP, total_ltc > 0)$people)
hscp_ltc_summary_val <- phsmethods::round_half_up(ltc_hscp_people / ltc_pops_total_hscp * 100, 1)

# Clean up temporary objects
rm(ltc_hscp_people, ltc_totals_hscp, slf_pops_hscp)
