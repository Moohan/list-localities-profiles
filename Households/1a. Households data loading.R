################################################################################ .
#                                                                              #
#                   LOCALITY PROFILES: HOUSEHOLDS (LOADING)                    #
#                                                                              #
################################################################################ .

# Update Data Year (this is the maximum year available for both housing data sets from NRS)
max_year_housing <- 2024
# Update Publication Year (the year marked on the Data folder)
ext_year <- 2025

# get historic housing data, each year is on a separate sheet so map over it
house_raw_dat <- purrr::map(
  2014L:max_year_housing,
  \(year) {
    readxl::read_excel(
      fs::path(
        lp_path,
        "Households",
        glue::glue("Data {ext_year}"),
        "household_estimates.xlsx"
      ),
      col_types = c(rep("text", 4L), rep("numeric", 8L), rep("skip", 7L)),
      sheet = as.character(year),
      skip = 3L,
      .name_repair = janitor::make_clean_names
    ) |>
      dplyr::mutate(year = year, .before = everything())
  }
) |>
  dplyr::bind_rows()

# Council Tax Band Data
house_raw_dat2 <- readxl::read_excel(
  fs::path(
    lp_path,
    "Households",
    glue::glue("Data {ext_year}"),
    "council_tax.xlsx"
  ),
  sheet = as.character(max_year_housing),
  skip = 4
) |>
  janitor::clean_names()

# Global localities lookup for datazones
lookup_dz_all <- read_in_localities(dz_level = TRUE)

# 3. Scotland
scot_n_houses <- format_number_for_text(sum(
  dplyr::filter(house_raw_dat, year == max(year))$total_number_of_dwellings,
  na.rm = TRUE
))
scot_perc_discount <- format_number_for_text(
  sum(
    dplyr::filter(
      house_raw_dat,
      year == max(year)
    )$dwellings_with_a_single_adult_council_tax_discount,
    na.rm = TRUE
  ) /
    sum(
      dplyr::filter(house_raw_dat, year == max(year))$total_number_of_dwellings,
      na.rm = TRUE
    ) *
    100
)

scot_perc_housesAC <- format_number_for_text(
  sum(
    house_raw_dat2$council_tax_band_a,
    house_raw_dat2$council_tax_band_b,
    house_raw_dat2$council_tax_band_c,
    na.rm = TRUE
  ) /
    sum(house_raw_dat2$total_number_of_dwellings, na.rm = TRUE) *
    100
)
scot_perc_housesFH <- format_number_for_text(
  sum(
    house_raw_dat2$council_tax_band_f,
    house_raw_dat2$council_tax_band_g,
    house_raw_dat2$council_tax_band_h,
    na.rm = TRUE
  ) /
    sum(house_raw_dat2$total_number_of_dwellings, na.rm = TRUE) *
    100
)
