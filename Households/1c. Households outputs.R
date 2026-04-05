################################################################################ .
#                                                                              #
#                      LOCALITY PROFILES: HOUSEHOLDS OUTPUTS                  #
#                                                                              #
################################################################################ .

# 1. Locality specific data zones ----

lookup_loc <- hscp_dz %>%
  dplyr::filter(hscp_locality == LOCALITY)

# 2. Households Data ----

# filter housing data for locality of interest
house_dat <- house_raw_dat %>%
  dplyr::filter(data_zone_code %in% lookup_loc[["datazone2011"]])

# aggregate data
house_dat1 <- house_dat %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    total_dwellings = sum(total_number_of_dwellings),
    occupied_dwellings = sum(occupied_dwellings),
    vacant_dwellings = sum(vacant_dwellings),
    second_homes = sum(second_homes),
    tax_exempt = sum(occupied_dwellings_exempt_from_paying_council_tax),
    tax_discount = sum(dwellings_with_a_single_adult_council_tax_discount)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(dplyr::across(3:7, list(perc = ~ 100 * .x / total_dwellings)))


## 2a) Text objects ----

# numbers
n_houses <- format_number_for_text(
  dplyr::filter(house_dat1, year == max(year))$total_dwellings
)
n_occupied <- format_number_for_text(
  dplyr::filter(house_dat1, year == max(year))$occupied_dwellings
)
n_vacant <- format_number_for_text(
  dplyr::filter(house_dat1, year == max(year))$vacant_dwellings
)
n_single_discount <- format_number_for_text(
  dplyr::filter(house_dat1, year == max(year))$tax_discount
)
n_exempt <- format_number_for_text(
  dplyr::filter(house_dat1, year == max(year))$tax_exempt
)
n_second_homes <- format_number_for_text(
  dplyr::filter(house_dat1, year == max(year))$second_homes
)

# percentages
perc_occupied <- format_number_for_text(
  dplyr::filter(house_dat1, year == max(year))$occupied_dwellings_perc
)
perc_vacant <- format_number_for_text(
  dplyr::filter(house_dat1, year == max(year))$vacant_dwellings_perc
)
perc_single_discount <- format_number_for_text(
  dplyr::filter(house_dat1, year == max(year))$tax_discount_perc
)
perc_exempt <- format_number_for_text(
  dplyr::filter(house_dat1, year == max(year))$tax_exempt_perc
)
perc_second_homes <- format_number_for_text(
  dplyr::filter(house_dat1, year == max(year))$second_homes_perc
)


## 2b) Plots and Tables ----

# Total dwellings over time
houses_ts <- ggplot2::ggplot(
  house_dat1,
  ggplot2::aes(x = year, y = total_dwellings, group = 1)
) +
  ggplot2::geom_line(linewidth = 1, colour = "#3F3685") +
  theme_profiles() +
  ggplot2::geom_point(color = "#3F3685") +
  ggplot2::geom_text(
    ggplot2::aes(label = format(total_dwellings, big.mark = ",")),
    vjust = 2,
    color = "#4a4a4a",
    size = 3.5
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::comma,
    limits = c(0, 1.1 * max(house_dat1$total_dwellings))
  ) +
  ggplot2::labs(
    x = "Year",
    y = "Number of Dwellings",
    title = paste0(
      "Number of Dwellings by Year in ",
      stringr::str_wrap(`LOCALITY`, 40),
      " ",
      max_year_housing
    ),
    caption = "Source: Council Tax billing system (via NRS)"
  ) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12))


# Table
house_table <- house_dat1 %>%
  dplyr::select(
    year,
    total_dwellings,
    occupied_dwellings,
    vacant_dwellings,
    tax_discount,
    tax_exempt,
    second_homes
  ) %>%
  dplyr::mutate(dplyr::across(2:7, ~ format(.x, big.mark = ",")))


# 3. Council Tax Band Data ----

# Filter and aggregate
house_dat2 <- house_raw_dat2 %>%
  dplyr::filter(data_zone_code %in% lookup_loc$datazone2011) %>%
  dplyr::select(5:14) %>%
  dplyr::summarise(dplyr::across(dplyr::everything(), sum))


## 3a) Plots & tables ----

ctb <- house_dat2 %>%
  dplyr::select(dplyr::matches("council_tax_band_[a-h]")) %>%
  tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable")

variable_names <- ctb[["variable"]]

pal_ctb <- phsstyles::phs_colours(c(
  "phs-magenta",
  "phs-magenta-80",
  "phs-magenta-50",
  "phs-magenta-10",
  "phs-purple-30",
  "phs-purple-50",
  "phs-purple-80",
  "phs-purple"
))

ctb_plot <- ctb %>%
  ggplot2::ggplot(ggplot2::aes(
    x = value,
    y = 1,
    fill = factor(variable, levels = rev(variable_names))
  )) +
  ggplot2::geom_col(
    position = "fill",
    colour = "black",
    size = 0.5,
    orientation = "y"
  ) +
  ggplot2::theme_classic() +
  ggplot2::labs(
    x = "Proportion of Households",
    y = "",
    caption = "Source: Scottish Assessors’ Association (via NRS)"
  ) +
  ggplot2::scale_fill_manual(
    name = "Council Tax Band",
    labels = paste("Band", LETTERS[8:1]),
    values = pal_ctb,
    drop = FALSE,
    guide = ggplot2::guide_legend(reverse = TRUE)
  ) +
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    legend.position = "top",
    legend.background = ggplot2::element_blank(),
    legend.box.background = ggplot2::element_rect(colour = "black")
  )

ctb_table <- ctb %>%
  dplyr::mutate(
    percent = paste0(format_number_for_text(100 * value / sum(value)), "%")
  ) %>%
  dplyr::select(-value) %>%
  tidyr::pivot_wider(names_from = variable, values_from = percent) %>%
  dplyr::mutate(`Tax Band` = "Percent of households") %>%
  dplyr::select(
    `Tax Band`,
    A = council_tax_band_a,
    B = council_tax_band_b,
    C = council_tax_band_c,
    D = council_tax_band_d,
    E = council_tax_band_e,
    `F` = council_tax_band_f,
    G = council_tax_band_g,
    H = council_tax_band_h
  )


## Objects for locality
perc_houses_AC <- format_number_for_text(
  sum(
    house_dat2$council_tax_band_a,
    house_dat2$council_tax_band_b,
    house_dat2$council_tax_band_c
  ) /
    house_dat2$total_number_of_dwellings *
    100
)

perc_houses_FH <- format_number_for_text(
  sum(
    house_dat2$council_tax_band_f,
    house_dat2$council_tax_band_g,
    house_dat2$council_tax_band_h
  ) /
    house_dat2$total_number_of_dwellings *
    100
)


# 4. Finalize 'Other Localities' Stats ----

other_locs_n_houses <- house_dat_all_locs %>%
  dplyr::filter(hscp_locality != LOCALITY) %>%
  dplyr::mutate(
    tot_dwellings_chr = formatC(total_dwellings, format = "d", big.mark = ",")
  ) %>%
  dplyr::arrange(hscp_locality) %>%
  dplyr::select(hscp_locality, tot_dwellings_chr) %>%
  tidyr::pivot_wider(
    names_from = hscp_locality,
    values_from = tot_dwellings_chr
  )

other_locs_perc_discount <- house_dat_all_locs %>%
  dplyr::filter(hscp_locality != LOCALITY) %>%
  dplyr::select(hscp_locality, tax_discount_perc) %>%
  dplyr::arrange(hscp_locality) %>%
  tidyr::pivot_wider(
    names_from = hscp_locality,
    values_from = tax_discount_perc
  )

other_locs_perc_housesAC <- house_dat2_all_locs %>%
  dplyr::filter(hscp_locality != LOCALITY) %>%
  dplyr::arrange(hscp_locality) %>%
  dplyr::select(hscp_locality, perc_houses_AC) %>%
  tidyr::pivot_wider(names_from = hscp_locality, values_from = perc_houses_AC)

other_locs_perc_housesFH <- house_dat2_all_locs %>%
  dplyr::filter(hscp_locality != LOCALITY) %>%
  dplyr::arrange(hscp_locality) %>%
  dplyr::select(hscp_locality, perc_houses_FH) %>%
  tidyr::pivot_wider(names_from = hscp_locality, values_from = perc_houses_FH)


# Housekeeping ----
rm(
  ctb,
  house_dat,
  house_dat1,
  house_dat2,
  lookup_loc,
  n_occupied,
  n_vacant,
  pal_ctb,
  perc_vacant,
  variable_names
)
gc()
