################################################################################ .
#                                                                              #
#                   LOCALITY PROFILES: HOUSEHOLDS (OUTPUTS)                    #
#                                                                              #
################################################################################ .

# filter housing data for locality of interest
house_dat1 <- all_locs_dat %>% dplyr::filter(hscp_locality == LOCALITY)

## 2b) Text objects ----

# numbers
n_houses <- format_number_for_text(
  dplyr::filter(house_dat1, year == max(year))$total_dwellings
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
perc_single_discount <- format_number_for_text(
  dplyr::filter(house_dat1, year == max(year))$tax_discount_perc
)
perc_exempt <- format_number_for_text(
  dplyr::filter(house_dat1, year == max(year))$tax_exempt_perc
)
perc_second_homes <- format_number_for_text(
  dplyr::filter(house_dat1, year == max(year))$second_homes_perc
)


## 2c) Plots and Tables ----

# Total dwellings over time
houses_ts <- ggplot(house_dat1, aes(x = year, y = total_dwellings, group = 1)) +
  geom_line(linewidth = 1, colour = "#3F3685") +
  theme_profiles() +
  geom_point(color = "#3F3685") +
  geom_text(
    aes(label = format(total_dwellings, big.mark = ",")),
    vjust = 2,
    color = "#4a4a4a",
    size = 3.5
  ) +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, 1.1 * max(house_dat1$total_dwellings))
  ) +
  labs(
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
  theme(plot.title = element_text(size = 12))


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


######################## Section 3 - Council Tax Band Data ############################

# Filter for locality
house_dat2 <- all_locs_dat2 %>% dplyr::filter(hscp_locality == LOCALITY)

## 3b) Plots & tables ----

ctb <- house_dat2 %>%
  dplyr::select(tidyselect::matches("council_tax_band_[a-h]")) %>%
  tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "variable")

variable <- ctb[["variable"]]

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
  ggplot(aes(
    x = value,
    y = 1,
    fill = factor(variable, levels = rev(variable))
  )) +
  geom_col(position = "fill", colour = "black", size = 0.5, orientation = "y") +
  theme_classic() +
  labs(
    x = "Proportion of Households",
    y = "",
    caption = "Source: Scottish Assessors’ Association (via NRS)"
  ) +
  scale_fill_manual(
    name = "Council Tax Band",
    labels = paste("Band", LETTERS[8:1]),
    values = pal_ctb,
    drop = FALSE,
    guide = guide_legend(reverse = TRUE)
  ) +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black")
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

########################## Section 4 - Objects for Summary Table ########################

# Determine other localities based on LOCALITY object
other_locs <- lookup_hscp %>%
  dplyr::select(hscp_locality, hscp2019name) %>%
  dplyr::filter(hscp2019name == HSCP & hscp_locality != LOCALITY) %>%
  dplyr::arrange(hscp_locality)

# Find number of locs per partnership
n_loc <- count_localities(lookup_hscp, HSCP)

# 1. Other localities
other_locs_dat_raw <- all_locs_dat %>%
  dplyr::filter(hscp_locality %in% other_locs$hscp_locality, year == max(year))

other_locs_n_houses <- other_locs_dat_raw %>%
  dplyr::mutate(
    tot_dwellings_chr = formatC(total_dwellings, format = "d", big.mark = ",")
  ) %>%
  dplyr::arrange(hscp_locality) %>%
  dplyr::select(hscp_locality, tot_dwellings_chr) %>%
  tidyr::pivot_wider(names_from = hscp_locality, values_from = tot_dwellings_chr)

other_locs_perc_discount <- other_locs_dat_raw %>%
  dplyr::mutate(tax_discount_perc = janitor::round_half_up(tax_discount_perc, 1)) %>%
  dplyr::select(hscp_locality, tax_discount_perc) %>%
  dplyr::arrange(hscp_locality) %>%
  tidyr::pivot_wider(names_from = hscp_locality, values_from = tax_discount_perc)

other_locs_dat2_raw <- all_locs_dat2 %>%
  dplyr::filter(hscp_locality %in% other_locs$hscp_locality) %>%
  dplyr::mutate(
    perc_houses_AC = janitor::round_half_up(
      (council_tax_band_a + council_tax_band_b + council_tax_band_c) / total_number_of_dwellings * 100,
      1
    ),
    perc_houses_FH = janitor::round_half_up(
      (council_tax_band_f + council_tax_band_g + council_tax_band_h) / total_number_of_dwellings * 100,
      1
    )
  )

other_locs_perc_housesAC <- other_locs_dat2_raw %>%
  dplyr::arrange(hscp_locality) %>%
  dplyr::select(hscp_locality, perc_houses_AC) %>%
  tidyr::pivot_wider(names_from = hscp_locality, values_from = perc_houses_AC)

other_locs_perc_housesFH <- other_locs_dat2_raw %>%
  dplyr::arrange(hscp_locality) %>%
  dplyr::select(hscp_locality, perc_houses_FH) %>%
  tidyr::pivot_wider(names_from = hscp_locality, values_from = perc_houses_FH)

# Clean up
rm(ctb, other_locs_dat_raw, other_locs_dat2_raw, variable, pal_ctb)
