################################################################################ .
#                                                                              #
#                      LOCALITY PROFILES: HOUSEHOLDS OUTPUTS                   #
#                                                                              #
################################################################################ .
###
### The purpose of this code is to produce graphs, tables, and text objects for
### the locality profiles. This script should be sourced once per locality.
###

# Load required packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(phsmethods)

## 1. Locality Lookup & Filtering ----

# Global Script Function to read in Localities Lookup
lookup <- read_in_localities(dz_level = TRUE) |>
  select(datazone2011, hscp_locality) |>
  filter(hscp_locality == LOCALITY)

# filter housing data for locality of interest
house_dat <- house_raw_dat |>
  filter(data_zone_code %in% lookup[["datazone2011"]])

# aggregate data
house_dat1 <- house_dat |>
  group_by(year) |>
  summarise(
    total_dwellings = sum(total_number_of_dwellings),
    occupied_dwellings = sum(occupied_dwellings),
    vacant_dwellings = sum(vacant_dwellings),
    second_homes = sum(second_homes),
    tax_exempt = sum(occupied_dwellings_exempt_from_paying_council_tax),
    tax_discount = sum(dwellings_with_a_single_adult_council_tax_discount)
  ) |>
  ungroup() |>
  mutate(across(3:7, list(perc = ~ 100 * .x / total_dwellings)))


## 2. Text objects ----

# numbers
n_houses <- format_number_for_text(
  filter(house_dat1, year == max(year))[["total_dwellings"]]
)
n_occupied <- format_number_for_text(
  filter(house_dat1, year == max(year))[["occupied_dwellings"]]
)
n_vacant <- format_number_for_text(
  filter(house_dat1, year == max(year))[["vacant_dwellings"]]
)
n_single_discount <- format_number_for_text(
  filter(house_dat1, year == max(year))[["tax_discount"]]
)
n_exempt <- format_number_for_text(
  filter(house_dat1, year == max(year))[["tax_exempt"]]
)
n_second_homes <- format_number_for_text(
  filter(house_dat1, year == max(year))[["second_homes"]]
)

# percentages
perc_occupied <- format_number_for_text(
  filter(house_dat1, year == max(year))[["occupied_dwellings_perc"]]
)
perc_vacant <- format_number_for_text(
  filter(house_dat1, year == max(year))[["vacant_dwellings_perc"]]
)
perc_single_discount <- format_number_for_text(
  filter(house_dat1, year == max(year))[["tax_discount_perc"]]
)
perc_exempt <- format_number_for_text(
  filter(house_dat1, year == max(year))[["tax_exempt_perc"]]
)
perc_second_homes <- format_number_for_text(
  filter(house_dat1, year == max(year))[["second_homes_perc"]]
)


## 3. Plots and Tables ----

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
    limits = c(0, 1.1 * max(house_dat1[["total_dwellings"]]))
  ) +
  labs(
    x = "Year",
    y = "Number of Dwellings",
    title = paste0(
      "Number of Dwellings by Year in ",
      str_wrap(`LOCALITY`, 40),
      " ",
      max_year_housing
    ),
    caption = "Source: Council Tax billing system (via NRS)"
  ) +
  theme(plot.title = element_text(size = 12))

# Table for Excel output
house_table <- house_dat1 |>
  select(
    year,
    total_dwellings,
    occupied_dwellings,
    vacant_dwellings,
    tax_discount,
    tax_exempt,
    second_homes
  ) |>
  mutate(across(2:7, ~ format(.x, big.mark = ",")))


## 4. Council Tax Band Data ----

# Filter and aggregate
house_dat2 <- house_raw_dat2 |>
  filter(data_zone_code %in% lookup[["datazone2011"]]) |>
  select(5:14) |>
  summarise(across(everything(), sum))

ctb <- house_dat2 |>
  select(matches("council_tax_band_[a-h]")) |>
  pivot_longer(cols = everything(), names_to = "variable")

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

ctb_plot <- ctb |>
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

ctb_table <- ctb |>
  mutate(
    percent = paste0(format_number_for_text(100 * value / sum(value)), "%")
  ) |>
  select(-value) |>
  pivot_wider(names_from = variable, values_from = percent) |>
  mutate(`Tax Band` = "Percent of households") |>
  select(
    `Tax Band`,
    A = council_tax_band_a,
    B = council_tax_band_b,
    C = council_tax_band_c,
    D = council_tax_band_d,
    E = council_tax_band_e,
    F = council_tax_band_f,
    G = council_tax_band_g,
    H = council_tax_band_h
  )

perc_houses_AC <- format_number_for_text(
  sum(
    house_dat2[["council_tax_band_a"]],
    house_dat2[["council_tax_band_b"]],
    house_dat2[["council_tax_band_c"]]
  ) /
    house_dat2[["total_number_of_dwellings"]] *
    100
)

perc_houses_FH <- format_number_for_text(
  sum(
    house_dat2[["council_tax_band_f"]],
    house_dat2[["council_tax_band_g"]],
    house_dat2[["council_tax_band_h"]]
  ) /
    house_dat2[["total_number_of_dwellings"]] *
    100
)


## 5. Summary Table (Other Localities) ----

other_locs_n_houses <- house_dat_all_locs |>
  filter(hscp_locality != LOCALITY) |>
  arrange(hscp_locality) |>
  select(hscp_locality, tot_dwellings_chr) |>
  pivot_wider(names_from = hscp_locality, values_from = tot_dwellings_chr)

other_locs_perc_discount <- house_dat_all_locs |>
  filter(hscp_locality != LOCALITY) |>
  select(hscp_locality, tax_discount_perc) |>
  arrange(hscp_locality) |>
  pivot_wider(names_from = hscp_locality, values_from = tax_discount_perc)

other_locs_perc_housesAC <- house_dat2_all_locs |>
  filter(hscp_locality != LOCALITY) |>
  arrange(hscp_locality) |>
  select(hscp_locality, perc_houses_AC) |>
  pivot_wider(names_from = hscp_locality, values_from = perc_houses_AC)

other_locs_perc_housesFH <- house_dat2_all_locs |>
  filter(hscp_locality != LOCALITY) |>
  arrange(hscp_locality) |>
  select(hscp_locality, perc_houses_FH) |>
  pivot_wider(names_from = hscp_locality, values_from = perc_houses_FH)


# Housekeeping ----
rm(
  ctb,
  pal_ctb,
  variable
)
