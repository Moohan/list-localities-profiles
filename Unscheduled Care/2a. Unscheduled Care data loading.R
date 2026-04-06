##################### LOCALITY PROFILES UNSCHEDULED CARE: DATA LOADING ######################

## Packages
library(scales)

## Constants
ext_year <- 2025
import_folder <- paste0(lp_path, "Unscheduled Care/DATA ", ext_year, "/")
max_fy <- "2024/25" # TODO Change this to be dynamic and move to general!

## Helper Functions

# Functions for creating time trends
age_group_trend_usc <- function(
  data_for_plot,
  plot_title,
  yaxis_title,
  source
) {
  data_for_plot %>%
    ggplot(aes(
      x = financial_year,
      y = data,
      group = age_group,
      color = age_group
    )) +
    geom_line(linewidth = 1) +
    geom_point() +
    scale_colour_manual(values = c(palette)) +
    scale_x_discrete(breaks = data_for_plot$financial_year) +
    scale_y_continuous(
      labels = comma,
      limits = c(0, 1.1 * max(data_for_plot$data))
    ) +
    theme_profiles() +
    labs(
      title = plot_title,
      y = yaxis_title,
      x = "Financial Year",
      color = "Age Group",
      caption = source
    ) +
    theme(plot.title = element_text(hjust = 0.5, size = 12))
}

area_trend_usc <- function(data_for_plot, plot_title, yaxis_title, source) {
  data_for_plot %>%
    mutate(
      location = fct_reorder(
        as.factor(str_wrap(location, 23)),
        as.numeric(area_type)
      )
    ) %>%
    ggplot() +
    aes(
      x = financial_year,
      y = data,
      group = location,
      fill = location,
      linetype = area_type
    ) +
    geom_line(aes(colour = location), linewidth = 1) +
    geom_point(aes(colour = location), size = 2) +
    scale_fill_manual(values = palette) +
    scale_colour_manual(values = palette) +
    theme_profiles() +
    expand_limits(y = 0) +
    scale_x_discrete(breaks = data_for_plot$financial_year) +
    scale_y_continuous(
      labels = comma,
      limits = c(0, 1.1 * max(data_for_plot$data))
    ) +
    labs(
      title = plot_title,
      y = yaxis_title,
      x = "Financial Year",
      caption = source
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      legend.title = element_blank()
    ) +
    guides(
      linetype = "none",
      shape = "none",
      fill = "none",
      colour = guide_legend(nrow = 1, byrow = TRUE)
    )
}

percent_change_calc <- function(numerator, denominator, digits = 1) {
  round_half_up(
    abs(numerator - denominator) / denominator * 100,
    digits = digits
  )
}

word_change_calc <- function(latest, first) {
  dplyr::case_when(
    dplyr::near(latest, first) ~ "change",
    latest > first ~ "increase",
    latest < first ~ "decrease"
  )
}

## Data Loading

emergency_adm_raw <- read_parquet(paste0(
  import_folder,
  "emergency_admissions_msg.parquet"
)) %>%
  filter(financial_year <= max_fy)

bed_days_raw <- read_parquet(paste0(import_folder, "bed_days_msg.parquet")) %>%
  filter(financial_year <= max_fy)

bed_days_mh_raw <- read_parquet(paste0(
  import_folder,
  "bed_days_mh_msg.parquet"
)) %>%
  filter(financial_year <= max_fy)

ae_attendances_raw <- read_parquet(paste0(
  import_folder,
  "ae_attendances_msg.parquet"
)) %>%
  filter(financial_year <= max_fy)

delayed_disch_raw <- read_parquet(paste0(
  import_folder,
  "delayed_discharges_msg.parquet"
)) %>%
  filter(financial_year <= max_fy) %>%
  filter(age_group %in% c("65 - 74", "75+"))

falls_raw <- read_parquet(paste0(import_folder, "falls_smr.parquet")) %>%
  filter(financial_year <= max_fy) %>%
  filter(age_group %in% c("65 - 74", "75+"))

readmissions_raw <- read_parquet(paste0(
  import_folder,
  "readmissions_smr.parquet"
)) %>%
  filter(financial_year <= max_fy)

ppa_raw <- read_parquet(paste0(import_folder, "ppa_smr.parquet")) %>%
  filter(financial_year <= max_fy)

psych_hosp_raw <- read_csv(paste0(
  import_folder,
  "scotpho_data_extract_psychiatric_admissions.csv"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 18), fixed = TRUE))

## Population data

populations_raw <- read_in_dz_pops()

# compute age bands
populations_raw$"Pop0_17" <- rowSums(subset(
  populations_raw,
  select = age0:age17
))
populations_raw$"Pop18_44" <- rowSums(subset(
  populations_raw,
  select = age18:age44
))
populations_raw$"Pop45_64" <- rowSums(subset(
  populations_raw,
  select = age45:age64
))
populations_raw$"Pop65_74" <- rowSums(subset(
  populations_raw,
  select = age65:age74
))
populations_raw$"Pop75Plus" <- rowSums(subset(
  populations_raw,
  select = age75:age90plus
))
populations_raw$"Pop65Plus" <- rowSums(subset(
  populations_raw,
  select = age65:age90plus
))

pops <- populations_raw %>%
  select(
    year,
    hb2019name,
    hscp2019name,
    hscp_locality,
    Pop0_17,
    Pop18_44,
    Pop45_64,
    Pop65_74,
    Pop75Plus,
    Pop65Plus,
    total_pop
  ) %>%
  mutate(financial_year = paste0(year, "/", substr(year + 1, 3, 4))) %>%
  group_by(financial_year, year, hb2019name, hscp2019name, hscp_locality) %>%
  summarise(across(everything(), sum)) %>%
  ungroup()

loc_pop <- pops %>%
  pivot_longer(
    "Pop0_17":"total_pop",
    names_to = "age_group",
    values_to = "pop"
  ) %>%
  mutate(
    age_group = case_when(
      age_group == "Pop0_17" ~ "0 - 17",
      age_group == "Pop18_44" ~ "18 - 44",
      age_group == "Pop45_64" ~ "45 - 64",
      age_group == "Pop65_74" ~ "65 - 74",
      age_group == "Pop75Plus" ~ "75+",
      age_group == "Pop65Plus" ~ "65+",
      age_group == "total_pop" ~ "Total"
    )
  )
