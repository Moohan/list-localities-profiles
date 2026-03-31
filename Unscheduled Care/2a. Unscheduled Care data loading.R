##################### LOCALITY PROFILES UNSCHEDULED CARE: DATA LOADING ######################.

## Manually set year that the profiles are being run (year on data folder)
ext_year <- 2025
import_folder <- fs::path(
  lp_path,
  "Unscheduled Care",
  paste0("DATA ", ext_year)
)
max_fy <- "2024/25" # TODO Change this to be dynamic and move to general!

## Packages
library(scales)

# Functions for aggregating data
# For this function to work, the main variable of the data (ex: number of admissions) must be renamed "n"

aggregate_usc_area_data <- function(data) {
  pts_locality <- data %>%
    filter(hscp_locality == LOCALITY) %>%
    mutate(location = hscp_locality) %>%
    group_by(financial_year, location) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    mutate(area_type = "Locality")

  pts_hscp <- data %>%
    filter(hscp2019name == HSCP) %>%
    mutate(location = hscp2019name) %>%
    group_by(financial_year, location) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    mutate(area_type = "HSCP")

  pts_hb <- data %>%
    left_join(
      select(localities, hscp_locality, hb2019name),
      by = join_by(hscp_locality)
    ) %>%
    filter(hb2019name == HB) %>%
    mutate(location = hb2019name) %>%
    group_by(financial_year, location) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    mutate(area_type = "HB")

  pts_scot <- data %>%
    group_by(financial_year) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    mutate(
      location = "Scotland",
      area_type = "Scotland"
    )

  bind_rows(pts_locality, pts_hscp, pts_hb, pts_scot) %>%
    mutate(
      area_type = factor(
        area_type,
        levels = c("Locality", "HSCP", "HB", "Scotland")
      )
    )
}

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

# Functions for text variables

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

## Load Data ----

emergency_adm_raw <- read_parquet(fs::path(
  import_folder,
  "emergency_admissions_msg.parquet"
))
bed_days_raw <- read_parquet(fs::path(import_folder, "bed_days_msg.parquet"))
bed_days_mh_raw <- read_parquet(fs::path(
  import_folder,
  "bed_days_mh_msg.parquet"
))
ae_attendances_raw <- read_parquet(fs::path(
  import_folder,
  "ae_attendances_msg.parquet"
))
delayed_disch_raw <- read_parquet(fs::path(
  import_folder,
  "delayed_discharges_msg.parquet"
))
falls_raw <- read_parquet(fs::path(import_folder, "falls_smr.parquet"))
readmissions_raw <- read_parquet(fs::path(
  import_folder,
  "readmissions_smr.parquet"
))
ppa_raw <- read_parquet(fs::path(import_folder, "ppa_smr.parquet"))
psych_hosp_raw <- read_csv(fs::path(
  import_folder,
  "scotpho_data_extract_psychiatric_admissions.csv"
))
