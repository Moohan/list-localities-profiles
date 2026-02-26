##################### LOCALITY PROFILES UNSCHEDULED CARE: OUTPUTS ######################.

# Original author: Will Clayton
# Updated Oct/Nov 2022 by Adam Rennie to use Global Script functions
# Last edits Late Nov 22 by Luke Taylor and Cecilia Puech to tidy up script and change outputs

####################### SECTION 1: Packages, file paths, etc #########################

## Manually set year that the profiles are being run (year on data folder)
ext_year <- 2024

# Set locality profiles file path
# lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"
import_folder <- paste0(lp_path, "Unscheduled Care/DATA ", ext_year, "/")

### for testing run global script and locality placeholder below

## Packages
library(scales)

## Functions
# source("Master RMarkdown Document & Render Code/Global Script.R")

## Define locality
# LOCALITY <- "Stirling City with the Eastern Villages Bridge of Allan and Dunblane"
# LOCALITY <- "Inverness"
# LOCALITY <- "Ayr North and Former Coalfield Communities"
# LOCALITY <- "Whalsay and Skerries"
# LOCALITY <- "North Perthshire"
# LOCALITY <- "Inverclyde East"
# Set date limit for financial year
# Unless we're in Q4 use the previous FY as the max
# max_fy <- ifelse(
#   lubridate::quarter(Sys.Date(), fiscal_start = 4) != 4,
#   phsmethods::extract_fin_year(Sys.Date() - years(1)),
#   phsmethods::extract_fin_year(Sys.Date())
# )
max_fy <- "2023/24" # TODO Change this to be dynamic and move to general!

########################## SECTION 2: Lookups & Populations ###############################

## 1. Lookups ----

localities <- read_in_localities()
context <- get_locality_context(localities, LOCALITY)
HSCP <- context$HSCP
HB <- context$HB
other_locs <- context$other_locs
n_loc <- context$n_loc


## 2. Populations (for rates) ----

populations <- read_in_dz_pops()

populations_proxy_year <- read_in_dz_pops_proxy_year()

populations <- rbind(populations, populations_proxy_year)


# compute age bands
populations$"Pop0_17" <- rowSums(subset(populations, select = age0:age17))
populations$"Pop18_44" <- rowSums(subset(populations, select = age18:age44))
populations$"Pop45_64" <- rowSums(subset(populations, select = age45:age64))
populations$"Pop65_74" <- rowSums(subset(populations, select = age65:age74))
populations$"Pop75Plus" <- rowSums(subset(
  populations,
  select = age75:age90plus
))
populations$"Pop65Plus" <- rowSums(subset(
  populations,
  select = age65:age90plus
))

pops <- populations %>%
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


# Aggregate and add partnership + HB + Scotland totals

pop_areas <- pops %>%
  filter(hscp_locality == LOCALITY) %>%
  select(-hb2019name, -hscp2019name) %>%
  rename(location = hscp_locality) %>%
  # Add a partnership total
  bind_rows(
    pops %>%
      select(-hscp_locality, -hb2019name) %>%
      filter(hscp2019name == HSCP) %>%
      group_by(financial_year, year, hscp2019name) %>%
      summarise(across(everything(), sum)) %>%
      ungroup() %>%
      rename(location = hscp2019name)
  ) %>%
  # Add HB total
  bind_rows(
    pops %>%
      select(-hscp_locality, -hscp2019name) %>%
      filter(hb2019name == HB) %>%
      group_by(financial_year, year, hb2019name) %>%
      summarise(across(everything(), sum)) %>%
      ungroup() %>%
      rename(location = hb2019name)
  ) %>%
  # Add a Scotland total
  bind_rows(
    pops %>%
      select(-hscp_locality, -hscp2019name, -hb2019name) %>%
      group_by(financial_year, year) %>%
      summarise(across(everything(), sum)) %>%
      ungroup() %>%
      mutate(location = "Scotland")
  ) %>%
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

# populations for age group charts
loc_pop_age1 <- loc_pop %>%
  filter(
    hscp_locality == LOCALITY,
    age_group %in% c("0 - 17", "18 - 44", "45 - 64", "65 - 74", "75+")
  )

# pop for MH emergency admissions age group chart
loc_pop_age2 <- loc_pop %>%
  filter(
    hscp_locality == LOCALITY,
    age_group %in% c("0 - 17", "18 - 44", "45 - 64", "65+")
  )

# populations by area - all ages
pop_areas_all_ages <- pop_areas %>%
  filter(age_group == "Total")

# populations by area - 65+
pop_areas_65plus <- pop_areas %>%
  filter(age_group == "65+")

# populations for other localities in the HSCP (for summary table only) - all ages
pops_other_locs <- inner_join(
  loc_pop,
  other_locs,
  by = join_by(hscp2019name, hscp_locality)
) %>%
  filter(
    age_group == "Total",
    year == max(year)
  ) %>%
  select(financial_year, year, hscp_locality, pop)

# populations for other localities in the HSCP (for summary table only) - 65+
pops_other_locs_65plus <- inner_join(
  loc_pop,
  other_locs,
  by = join_by(
    hscp2019name,
    hscp_locality
  )
) %>%
  filter(
    age_group == "65+",
    year == max(year)
  ) %>%
  select(financial_year, year, hscp_locality, pop)


########################## SECTION 3: Functions ###############################

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

####################### SECTION 4: Data manipulation & outputs #########################

source("Unscheduled Care/2a. Emergency Admissions.R")
source("Unscheduled Care/2b. AE Attendances.R")
source("Unscheduled Care/2c. Delayed Discharges.R")
source("Unscheduled Care/2d. Other Unscheduled Care.R")

# End of script - individual sections moved to separate files.
