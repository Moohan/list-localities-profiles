############################################################################################# .
#                                                                                           #
#                   LOCALITY PROFILES LIFESTYLE & RISK FACTORS DATA LOADING                 #
#                                                                                           #
############################################################################################# .

## Code used to load and clean datasets for the Lifestyle & Risk factors
#  section of the locality profiles.

# Set year of data extracts for folder
ext_year <- 2025

### Import + clean datasets ----

## Drug-related hospital admissions
drug_hosp <- readRDS(paste0(
  lp_path,
  "Lifestyle & Risk Factors/Data ",
  ext_year,
  "/scotpho_data_extract_drug_hosp.RDS"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 18), fixed = TRUE))

check_missing_data_scotpho(drug_hosp)


# Alcohol-related hospital admissions
alcohol_hosp <- readRDS(paste0(
  lp_path,
  "Lifestyle & Risk Factors/Data ",
  ext_year,
  "/scotpho_data_extract_alcohol_hosp.RDS"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = substr(period, 1, 7))

check_missing_data_scotpho(alcohol_hosp)


## Alcohol-specific deaths
alcohol_deaths <- readRDS(paste0(
  lp_path,
  "Lifestyle & Risk Factors/Data ",
  ext_year,
  "/scotpho_data_extract_alcohol_deaths.RDS"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 12), fixed = TRUE))

check_missing_data_scotpho(alcohol_deaths)

## Bowel screening uptake
bowel_screening <- readRDS(paste0(
  lp_path,
  "Lifestyle & Risk Factors/Data ",
  ext_year,
  "/scotpho_data_extract_bowel_screening.RDS"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 12), fixed = TRUE))

check_missing_data_scotpho(bowel_screening)

# Function to get latest data from scotpho for other localities
other_locs_summary_table <- function(data, latest_year) {
  data %>%
    filter(year == latest_year) %>%
    filter(area_type == "Locality") %>%
    rename("hscp_locality" = "area_name") %>%
    right_join(other_locs, by = join_by(hscp_locality)) %>%
    arrange(hscp_locality) %>%
    select(hscp_locality, measure) %>%
    mutate(measure = round_half_up(measure, 1)) %>%
    pivot_wider(names_from = hscp_locality, values_from = measure)
}
