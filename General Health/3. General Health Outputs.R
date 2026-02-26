############################################################################################# .
#                                                                                           #
#                       LOCALITY PROFILES GENERAL HEALTH OUTPUTS CODE                       #
#                                                                                           #
############################################################################################# .

## Code used to create infographics, charts, and figures for the General Health section of
#  the locality profiles.

############# 1) PACKAGES, DIRECTORY, LOOKUPS, DATA IMPORT + CLEANING #############

## load packages
library(cowplot)
library(png)
library(flextable)
library(officer)

# Determine locality (for testing only)
# LOCALITY <- "Eastwood"
# LOCALITY <- "Stirling City with the Eastern Villages Bridge of Allan and Dunblane"
# LOCALITY <- "Mid-Argyll, Kintyre and Islay"
# LOCALITY <- "City of Dunfermline"
# LOCALITY <- "Barra"

# Set year of data extracts for folder
ext_year <- 2024

# Source in functions code
# source("Master RMarkdown Document & Render Code/Global Script.R")

# Set file path
# lp_path <- path("/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles")

gen_health_data_dir <- path(lp_path, "General Health", glue("DATA {ext_year}"))

### Geographical lookups and objects ----

# Locality lookup
lookup <- read_in_localities()

# Determine HSCP and HB based on Locality
context <- get_locality_context(lookup, LOCALITY)
HSCP <- context$HSCP
HB <- context$HB
other_locs <- context$other_locs
n_loc <- context$n_loc

### Import + clean datasets ----

# Life expectancy

# Males
life_exp_male <- read_parquet(path(
  gen_health_data_dir,
  "scotpho_data_extract_life_exp_male.parquet"
)) %>%
  clean_scotpho_dat()
# Females
life_exp_fem <- read_parquet(path(
  gen_health_data_dir,
  "scotpho_data_extract_life_exp_fem.parquet"
)) %>%
  clean_scotpho_dat()

life_exp <- bind_rows(life_exp_male, life_exp_fem) %>%
  mutate(
    sex = case_match(
      indicator,
      "Life expectancy, males" ~ "Male",
      "Life expectancy, females" ~ "Female"
    )
  ) %>%
  mutate(
    period_short = str_replace(period, fixed(" to "), "-") |>
      str_sub(end = 9)
  )

rm(life_exp_fem, life_exp_male)

check_missing_data_scotpho(life_exp)

## Deaths aged 15-44
deaths_15_44 <- read_parquet(path(
  gen_health_data_dir,
  "scotpho_data_extract_deaths_15_44.parquet"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 12), fixed = TRUE))

check_missing_data_scotpho(deaths_15_44)

## Cancer registrations
cancer_reg <- read_parquet(path(
  gen_health_data_dir,
  "scotpho_data_extract_cancer_reg.parquet"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 12), fixed = TRUE))

check_missing_data_scotpho(cancer_reg)

## Early deaths cancer
early_deaths_cancer <- read_parquet(path(
  gen_health_data_dir,
  "scotpho_data_extract_early_deaths_cancer.parquet"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 12), fixed = TRUE))

check_missing_data_scotpho(early_deaths_cancer)


## Asthma hospitalisations
asthma_hosp <- read_parquet(path(
  gen_health_data_dir,
  "scotpho_data_extract_asthma_hosp.parquet"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 18), fixed = TRUE))

check_missing_data_scotpho(asthma_hosp)

## CHD hospitalisations
chd_hosp <- read_parquet(path(
  gen_health_data_dir,
  "scotpho_data_extract_chd_hosp.parquet"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 18), fixed = TRUE))

check_missing_data_scotpho(chd_hosp)

## COPD hospitalisations
copd_hosp <- read_parquet(path(
  gen_health_data_dir,
  "scotpho_data_extract_copd_hosp.parquet"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 18), fixed = TRUE))

check_missing_data_scotpho(copd_hosp)

## Anxiety/depression/psychosis prescriptions
adp_presc <- read_parquet(path(
  gen_health_data_dir,
  "scotpho_data_extract_adp_presc.parquet"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = substr(period, 1, 7))

check_missing_data_scotpho(adp_presc)


# Long-term conditions
ltc <- read_parquet(path(gen_health_data_dir, "LTC_from_SLF.parquet")) %>%
  rename(
    "Arthritis" = "arth",
    "Asthma" = "asthma",
    "Atrial fibrillation" = "atrialfib",
    "Cancer" = "cancer",
    "Cardiovascular disease" = "cvd",
    "Liver disease" = "liver",
    "COPD*" = "copd",
    "Dementia" = "dementia",
    "Diabetes" = "diabetes",
    "Epilepsy" = "epilepsy",
    "Coronary heart disease" = "chd",
    "Heart failure" = "hefailure",
    "Multiple sclerosis" = "ms",
    "Parkinsons" = "parkinsons",
    "Renal failure" = "refailure"
  ) %>%
  mutate(
    hscp_locality = gsub("&", "and", hscp_locality, fixed = TRUE),
    year = paste0("20", substr(year, 1, 2), "/", substr(year, 3, 4))
  )


############################### 2) SCOTPHO DATA ####################################

source("General Health/3a. Life Expectancy.R")
source("General Health/3b. Mortality and Cancer.R")
source("General Health/3c. Hospitalisations and Prescriptions.R")
source("General Health/3d. LTCs.R")

# End of script - individual sections moved to separate files.
