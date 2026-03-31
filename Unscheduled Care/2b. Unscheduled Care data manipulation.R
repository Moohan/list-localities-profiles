##################### LOCALITY PROFILES UNSCHEDULED CARE: DATA MANIPULATION ######################.

# This script handles HSCP-level data manipulation and population pre-processing.

# Defensive logic for HSCP and HB
if (!exists("HSCP") && exists("LOCALITY")) {
  localities_lkp <- read_in_localities()
  HSCP <- as.character(filter(localities_lkp, hscp_locality == LOCALITY)$hscp2019name)
  HB <- as.character(filter(localities_lkp, hscp_locality == LOCALITY)$hb2019name)
}

# Find number of locs per partnership
if (exists("HSCP")) {
  localities_lkp <- read_in_localities()
  n_loc <- count_localities(localities_lkp, HSCP)
}

## Populations ----

populations <- read_in_dz_pops()

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

# Pre-calculate ScotPHO clean data
psych_hosp_clean <- psych_hosp_raw %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 18), fixed = TRUE))
