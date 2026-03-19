##################### LOCALITY PROFILES DEMOGRAPHICS: POPULATION LOADING ############

# Description: Global population data loading and aggregation for locality profiles.
#              This script is intended to be run once per session or HSCP.

## Locality/DZ lookup
lookup <- read_in_localities()

## Population data
pop_raw_data_base <- read_in_dz_pops()

## Population Projection Data
hscp_pop_proj <- read_in_pop_proj()

## Set year
pop_max_year <- max(pop_raw_data_base$year)
pop_min_year <- pop_max_year - 5

# compute age bands
pop_raw_data_base$"Pop0_4" <- rowSums(subset(
  pop_raw_data_base,
  select = age0:age4
))
pop_raw_data_base$"Pop5_17" <- rowSums(subset(
  pop_raw_data_base,
  select = age5:age17
))
pop_raw_data_base$"Pop18_44" <- rowSums(subset(
  pop_raw_data_base,
  select = age18:age44
))
pop_raw_data_base$"Pop45_64" <- rowSums(subset(
  pop_raw_data_base,
  select = age45:age64
))
pop_raw_data_base$"Pop65_74" <- rowSums(subset(
  pop_raw_data_base,
  select = age65:age74
))
pop_raw_data_base$"Pop75_84" <- rowSums(subset(
  pop_raw_data_base,
  select = age75:age84
))
pop_raw_data_base$"Pop85Plus" <- rowSums(subset(
  pop_raw_data_base,
  select = age85:age90plus
))
pop_raw_data_base$"Pop65Plus" <- rowSums(subset(
  pop_raw_data_base,
  select = age65:age90plus
))

pops_base <- select(
  pop_raw_data_base,
  year,
  sex,
  hscp2019name,
  hscp_locality,
  Pop0_4,
  Pop5_17,
  Pop18_44,
  Pop45_64,
  Pop65_74,
  Pop75_84,
  Pop85Plus,
  Pop65Plus,
  total_pop
)

# Aggregate and add partnership + Scotland totals
pops_base <- pops_base %>%
  group_by(year, sex, hscp2019name, hscp_locality) %>%
  summarise(across(everything(), sum)) %>%
  ungroup() %>%
  # Add a partnership total
  bind_rows(
    pops_base %>%
      select(-hscp_locality) %>%
      group_by(year, hscp2019name, sex) %>%
      summarise(across(everything(), sum)) %>%
      ungroup() %>%
      mutate(hscp_locality = "Partnership Total")
  ) %>%
  # Add a Scotland total
  bind_rows(
    pops_base %>%
      select(-hscp_locality, -hscp2019name) %>%
      group_by(year, sex) %>%
      summarise(across(everything(), sum)) %>%
      ungroup() %>%
      mutate(hscp_locality = "Scotland Total", hscp2019name = "Scotland")
  )

# Current locality populations data breakdown for projections
loc_pops_base <- pops_base %>%
  select(-Pop65Plus, -total_pop) %>%
  filter(year == pop_max_year) %>%
  filter(!(hscp_locality %in% c("Partnership Total", "Scotland Total"))) %>%
  reshape2::melt(
    id.vars = c("year", "sex", "hscp2019name", "hscp_locality")
  ) %>%
  rename(age_group = variable) %>%
  as_tibble() %>%
  select(-year)

# HSCP population projection data weights
hscp_pop_proj_weight_base <- hscp_pop_proj %>%
  mutate(
    age_group = case_when(
      age %in% 0:4 ~ "Pop0_4",
      age %in% 5:17 ~ "Pop5_17",
      age %in% 18:44 ~ "Pop18_44",
      age %in% 45:64 ~ "Pop45_64",
      age %in% 65:74 ~ "Pop65_74",
      age %in% 75:84 ~ "Pop75_84",
      age > 84 ~ "Pop85Plus"
    )
  ) %>%
  # projection until 2028
  filter(year %in% pop_max_year:2028) %>%
  # aggregate to age groups
  group_by(year, hscp2019, hscp2019name, sex, age_group) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  # change sex variable coding
  mutate(sex = ifelse(sex == 1, "M", "F")) %>%
  # calculate weights
  arrange(hscp2019, sex, age_group, year) %>%
  group_by(hscp2019, sex, age_group) %>%
  mutate(pop_change = if_else(year != pop_max_year, pop / first(pop), 1)) %>%
  ungroup()
