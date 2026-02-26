# 1. Emergency Admissions ----
# _________________________________________________________________________

emergency_adm <- read_parquet(paste0(
  import_folder,
  "emergency_admissions_msg.parquet"
)) %>%
  filter(financial_year <= max_fy)

# Plotting by age
emergency_adm_age <- emergency_adm %>%
  filter(hscp_locality == LOCALITY) %>%
  drop_na(age_group) %>%
  group_by(financial_year, age_group) %>%
  summarise(adm = sum(admissions)) %>%
  ungroup() %>%
  left_join(
    loc_pop_age1,
    by = join_by(financial_year, age_group)
  ) %>%
  mutate(data = round_half_up(adm / pop * 100000)) %>%
  drop_na(year)


EAs_age_ts <- age_group_trend_usc(
  data_for_plot = emergency_adm_age,
  plot_title = paste(
    "Emergency admissions per 100,000 over time by age group\n for",
    LOCALITY
  ),
  yaxis_title = "Emergency admission rate\n per 100,000 population",
  source = "Source: PHS SMR01"
)

# Plotting by area
emergency_adm_areas <- emergency_adm %>%
  rename(n = admissions) %>%
  aggregate_usc_area_data() %>%
  left_join(
    pop_areas_all_ages,
    by = join_by(financial_year, location)
  ) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  drop_na(year)

EAs_loc_ts <- area_trend_usc(
  data_for_plot = emergency_adm_areas,
  plot_title = "Emergency admissions per 100,000 over time by residence",
  yaxis_title = "Emergency admission rate\n per 100,000 population",
  source = "Source: PHS SMR01"
)


# Objects for text for Figures and Summary (Emergency Admissions)
min_year_ea <- min(emergency_adm_areas$financial_year)
max_year_ea <- max(emergency_adm_areas$financial_year)

first_fy_rate <- filter(
  emergency_adm_areas,
  financial_year == min(financial_year),
  location == LOCALITY,
  area_type == "Locality"
)$data

latest_emergency_adm_loc <- emergency_adm_areas %>%
  filter(
    location == LOCALITY,
    year == max(year, na.rm = TRUE)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_emergency_adm_loc1 <- latest_emergency_adm_loc %>% pull(formatted_data)
latest_emergency_adm_loc2 <- latest_emergency_adm_loc %>% pull(data)

percent_rate_change <- percent_change_calc(
  latest_emergency_adm_loc2,
  first_fy_rate
)
word_change_rate <- word_change_calc(latest_emergency_adm_loc2, first_fy_rate)

# HSCP
hscp_emergency_adm <- emergency_adm_areas %>%
  filter(
    location == HSCP,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hscp_emergency_adm1 <- hscp_emergency_adm %>% pull(formatted_data)
hscp_emergency_adm2 <- hscp_emergency_adm %>% pull(data)

first_fy_hscp <- filter(
  emergency_adm_areas,
  financial_year == min(financial_year),
  area_type == "HSCP"
)$data

hscp_rate_change <- percent_change_calc(hscp_emergency_adm2, first_fy_hscp)
word_change_hscp <- word_change_calc(hscp_emergency_adm2, first_fy_hscp)

# Scotland
scot_emergency_adm <- emergency_adm_areas %>%
  filter(
    location == "Scotland",
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

scot_emergency_adm1 <- scot_emergency_adm %>% pull(formatted_data)
scot_emergency_adm2 <- scot_emergency_adm %>% pull(data)

first_fy_scot <- filter(
  emergency_adm_areas,
  financial_year == min(financial_year),
  location == "Scotland"
)$data

scot_rate_change <- percent_change_calc(scot_emergency_adm2, first_fy_scot)
word_change_scot <- word_change_calc(scot_emergency_adm2, first_fy_scot)

# NHS health board
hb_emergency_adm <- emergency_adm_areas %>%
  filter(
    location == HB,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hb_emergency_adm1 <- hb_emergency_adm %>% pull(formatted_data)
hb_emergency_adm2 <- hb_emergency_adm %>% pull(data)

first_fy_hb <- filter(
  emergency_adm_areas,
  financial_year == min(financial_year),
  location == HB
)$data

hb_rate_change <- percent_change_calc(hb_emergency_adm2, first_fy_hb)
word_change_hb <- word_change_calc(hb_emergency_adm2, first_fy_hb)

# other locations
other_loc_emergency_adm <- emergency_adm %>%
  group_by(financial_year, hscp_locality) %>%
  summarise(adm = sum(admissions)) %>%
  ungroup() %>%
  right_join(
    pops_other_locs,
    by = join_by(financial_year, hscp_locality)
  ) %>%
  mutate(adm = replace_na(adm, 0)) %>%
  mutate(data = round_half_up(adm / pop * 100000)) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  select(hscp_locality, data) %>%
  pivot_wider(names_from = hscp_locality, values_from = data)

# Create objects for text emergency admissions by age group
max_ea_age <- max(emergency_adm_age$financial_year)
min_ea_age <- min(emergency_adm_age$financial_year)

latest_ea_max_age <- emergency_adm_age %>%
  filter(
    year == max(year)
  ) %>%
  filter(
    data == max(data)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_ea_max_age1 <- latest_ea_max_age %>% pull(formatted_data)
latest_ea_max_age2 <- latest_ea_max_age %>% pull(data)
age_group_max_ea <- latest_ea_max_age %>% pull(age_group)

first_ea_max_age <- emergency_adm_age %>%
  filter(
    year == min(year),
    age_group == age_group_max_ea
  ) %>%
  pull(data)

max_rate_change_ea <- percent_change_calc(latest_ea_max_age2, first_ea_max_age)
max_word_change_ea <- word_change_calc(latest_ea_max_age2, first_ea_max_age)

latest_ea_min_age <- emergency_adm_age %>%
  filter(
    year == max(year)
  ) %>%
  filter(
    data == min(data)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_ea_min_age1 <- latest_ea_min_age %>% pull(formatted_data)
latest_ea_min_age2 <- latest_ea_min_age %>% pull(data)
age_group_min_ea <- latest_ea_min_age %>% pull(age_group)

first_ea_min_age <- emergency_adm_age %>%
  filter(
    age_group == age_group_min_ea
  ) %>%
  filter(year == min(year))

first_ea_min_age1 <- first_ea_min_age %>% pull(data)
min_year_ea_age1 <- first_ea_min_age %>% pull(year)


min_rate_change_ea <- percent_change_calc(latest_ea_min_age2, first_ea_min_age1)
min_word_change_ea <- word_change_calc(latest_ea_min_age2, first_ea_min_age1)

# 2a. Unscheduled bed days ----
# _________________________________________________________________________

bed_days <- read_parquet(paste0(import_folder, "bed_days_msg.parquet")) %>%
  filter(financial_year <= max_fy)

# Plotting by age
bed_days_age <- bed_days %>%
  filter(hscp_locality == LOCALITY) %>%
  drop_na(age_group) %>%
  group_by(financial_year, age_group) %>%
  summarise(bed_days = sum(bed_days)) %>%
  ungroup() %>%
  left_join(loc_pop_age1, by = join_by(financial_year, age_group)) %>%
  mutate(data = round_half_up(bed_days / pop * 100000)) %>%
  drop_na(year)


BDs_age_ts <- age_group_trend_usc(
  data_for_plot = bed_days_age,
  plot_title = paste(
    "Unscheduled bed days per 100,000 over time by age group\n for",
    LOCALITY
  ),
  yaxis_title = "Unscheduled bed day rate\n per 100,000 population",
  source = "Source: PHS SMR01"
)


# Plotting by area
bed_days_areas <- bed_days %>%
  rename(n = bed_days) %>%
  aggregate_usc_area_data() %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  drop_na(year)

BDs_loc_ts <- area_trend_usc(
  data_for_plot = bed_days_areas,
  plot_title = "Unscheduled bed days per 100,000 over time by residence",
  yaxis_title = "Unscheduled bed day rate\n per 100,000 population",
  source = "Source: PHS SMR01"
)

# Objects for text for Figures and Summary (Unscheduled Bed Days)
min_year_ubd <- min(bed_days_areas$financial_year)
max_year_ubd <- max(bed_days_areas$financial_year)

# LOCALITY
first_fy_rate_ubd <- filter(
  bed_days_areas,
  financial_year == min(financial_year),
  location == LOCALITY & area_type == "Locality"
)$data

latest_bed_days_loc <- bed_days_areas %>%
  filter(location == LOCALITY, year == max(year)) %>%
  mutate(formatted_data = format(data, big.mark = ","))
latest_bed_days_loc1 <- latest_bed_days_loc %>% pull(formatted_data)
latest_bed_days_loc2 <- latest_bed_days_loc %>% pull(data)

rate_change_ubd <- percent_change_calc(latest_bed_days_loc2, first_fy_rate_ubd)
word_change_ubd <- word_change_calc(latest_bed_days_loc2, first_fy_rate_ubd)
# HSCP
first_fy_hscp_ubd <- filter(
  bed_days_areas,
  (financial_year == min(bed_days_areas$financial_year)) & (area_type == "HSCP")
)$data

hscp_bed_days <- bed_days_areas %>%
  filter(location == HSCP, year == max(year)) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hscp_bed_days1 <- hscp_bed_days %>% pull(formatted_data)
hscp_bed_days2 <- hscp_bed_days %>% pull(data)

hscp_rate_ubd <- percent_change_calc(hscp_bed_days2, first_fy_hscp_ubd)
hscp_change_ubd <- word_change_calc(hscp_bed_days2, first_fy_hscp_ubd)

# Scotland
first_fy_scot_ubd <- filter(
  bed_days_areas,
  (financial_year == min(bed_days_areas$financial_year)) &
    (area_type == "Scotland")
)$data

scot_bed_days <- bed_days_areas %>%
  filter(location == "Scotland", year == max(year)) %>%
  mutate(formatted_data = format(data, big.mark = ","))

scot_bed_days1 <- scot_bed_days %>% pull(formatted_data)
scot_bed_days2 <- scot_bed_days %>% pull(data)

scot_rate_ubd <- percent_change_calc(scot_bed_days2, first_fy_scot_ubd)
scot_change_ubd <- word_change_calc(scot_bed_days2, first_fy_scot_ubd)

# NHS health board
hb_bed_days <- bed_days_areas %>%
  filter(
    location == HB,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hb_bed_days1 <- hb_bed_days %>% pull(formatted_data)
hb_bed_days2 <- hb_bed_days %>% pull(data)
first_fy_hb_ubd <- filter(
  bed_days_areas,
  financial_year == min(financial_year),
  location == HB
)$data

hb_rate_change_ubd <- percent_change_calc(hb_bed_days2, first_fy_hb_ubd)
word_change_hb_ubd <- word_change_calc(hb_bed_days2, first_fy_hb_ubd)


other_loc_bed_days <- bed_days %>%
  group_by(financial_year, hscp_locality) %>%
  summarise(bed_days = sum(bed_days)) %>%
  ungroup() %>%
  right_join(pops_other_locs, by = join_by(financial_year, hscp_locality)) %>%
  mutate(adm = replace_na(bed_days, 0)) %>%
  mutate(data = round_half_up(bed_days / pop * 100000)) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  select(hscp_locality, data) %>%
  pivot_wider(names_from = hscp_locality, values_from = data)

# Create objects for text emergency admissions by age group
max_ubd_age <- max(bed_days_age$financial_year)
min_ubd_age <- min(bed_days_age$financial_year)

latest_ubd_max_age <- bed_days_age %>%
  filter(
    year == max(year)
  ) %>%
  filter(
    data == max(data)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_ubd_max_age1 <- latest_ubd_max_age %>% pull(formatted_data)
latest_ubd_max_age2 <- latest_ubd_max_age %>% pull(data)
age_group_max_ubd <- latest_ubd_max_age %>% pull(age_group)

first_ubd_max_age <- bed_days_age %>%
  filter(
    year == min(year),
    age_group == age_group_max_ubd
  ) %>%
  pull(data)

max_rate_change_ubd <- percent_change_calc(
  latest_ubd_max_age2,
  first_ubd_max_age
)
max_word_change_ubd <- word_change_calc(latest_ubd_max_age2, first_ubd_max_age)

latest_ubd_min_age <- bed_days_age %>%
  filter(
    year == max(year)
  ) %>%
  filter(
    data == min(data)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_ubd_min_age1 <- latest_ubd_min_age %>% pull(formatted_data)
latest_ubd_min_age2 <- latest_ubd_min_age %>% pull(data)
age_group_min_ubd <- latest_ubd_min_age %>% pull(age_group)

first_ubd_min_age <- bed_days_age %>%
  filter(
    age_group == age_group_min_ubd
  ) %>%
  filter(year == min(year))

first_ubd_min_age1 <- first_ubd_min_age %>% pull(data)
min_year_ubd_age1 <- first_ubd_min_age %>% pull(year)


min_rate_change_ubd <- percent_change_calc(
  latest_ubd_min_age2,
  first_ubd_min_age1
)
min_word_change_ubd <- word_change_calc(latest_ubd_min_age2, first_ubd_min_age1)

# 2b. Unscheduled bed days - Mental Health ----
# _________________________________________________________________________

bed_days_mh <- read_parquet(paste0(
  import_folder,
  "bed_days_mh_msg.parquet"
)) %>%
  filter(financial_year <= max_fy)

# Plotting by age
bed_days_mh_age <- bed_days_mh %>%
  filter(hscp_locality == LOCALITY) %>%
  drop_na(age_group) %>%
  group_by(financial_year, age_group) %>%
  summarise(bed_days = sum(bed_days)) %>%
  ungroup() %>%
  left_join(loc_pop_age1, by = join_by(financial_year, age_group)) %>%
  mutate(data = round_half_up(bed_days / pop * 100000)) %>%
  drop_na(year)


BDMH_age_ts <- age_group_trend_usc(
  data_for_plot = bed_days_mh_age,
  plot_title = paste(
    "Unscheduled bed days (MH) per 100,000 over time by age group\n for",
    LOCALITY
  ),
  yaxis_title = "Unscheduled bed day (MH) rate\n per 100,000 population",
  source = "Source: PHS SMR04"
)


# Plotting by area
bed_days_mh_areas <- bed_days_mh %>%
  rename(n = bed_days) %>%
  aggregate_usc_area_data() %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  drop_na(year)

BDMH_loc_ts <- area_trend_usc(
  data_for_plot = bed_days_mh_areas,
  plot_title = "Unscheduled bed days (MH) per 100,000 over time by residence",
  yaxis_title = "Unscheduled bed day (MH) rate\n per 100,000 population",
  source = "Source: PHS SMR04"
)


# Objects for text and summary table- age
max_year_bd_mh_age <- max(bed_days_mh_age$financial_year)
min_year_bd_mh_age <- min(bed_days_mh_age$financial_year)

latest_bd_mh_max_age <- bed_days_mh_age %>%
  filter(
    year == max(year)
  ) %>%
  filter(
    data == max(data)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_bd_mh_max_age1 <- latest_bd_mh_max_age %>% pull(formatted_data)
latest_bd_mh_max_age2 <- latest_bd_mh_max_age %>% pull(data)
age_group_max_mh <- latest_bd_mh_max_age %>% pull(age_group)

first_bd_mh_max_age <- bed_days_mh_age %>%
  filter(
    year == min(year),
    age_group == age_group_max_mh
  ) %>%
  pull(data)

max_rate_change_beds_mh <- percent_change_calc(
  latest_bd_mh_max_age2,
  first_bd_mh_max_age
)
max_word_change_beds_mh <- word_change_calc(
  latest_bd_mh_max_age2,
  first_bd_mh_max_age
)

latest_bd_mh_min_age <- bed_days_mh_age %>%
  filter(
    year == max(year)
  ) %>%
  filter(
    data == min(data)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_bd_mh_min_age1 <- latest_bd_mh_min_age %>% pull(formatted_data)
latest_bd_mh_min_age2 <- latest_bd_mh_min_age %>% pull(data)
age_group_min_mh <- latest_bd_mh_min_age %>% pull(age_group)

first_bd_mh_min_age <- bed_days_mh_age %>%
  filter(
    age_group == age_group_min_mh
  ) %>%
  filter(year == min(year))

first_bd_mh_min_age1 <- first_bd_mh_min_age %>% pull(data)
min_year_bd_mh_age1 <- first_bd_mh_min_age %>% pull(year)


min_rate_change_beds_mh <- percent_change_calc(
  latest_bd_mh_min_age2,
  first_bd_mh_min_age1
)
min_word_change_beds_mh <- word_change_calc(
  latest_bd_mh_min_age2,
  first_bd_mh_min_age1
)

# Objects for text and summary table- area
max_year_bd_mh_areas <- max(bed_days_mh_areas$financial_year)
min_year_bd_mh_areas <- min(bed_days_mh_areas$financial_year)

latest_bed_days_mh_loc <- bed_days_mh_areas %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_bed_days_mh_loc1 <- latest_bed_days_mh_loc %>% pull(formatted_data)
latest_bed_days_mh_loc2 <- latest_bed_days_mh_loc %>% pull(data)
latest_bed_days_mh_loc1 <- ifelse(
  is_empty(latest_bed_days_mh_loc1),
  "NA",
  latest_bed_days_mh_loc1
)

first_bed_days_mh_loc <- bed_days_mh_areas %>%
  filter(
    location == LOCALITY,
    year == min(year)
  ) %>%
  pull(data)

loc_rate_change_beds_mh <- percent_change_calc(
  latest_bed_days_mh_loc2,
  first_bed_days_mh_loc
)
loc_word_change_beds_mh <- word_change_calc(
  latest_bed_days_mh_loc2,
  first_bed_days_mh_loc
)

hscp_bed_days_mh <- bed_days_mh_areas %>%
  filter(
    location == HSCP,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hscp_bed_days_mh1 <- hscp_bed_days_mh %>% pull(formatted_data)
hscp_bed_days_mh2 <- hscp_bed_days_mh %>% pull(data)

first_hscp_bed_days_mh <- bed_days_mh_areas %>%
  filter(
    location == HSCP,
    year == min(year)
  ) %>%
  pull(data)

hscp_rate_change_beds_mh <- percent_change_calc(
  hscp_bed_days_mh2,
  first_hscp_bed_days_mh
)
hscp_word_change_beds_mh <- word_change_calc(
  hscp_bed_days_mh2,
  first_hscp_bed_days_mh
)

scot_bed_days_mh <- bed_days_mh_areas %>%
  filter(
    location == "Scotland",
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

scot_bed_days_mh1 <- scot_bed_days_mh %>% pull(formatted_data)
scot_bed_days_mh2 <- scot_bed_days_mh %>% pull(data)

first_scot_bed_days_mh <- bed_days_mh_areas %>%
  filter(
    location == "Scotland",
    year == min(year)
  ) %>%
  pull(data)

scot_rate_change_beds_mh <- percent_change_calc(
  scot_bed_days_mh2,
  first_scot_bed_days_mh
)
scot_word_change_beds_mh <- word_change_calc(
  scot_bed_days_mh2,
  first_scot_bed_days_mh
)

# NHS health board
hb_mh_beddays <- bed_days_mh_areas %>%
  filter(
    location == HB,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hb_mh_beddays1 <- hb_mh_beddays %>% pull(formatted_data)
hb_mh_beddays2 <- hb_mh_beddays %>% pull(data)

first_fy_hb_mh <- filter(
  bed_days_mh_areas,
  financial_year == min(financial_year),
  location == HB
)$data

hb_rate_change_mh <- round(
  abs(hb_mh_beddays2 - first_fy_hb_mh) / first_fy_hb_mh * 100,
  digits = 1
)
word_change_hb_mh <- word_change_calc(hb_mh_beddays2, first_fy_hb_mh)

other_loc_bed_days_mh <- bed_days_mh %>%
  group_by(financial_year, hscp_locality) %>%
  summarise(bed_days = sum(bed_days)) %>%
  ungroup() %>%
  right_join(pops_other_locs, by = join_by(financial_year, hscp_locality)) %>%
  mutate(adm = replace_na(bed_days, 0)) %>%
  mutate(data = round_half_up(bed_days / pop * 100000)) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  select(hscp_locality, data) %>%
  pivot_wider(names_from = hscp_locality, values_from = data)
