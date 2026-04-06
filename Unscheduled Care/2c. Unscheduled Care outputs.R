##################### LOCALITY PROFILES UNSCHEDULED CARE: LOCALITY OUTPUTS ######################

# Subset population aggregates for the current LOCALITY
pop_areas <- pop_hscp_aggregates %>%
  filter(location %in% c(LOCALITY, HSCP, HB, "Scotland"))

loc_pop_age1 <- loc_pop %>%
  filter(
    hscp_locality == LOCALITY,
    age_group %in% c("0 - 17", "18 - 44", "45 - 64", "65 - 74", "75+")
  )

loc_pop_age2 <- loc_pop %>%
  filter(
    hscp_locality == LOCALITY,
    age_group %in% c("0 - 17", "18 - 44", "45 - 64", "65+")
  )

pop_areas_all_ages <- pop_areas %>%
  filter(age_group == "Total")

pop_areas_65plus <- pop_areas %>%
  filter(age_group == "65+")

other_locs <- localities %>%
  select(hscp_locality, hscp2019name) %>%
  filter(hscp2019name == HSCP & hscp_locality != LOCALITY) %>%
  arrange(hscp_locality)

pops_other_locs <- loc_pop %>%
  inner_join(other_locs, by = join_by(hscp2019name, hscp_locality)) %>%
  filter(age_group == "Total", year == max(year)) %>%
  select(financial_year, year, hscp_locality, pop)

pops_other_locs_65plus <- loc_pop %>%
  inner_join(other_locs, by = join_by(hscp2019name, hscp_locality)) %>%
  filter(age_group == "65+", year == max(year)) %>%
  select(financial_year, year, hscp_locality, pop)

# 1. Emergency Admissions ----
# _________________________________________________________________________

emergency_adm_age <- emergency_adm_raw %>%
  filter(hscp_locality == LOCALITY) %>%
  drop_na(age_group) %>%
  group_by(financial_year, age_group) %>%
  summarise(adm = sum(admissions)) %>%
  ungroup() %>%
  left_join(loc_pop_age1, by = join_by(financial_year, age_group)) %>%
  mutate(data = round_half_up(adm / pop * 100000)) %>%
  drop_na(year)

EAs_age_ts <- age_group_trend_usc(
  data_for_plot = emergency_adm_age,
  plot_title = paste("Emergency admissions per 100,000 over time by age group\n for", LOCALITY),
  yaxis_title = "Emergency admission rate\n per 100,000 population",
  source = "Source: PHS SMR01"
)

emergency_adm_areas <- emergency_adm_hscp_aggregates %>%
  filter(location %in% c(LOCALITY, HSCP, HB, "Scotland")) %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  drop_na(year)

EAs_loc_ts <- area_trend_usc(
  data_for_plot = emergency_adm_areas,
  plot_title = "Emergency admissions per 100,000 over time by residence",
  yaxis_title = "Emergency admission rate\n per 100,000 population",
  source = "Source: PHS SMR01"
)

# Objects for text
min_year_ea <- min(emergency_adm_areas$financial_year)
max_year_ea <- max(emergency_adm_areas$financial_year)

latest_emergency_adm_loc <- emergency_adm_areas %>%
  filter(location == LOCALITY, year == max(year, na.rm = TRUE)) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_emergency_adm_loc1 <- latest_emergency_adm_loc %>% pull(formatted_data)
latest_emergency_adm_loc2 <- latest_emergency_adm_loc %>% pull(data)

first_fy_rate <- filter(emergency_adm_areas, financial_year == min_year_ea, location == LOCALITY)$data
percent_rate_change <- percent_change_calc(latest_emergency_adm_loc2, first_fy_rate)
word_change_rate <- word_change_calc(latest_emergency_adm_loc2, first_fy_rate)

# HSCP/Scot/HB
hscp_emergency_adm <- emergency_adm_areas %>%
  filter(location == HSCP, year == max(year)) %>%
  mutate(formatted_data = format(data, big.mark = ","))
hscp_emergency_adm1 <- hscp_emergency_adm %>% pull(formatted_data)
hscp_emergency_adm2 <- hscp_emergency_adm %>% pull(data)
first_fy_hscp <- filter(emergency_adm_areas, financial_year == min_year_ea, location == HSCP)$data
hscp_rate_change <- percent_change_calc(hscp_emergency_adm2, first_fy_hscp)
word_change_hscp <- word_change_calc(hscp_emergency_adm2, first_fy_hscp)

scot_emergency_adm <- emergency_adm_areas %>%
  filter(location == "Scotland", year == max(year)) %>%
  mutate(formatted_data = format(data, big.mark = ","))
scot_emergency_adm1 <- scot_emergency_adm %>% pull(formatted_data)
scot_emergency_adm2 <- scot_emergency_adm %>% pull(data)
first_fy_scot <- filter(emergency_adm_areas, financial_year == min_year_ea, location == "Scotland")$data
scot_rate_change <- percent_change_calc(scot_emergency_adm2, first_fy_scot)
word_change_scot <- word_change_calc(scot_emergency_adm2, first_fy_scot)

hb_emergency_adm <- emergency_adm_areas %>%
  filter(location == HB, year == max(year)) %>%
  mutate(formatted_data = format(data, big.mark = ","))
hb_emergency_adm1 <- hb_emergency_adm %>% pull(formatted_data)
hb_emergency_adm2 <- hb_emergency_adm %>% pull(data)
first_fy_hb <- filter(emergency_adm_areas, financial_year == min_year_ea, location == HB)$data
hb_rate_change <- percent_change_calc(hb_emergency_adm2, first_fy_hb)
word_change_hb <- word_change_calc(hb_emergency_adm2, first_fy_hb)

# other locations
other_loc_emergency_adm <- emergency_adm_hscp_aggregates %>%
  filter(area_type == "Locality", location %in% other_locs$hscp_locality) %>%
  left_join(pop_hscp_aggregates %>% filter(area_type == "Locality", age_group == "Total"), by = join_by(financial_year, location)) %>%
  filter(year == max(year)) %>%
  mutate(data = format(round_half_up(n / pop * 100000), big.mark = ",")) %>%
  select(location, data) %>%
  rename(hscp_locality = location) %>%
  pivot_wider(names_from = hscp_locality, values_from = data)

# Age group text objects
max_ea_age <- max(emergency_adm_age$financial_year)
min_ea_age <- min(emergency_adm_age$financial_year)

latest_ea_max_age <- emergency_adm_age %>% filter(year == max(year)) %>% filter(data == max(data)) %>% mutate(formatted_data = format(data, big.mark = ","))
latest_ea_max_age1 <- latest_ea_max_age %>% pull(formatted_data)
latest_ea_max_age2 <- latest_ea_max_age %>% pull(data)
age_group_max_ea <- latest_ea_max_age %>% pull(age_group)
first_ea_max_age <- emergency_adm_age %>% filter(year == min(year), age_group == age_group_max_ea) %>% pull(data)
max_rate_change_ea <- percent_change_calc(latest_ea_max_age2, first_ea_max_age)
max_word_change_ea <- word_change_calc(latest_ea_max_age2, first_ea_max_age)

latest_ea_min_age <- emergency_adm_age %>% filter(year == max(year)) %>% filter(data == min(data)) %>% mutate(formatted_data = format(data, big.mark = ","))
latest_ea_min_age1 <- latest_ea_min_age %>% pull(formatted_data)
latest_ea_min_age2 <- latest_ea_min_age %>% pull(data)
age_group_min_ea <- latest_ea_min_age %>% pull(age_group)
first_ea_min_age_dat <- emergency_adm_age %>% filter(age_group == age_group_min_ea, year == min(year))
first_ea_min_age1 <- first_ea_min_age_dat %>% pull(data)
min_year_ea_age1 <- first_ea_min_age_dat %>% pull(year)
min_rate_change_ea <- percent_change_calc(latest_ea_min_age2, first_ea_min_age1)
min_word_change_ea <- word_change_calc(latest_ea_min_age2, first_ea_min_age1)

# 2a. Unscheduled bed days ----
# _________________________________________________________________________

bed_days_age <- bed_days_raw %>%
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
  plot_title = paste("Unscheduled bed days per 100,000 over time by age group\n for", LOCALITY),
  yaxis_title = "Unscheduled bed day rate\n per 100,000 population",
  source = "Source: PHS SMR01"
)

bed_days_areas <- bed_days_hscp_aggregates %>%
  filter(location %in% c(LOCALITY, HSCP, HB, "Scotland")) %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  drop_na(year)

BDs_loc_ts <- area_trend_usc(
  data_for_plot = bed_days_areas,
  plot_title = "Unscheduled bed days per 100,000 over time by residence",
  yaxis_title = "Unscheduled bed day rate\n per 100,000 population",
  source = "Source: PHS SMR01"
)

# Text objects
min_year_ubd <- min(bed_days_areas$financial_year)
max_year_ubd <- max(bed_days_areas$financial_year)

first_fy_rate_ubd <- filter(bed_days_areas, financial_year == min_year_ubd, location == LOCALITY)$data
latest_bed_days_loc <- bed_days_areas %>% filter(location == LOCALITY, year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
latest_bed_days_loc1 <- latest_bed_days_loc %>% pull(formatted_data)
latest_bed_days_loc2 <- latest_bed_days_loc %>% pull(data)
rate_change_ubd <- percent_change_calc(latest_bed_days_loc2, first_fy_rate_ubd)
word_change_ubd <- word_change_calc(latest_bed_days_loc2, first_fy_rate_ubd)

hscp_bed_days <- bed_days_areas %>% filter(location == HSCP, year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
hscp_bed_days1 <- hscp_bed_days %>% pull(formatted_data)
hscp_bed_days2 <- hscp_bed_days %>% pull(data)
first_fy_hscp_ubd <- filter(bed_days_areas, financial_year == min_year_ubd, location == HSCP)$data
hscp_rate_ubd <- percent_change_calc(hscp_bed_days2, first_fy_hscp_ubd)
hscp_change_ubd <- word_change_calc(hscp_bed_days2, first_fy_hscp_ubd)

scot_bed_days <- bed_days_areas %>% filter(location == "Scotland", year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
scot_bed_days1 <- scot_bed_days %>% pull(formatted_data)
scot_bed_days2 <- scot_bed_days %>% pull(data)
first_fy_scot_ubd <- filter(bed_days_areas, financial_year == min_year_ubd, location == "Scotland")$data
scot_rate_ubd <- percent_change_calc(scot_bed_days2, first_fy_scot_ubd)
scot_change_ubd <- word_change_calc(scot_bed_days2, first_fy_scot_ubd)

hb_bed_days <- bed_days_areas %>% filter(location == HB, year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
hb_bed_days1 <- hb_bed_days %>% pull(formatted_data)
hb_bed_days2 <- hb_bed_days %>% pull(data)
first_fy_hb_ubd <- filter(bed_days_areas, financial_year == min_year_ubd, location == HB)$data
hb_rate_change_ubd <- percent_change_calc(hb_bed_days2, first_fy_hb_ubd)
word_change_hb_ubd <- word_change_calc(hb_bed_days2, first_fy_hb_ubd)

other_loc_bed_days <- bed_days_hscp_aggregates %>%
  filter(area_type == "Locality", location %in% other_locs$hscp_locality) %>%
  left_join(pop_hscp_aggregates %>% filter(area_type == "Locality", age_group == "Total"), by = join_by(financial_year, location)) %>%
  filter(year == max(year)) %>%
  mutate(data = format(round_half_up(n / pop * 100000), big.mark = ",")) %>%
  select(location, data) %>%
  rename(hscp_locality = location) %>%
  pivot_wider(names_from = hscp_locality, values_from = data)

# Age group text objects
max_ubd_age <- max(bed_days_age$financial_year)
min_ubd_age <- min(bed_days_age$financial_year)
latest_ubd_max_age <- bed_days_age %>% filter(year == max(year)) %>% filter(data == max(data)) %>% mutate(formatted_data = format(data, big.mark = ","))
latest_ubd_max_age1 <- latest_ubd_max_age %>% pull(formatted_data)
latest_ubd_max_age2 <- latest_ubd_max_age %>% pull(data)
age_group_max_ubd <- latest_ubd_max_age %>% pull(age_group)
first_ubd_max_age <- bed_days_age %>% filter(year == min(year), age_group == age_group_max_ubd) %>% pull(data)
max_rate_change_ubd <- percent_change_calc(latest_ubd_max_age2, first_ubd_max_age)
max_word_change_ubd <- word_change_calc(latest_ubd_max_age2, first_ubd_max_age)
latest_ubd_min_age <- bed_days_age %>% filter(year == max(year)) %>% filter(data == min(data)) %>% mutate(formatted_data = format(data, big.mark = ","))
latest_ubd_min_age1 <- latest_ubd_min_age %>% pull(formatted_data)
latest_ubd_min_age2 <- latest_ubd_min_age %>% pull(data)
age_group_min_ubd <- latest_ubd_min_age %>% pull(age_group)
first_ubd_min_age_dat <- bed_days_age %>% filter(age_group == age_group_min_ubd, year == min(year))
first_ubd_min_age1 <- first_ubd_min_age_dat %>% pull(data)
min_year_ubd_age1 <- first_ubd_min_age_dat %>% pull(year)
min_rate_change_ubd <- percent_change_calc(latest_ubd_min_age2, first_ubd_min_age1)
min_word_change_ubd <- word_change_calc(latest_ubd_min_age2, first_ubd_min_age1)

# 2b. Unscheduled bed days - Mental Health ----
# _________________________________________________________________________

bed_days_mh_age <- bed_days_mh_raw %>%
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
  plot_title = paste("Unscheduled bed days (MH) per 100,000 over time by age group\n for", LOCALITY),
  yaxis_title = "Unscheduled bed day (MH) rate\n per 100,000 population",
  source = "Source: PHS SMR04"
)

bed_days_mh_areas <- bed_days_mh_hscp_aggregates %>%
  filter(location %in% c(LOCALITY, HSCP, HB, "Scotland")) %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  drop_na(year)

BDMH_loc_ts <- area_trend_usc(
  data_for_plot = bed_days_mh_areas,
  plot_title = "Unscheduled bed days (MH) per 100,000 over time by residence",
  yaxis_title = "Unscheduled bed day (MH) rate\n per 100,000 population",
  source = "Source: PHS SMR04"
)

# Text objects
max_year_bd_mh_age <- max(bed_days_mh_age$financial_year)
min_year_bd_mh_age <- min(bed_days_mh_age$financial_year)
latest_bd_mh_max_age <- bed_days_mh_age %>% filter(year == max(year)) %>% filter(data == max(data)) %>% mutate(formatted_data = format(data, big.mark = ","))
latest_bd_mh_max_age1 <- latest_bd_mh_max_age %>% pull(formatted_data)
latest_bd_mh_max_age2 <- latest_bd_mh_max_age %>% pull(data)
age_group_max_mh <- latest_bd_mh_max_age %>% pull(age_group)
first_bd_mh_max_age <- bed_days_mh_age %>% filter(year == min(year), age_group == age_group_max_mh) %>% pull(data)
max_rate_change_beds_mh <- percent_change_calc(latest_bd_mh_max_age2, first_bd_mh_max_age)
max_word_change_beds_mh <- word_change_calc(latest_bd_mh_max_age2, first_bd_mh_max_age)
latest_bd_mh_min_age <- bed_days_mh_age %>% filter(year == max(year)) %>% filter(data == min(data)) %>% mutate(formatted_data = format(data, big.mark = ","))
latest_bd_mh_min_age1 <- latest_bd_mh_min_age %>% pull(formatted_data)
latest_bd_mh_min_age2 <- latest_bd_mh_min_age %>% pull(data)
age_group_min_mh <- latest_bd_mh_min_age %>% pull(age_group)
first_bd_mh_min_age_dat <- bed_days_mh_age %>% filter(age_group == age_group_min_mh, year == min(year))
first_bd_mh_min_age1 <- first_bd_mh_min_age_dat %>% pull(data)
min_year_bd_mh_age1 <- first_bd_mh_min_age_dat %>% pull(year)
min_rate_change_beds_mh <- percent_change_calc(latest_bd_mh_min_age2, first_bd_mh_min_age1)
min_word_change_beds_mh <- word_change_calc(latest_bd_mh_min_age2, first_bd_mh_min_age1)

max_year_bd_mh_areas <- max(bed_days_mh_areas$financial_year)
min_year_bd_mh_areas <- min(bed_days_mh_areas$financial_year)
latest_bed_days_mh_loc <- bed_days_mh_areas %>% filter(location == LOCALITY, year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
latest_bed_days_mh_loc1 <- latest_bed_days_mh_loc %>% pull(formatted_data)
latest_bed_days_mh_loc2 <- latest_bed_days_mh_loc %>% pull(data)
latest_bed_days_mh_loc1 <- ifelse(is_empty(latest_bed_days_mh_loc1), "NA", latest_bed_days_mh_loc1)
first_bed_days_mh_loc <- bed_days_mh_areas %>% filter(location == LOCALITY, year == min(year)) %>% pull(data)
loc_rate_change_beds_mh <- percent_change_calc(latest_bed_days_mh_loc2, first_bed_days_mh_loc)
loc_word_change_beds_mh <- word_change_calc(latest_bed_days_mh_loc2, first_bed_days_mh_loc)

hscp_bed_days_mh <- bed_days_mh_areas %>% filter(location == HSCP, year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
hscp_bed_days_mh1 <- hscp_bed_days_mh %>% pull(formatted_data)
hscp_bed_days_mh2 <- hscp_bed_days_mh %>% pull(data)
first_hscp_bed_days_mh <- bed_days_mh_areas %>% filter(location == HSCP, year == min(year)) %>% pull(data)
hscp_rate_change_beds_mh <- percent_change_calc(hscp_bed_days_mh2, first_hscp_bed_days_mh)
hscp_word_change_beds_mh <- word_change_calc(hscp_bed_days_mh2, first_hscp_bed_days_mh)

scot_bed_days_mh <- bed_days_mh_areas %>% filter(location == "Scotland", year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
scot_bed_days_mh1 <- scot_bed_days_mh %>% pull(formatted_data)
scot_bed_days_mh2 <- scot_bed_days_mh %>% pull(data)
first_scot_bed_days_mh <- bed_days_mh_areas %>% filter(location == "Scotland", year == min(year)) %>% pull(data)
scot_rate_change_beds_mh <- percent_change_calc(scot_bed_days_mh2, first_scot_bed_days_mh)
scot_word_change_beds_mh <- word_change_calc(scot_bed_days_mh2, first_scot_bed_days_mh)

hb_mh_beddays <- bed_days_mh_areas %>% filter(location == HB, year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
hb_mh_beddays1 <- hb_mh_beddays %>% pull(formatted_data)
hb_mh_beddays2 <- hb_mh_beddays %>% pull(data)
first_fy_hb_mh <- filter(bed_days_mh_areas, financial_year == min_year_bd_mh_areas, location == HB)$data
hb_rate_change_mh <- round(abs(hb_mh_beddays2 - first_fy_hb_mh) / first_fy_hb_mh * 100, digits = 1)
word_change_hb_mh <- word_change_calc(hb_mh_beddays2, first_fy_hb_mh)

other_loc_bed_days_mh <- bed_days_mh_hscp_aggregates %>%
  filter(area_type == "Locality", location %in% other_locs$hscp_locality) %>%
  left_join(pop_hscp_aggregates %>% filter(area_type == "Locality", age_group == "Total"), by = join_by(financial_year, location)) %>%
  filter(year == max(year)) %>%
  mutate(data = format(round_half_up(n / pop * 100000), big.mark = ",")) %>%
  select(location, data) %>%
  rename(hscp_locality = location) %>%
  pivot_wider(names_from = hscp_locality, values_from = data)

# 3. A&E Attendances ----
# _________________________________________________________________________

ae_att_age <- ae_attendances_raw %>%
  filter(hscp_locality == LOCALITY, age_group != "NA") %>%
  group_by(financial_year, age_group) %>%
  summarise(attendances = sum(attendances)) %>%
  ungroup() %>%
  left_join(loc_pop_age1, by = join_by(financial_year, age_group)) %>%
  mutate(data = round_half_up(attendances / pop * 100000)) %>%
  drop_na(year)

AandE_age_ts <- age_group_trend_usc(
  data_for_plot = ae_att_age,
  plot_title = paste("A&E attendances per 100,000 over time by age group\n for", LOCALITY),
  yaxis_title = "A&E attendance rate\n per 100,000 population",
  source = "Source: PHS A&E Datamart"
)

ae_att_areas <- ae_attendances_hscp_aggregates %>%
  filter(location %in% c(LOCALITY, HSCP, HB, "Scotland")) %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  drop_na(year)

AandE_loc_ts <- area_trend_usc(
  data_for_plot = ae_att_areas,
  plot_title = paste("A&E attendances per 100,000 over time by residence"),
  yaxis_title = "A&E attendance rate\n per 100,000 population",
  source = "Source: PHS A&E Datamart"
)

# Text objects
min_year_ae_age <- min(ae_att_age$financial_year)
max_year_ae_age <- max(ae_att_age$financial_year)
latest_ae_att_max_age <- ae_att_age %>% filter(year == max(year)) %>% filter(data == max(data)) %>% mutate(formatted_data = format(data, big.mark = ","))
latest_ae_att_loc1_age <- latest_ae_att_max_age %>% pull(formatted_data)
latest_ae_att_loc2_age <- latest_ae_att_max_age %>% pull(data)
age_group_max <- latest_ae_att_max_age %>% pull(age_group)
first_ae_att_max_age <- ae_att_age %>% filter(year == min(year), age_group == age_group_max) %>% mutate(formatted_data = format(data, big.mark = ","))
first_ae_att_max_age_data <- first_ae_att_max_age %>% pull(data)
percent_rate_change_ae_age <- percent_change_calc(latest_ae_att_loc2_age, first_ae_att_max_age_data)
word_change_rate_ae_age <- word_change_calc(latest_ae_att_loc2_age, first_ae_att_max_age_data)

latest_ae_att_min_age <- ae_att_age %>% filter(year == max(year)) %>% filter(data == min(data)) %>% mutate(formatted_data = format(data, big.mark = ","))
latest_ae_att_loc1_age_min <- latest_ae_att_min_age %>% pull(formatted_data)
latest_ae_att_loc2_age_min <- latest_ae_att_min_age %>% pull(data)
age_group_min <- latest_ae_att_min_age %>% pull(age_group)
first_ae_att_min_age <- ae_att_age %>% filter(year == min(year), age_group == age_group_min) %>% mutate(formatted_data = format(data, big.mark = ","))
first_ae_att_min_data <- first_ae_att_min_age %>% pull(data)
percent_rate_change_ae_age2 <- percent_change_calc(latest_ae_att_loc2_age_min, first_ae_att_min_data)
word_change_rate_ae_age2 <- word_change_calc(latest_ae_att_loc2_age_min, first_ae_att_min_data)

min_year_ae_area <- min(ae_att_areas$financial_year)
max_year_ae_area <- max(ae_att_areas$financial_year)
first_fy_rate_ae_areas <- filter(ae_att_areas, financial_year == min_year_ae_area, location == LOCALITY)$data
latest_ae_att_loc <- ae_att_areas %>% filter(location == LOCALITY, year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
latest_ae_att_loc1 <- latest_ae_att_loc %>% pull(formatted_data)
latest_ae_att_loc2 <- latest_ae_att_loc %>% pull(data)
percent_rate_change_ae_areas <- percent_change_calc(latest_ae_att_loc2, first_fy_rate_ae_areas)
word_change_rate_ae_areas <- word_change_calc(latest_ae_att_loc2, first_fy_rate_ae_areas)

hscp_ae_att <- ae_att_areas %>% filter(location == HSCP, year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
hscp_ae_att1 <- hscp_ae_att %>% pull(formatted_data)
hscp_ae_att2 <- hscp_ae_att %>% pull(data)
first_fy_hscp_ae <- filter(ae_att_areas, financial_year == min_year_ae_area, location == HSCP)$data
percent_rate_change_ae_areas_hscp <- percent_change_calc(hscp_ae_att2, first_fy_hscp_ae)
word_change_rate_ae_areas_hscp <- word_change_calc(hscp_ae_att2, first_fy_hscp_ae)

scot_ae_att <- ae_att_areas %>% filter(location == "Scotland", year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
scot_ae_att1 <- scot_ae_att %>% pull(formatted_data)
scot_ae_att2 <- scot_ae_att %>% pull(data)
first_fy_scot_ae <- filter(ae_att_areas, financial_year == min_year_ae_area, location == "Scotland")$data
percent_rate_change_ae_areas_scot <- percent_change_calc(scot_ae_att2, first_fy_scot_ae)
word_change_rate_ae_areas_scot <- word_change_calc(scot_ae_att2, first_fy_scot_ae)

hb_ae_att_loc <- ae_att_areas %>% filter(location == HB, year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
hb_ae1 <- hb_ae_att_loc %>% pull(formatted_data)
hb_ae2 <- hb_ae_att_loc %>% pull(data)
first_fy_hb_ae <- filter(ae_att_areas, financial_year == min_year_ae_area, location == HB)$data
hb_rate_change_ae <- percent_change_calc(hb_ae2, first_fy_hb_ae)
word_change_hb_ae <- word_change_calc(hb_ae2, first_fy_hb_ae)

other_loc_ae_att <- ae_attendances_hscp_aggregates %>%
  filter(area_type == "Locality", location %in% other_locs$hscp_locality) %>%
  left_join(pop_hscp_aggregates %>% filter(area_type == "Locality", age_group == "Total"), by = join_by(financial_year, location)) %>%
  filter(year == max(year)) %>%
  mutate(data = format(round_half_up(n / pop * 100000), big.mark = ",")) %>%
  select(location, data) %>%
  rename(hscp_locality = location) %>%
  pivot_wider(names_from = hscp_locality, values_from = data)

# 4. Delayed Discharges ----
# _________________________________________________________________________

delayed_disch <- delayed_disch_raw %>%
  filter(hscp2019name == HSCP, hscp_locality == LOCALITY) %>%
  group_by(financial_year, hscp2019name, hscp_locality) %>%
  summarise(dd_people = sum(dd_people), dd_bed_days = sum(dd_bed_days)) %>%
  ungroup()

delayed_disch_areas <- delayed_disch_hscp_aggregates %>%
  filter(location %in% c(LOCALITY, HSCP, HB, "Scotland")) %>%
  left_join(pop_areas_65plus, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  drop_na(year)

DD_loc_ts <- area_trend_usc(
  data_for_plot = delayed_disch_areas,
  plot_title = paste0("Delayed discharge bed days per 100,000 population aged over 65\n", "over time by residence"),
  yaxis_title = "Delayed discharge bed day rate\n per 100,000 population aged 65+",
  source = "Source: PHS Delayed Discharges"
)

# Text objects
min_year_dd <- min(delayed_disch_areas$financial_year)
max_year_dd <- max(delayed_disch_areas$financial_year)
latest_dd_loc <- delayed_disch_areas %>% filter(location == LOCALITY, year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
latest_dd_loc1 <- latest_dd_loc %>% pull(formatted_data)
latest_dd_loc2 <- latest_dd_loc %>% pull(data)
first_dd_loc <- delayed_disch_areas %>% filter(location == LOCALITY, year == min(year)) %>% pull(data)
percent_rate_change_dd_loc <- percent_change_calc(latest_dd_loc2, first_dd_loc)
word_change_rate_dd_loc <- word_change_calc(latest_dd_loc2, first_dd_loc)

hscp_dd_loc <- delayed_disch_areas %>% filter(location == HSCP, year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
hscp_dd1 <- hscp_dd_loc %>% pull(formatted_data)
hscp_dd2 <- hscp_dd_loc %>% pull(data)
first_hscp_dd <- delayed_disch_areas %>% filter(location == HSCP, year == min(year)) %>% pull(data)
percent_rate_change_dd_hscp <- percent_change_calc(hscp_dd2, first_hscp_dd)
word_change_rate_dd_hscp <- word_change_calc(hscp_dd2, first_hscp_dd)

scot_dd_loc <- delayed_disch_areas %>% filter(location == "Scotland", year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
scot_dd1 <- scot_dd_loc %>% pull(formatted_data)
scot_dd2 <- scot_dd_loc %>% pull(data)
first_scot_dd <- delayed_disch_areas %>% filter(location == "Scotland", year == min(year)) %>% pull(data)
percent_rate_change_dd_scot <- percent_change_calc(scot_dd2, first_scot_dd)
word_change_rate_dd_scot <- word_change_calc(scot_dd2, first_scot_dd)

hb_dd_loc <- delayed_disch_areas %>% filter(location == HB, year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
hb_dd1 <- hb_dd_loc %>% pull(formatted_data)
hb_dd2 <- hb_dd_loc %>% pull(data)
first_fy_hb_dd <- filter(delayed_disch_areas, financial_year == min_year_dd, location == HB)$data
hb_rate_change_dd <- percent_change_calc(hb_dd2, first_fy_hb_dd)
word_change_hb_dd <- word_change_calc(hb_dd2, first_fy_hb_dd)

other_loc_dd <- delayed_disch_hscp_aggregates %>%
  filter(area_type == "Locality", location %in% other_locs$hscp_locality) %>%
  left_join(pop_hscp_aggregates %>% filter(area_type == "Locality", age_group == "65+"), by = join_by(financial_year, location)) %>%
  filter(year == max(year)) %>%
  mutate(data = format(round_half_up(n / pop * 100000), big.mark = ",")) %>%
  select(location, data) %>%
  rename(hscp_locality = location) %>%
  pivot_wider(names_from = hscp_locality, values_from = data)

# 5. Fall Admissions ----
# _________________________________________________________________________

falls_areas <- falls_hscp_aggregates %>%
  filter(location %in% c(LOCALITY, HSCP, HB, "Scotland")) %>%
  left_join(pop_areas_65plus, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  drop_na(year)

Falls_loc_ts <- area_trend_usc(
  data_for_plot = falls_areas,
  plot_title = paste0("Emergency admissions from falls per 100,000 population aged over 65\n", "over time by residence"),
  yaxis_title = "Emergency admissions from falls rate\nper 100,000 population aged 65+",
  source = "Source: PHS SMR01"
)

# Text objects
min_year_falls <- min(falls_areas$financial_year)
max_year_falls <- max(falls_areas$financial_year)
latest_falls_loc <- falls_areas %>% filter(location == LOCALITY, year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
latest_falls_loc1 <- latest_falls_loc %>% pull(formatted_data)
latest_falls_loc2 <- latest_falls_loc %>% pull(data)
first_falls_loc <- falls_areas %>% filter(location == LOCALITY, year == min(year)) %>% pull(data)
percent_rate_change_falls_loc <- percent_change_calc(latest_falls_loc2, first_falls_loc)
word_change_rate_falls_loc <- word_change_calc(latest_falls_loc2, first_falls_loc)

hscp_falls_loc <- falls_areas %>% filter(location == HSCP, year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
hscp_falls1 <- hscp_falls_loc %>% pull(formatted_data)
hscp_falls2 <- hscp_falls_loc %>% pull(data)
first_falls_hscp <- falls_areas %>% filter(location == HSCP, year == min(year)) %>% pull(data)
percent_rate_change_falls_hscp <- percent_change_calc(hscp_falls2, first_falls_hscp)
word_change_rate_falls_hscp <- word_change_calc(hscp_falls2, first_falls_hscp)

scot_falls_loc <- falls_areas %>% filter(location == "Scotland", year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
scot_falls1 <- scot_falls_loc %>% pull(formatted_data)
scot_falls2 <- scot_falls_loc %>% pull(data)
first_falls_scot <- falls_areas %>% filter(location == "Scotland", year == min(year)) %>% pull(data)
percent_rate_change_falls_scot <- percent_change_calc(scot_falls2, first_falls_scot)
word_change_rate_falls_scot <- word_change_calc(scot_falls2, first_falls_scot)

hb_falls_loc <- falls_areas %>% filter(location == HB, year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
hb_falls1 <- hb_falls_loc %>% pull(formatted_data)
hb_falls2 <- hb_falls_loc %>% pull(data)
first_fy_hb_falls <- filter(falls_areas, financial_year == min_year_falls, location == HB)$data
hb_rate_change_falls <- round(abs(hb_falls2 - first_fy_hb_falls) / first_fy_hb_falls * 100, digits = 1)
word_change_hb_falls <- word_change_calc(hb_falls2, first_fy_hb_falls)

# 6. Readmissions (28 days) ----
# _________________________________________________________________________

readmissions_age <- readmissions_raw %>%
  filter(hscp_locality == LOCALITY) %>%
  drop_na(age_group) %>%
  group_by(financial_year, age_group) %>%
  summarise(read_28 = sum(read_28), discharges = sum(discharges)) %>%
  ungroup() %>%
  mutate(data = round_half_up(read_28 / discharges * 1000, 1))

read_age_ts <- age_group_trend_usc(
  data_for_plot = readmissions_age,
  plot_title = paste("Readmission rate (28 days) per 1,000 discharges by age group\n for", LOCALITY),
  yaxis_title = "Readmission rate (28 days)\n per 1,000 discharges",
  source = "Source: PHS SMR01"
)

readmissions_areas <- readmissions_hscp_aggregates %>%
  filter(location %in% c(LOCALITY, HSCP, HB, "Scotland")) %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(read_28 / discharges * 1000, 1)) %>%
  drop_na(year)

read_loc_ts <- area_trend_usc(
  data_for_plot = readmissions_areas,
  plot_title = paste("Readmission rate (28 days) per 1,000 discharges over time by residence"),
  yaxis_title = "Readmission rate (28 days)\n per 1,000 discharges",
  source = "Source: PHS SMR01"
)

# Text objects
min_year_re_age <- min(readmissions_age$financial_year)
max_year_re_age <- max(readmissions_age$financial_year)
latest_re_max_age <- readmissions_age %>% filter(financial_year == max(financial_year)) %>% filter(data == max(data))
latest_re_max_age_data <- latest_re_max_age %>% pull(data)
latest_re_max_age_group <- latest_re_max_age %>% pull(age_group)
first_re_max_age <- readmissions_age %>% filter(financial_year == min(financial_year), age_group == latest_re_max_age_group) %>% pull(data)
percent_rate_change_re_age <- percent_change_calc(latest_re_max_age_data, first_re_max_age)
word_change_rate_re_age <- word_change_calc(latest_re_max_age_data, first_re_max_age)
latest_re_min_age <- readmissions_age %>% filter(financial_year == max(financial_year)) %>% filter(data == min(data))
latest_re_min_age_data <- latest_re_min_age %>% pull(data)
latest_re_min_age_group <- latest_re_min_age %>% pull(age_group)
first_re_min_age <- readmissions_age %>% filter(financial_year == min(financial_year), age_group == latest_re_min_age_group) %>% pull(data)
percent_rate_change_re_age_min <- percent_change_calc(latest_re_min_age_data, first_re_min_age)
word_change_rate_re_age_min <- word_change_calc(latest_re_min_age_data, first_re_min_age)

min_year_re_area <- min(readmissions_areas$financial_year)
max_year_re_area <- max(readmissions_areas$financial_year)
first_read_loc_dat <- readmissions_areas %>% filter(location == LOCALITY, year == min(year))
first_read_loc1 <- first_read_loc_dat %>% pull(data)
latest_read_loc_dat <- readmissions_areas %>% filter(location == LOCALITY, year == max(year))
latest_read_loc1 <- latest_read_loc_dat %>% pull(data)
percent_rate_change_re_area <- percent_change_calc(latest_read_loc1, first_read_loc1)
word_change_rate_re_area <- word_change_calc(latest_read_loc1, first_read_loc1)

first_hscp_read <- readmissions_areas %>% filter(location == HSCP, year == min(year)) %>% pull(data)
hscp_read <- readmissions_areas %>% filter(location == HSCP, year == max(year)) %>% pull(data)
percent_rate_change_re_area_hscp <- percent_change_calc(hscp_read, first_hscp_read)
word_change_rate_re_area_hscp <- word_change_calc(hscp_read, first_hscp_read)

first_scot_read <- readmissions_areas %>% filter(location == "Scotland", year == min(year)) %>% pull(data)
scot_read <- readmissions_areas %>% filter(location == "Scotland", year == max(year)) %>% pull(data)
percent_rate_change_re_area_scot <- percent_change_calc(scot_read, first_scot_read)
word_change_rate_re_area_scot <- word_change_calc(scot_read, first_scot_read)

hb_read_loc <- readmissions_areas %>% filter(location == HB, year == max(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
hb_read1 <- hb_read_loc %>% pull(formatted_data)
hb_read2 <- hb_read_loc %>% pull(data)
first_fy_hb_read <- filter(readmissions_areas, financial_year == min_year_re_area, location == HB)$data
hb_rate_change_read <- percent_change_calc(hb_read2, first_fy_hb_read)
word_change_hb_read <- word_change_calc(hb_read2, first_fy_hb_read)

# 8. Potentially Preventable Admissions ----
# _________________________________________________________________________

ppa_65plus <- ppa_65plus_hscp_aggregates %>%
  filter(location %in% c(LOCALITY, HSCP, HB, "Scotland")) %>%
  left_join(ppa_hscp_aggregates %>% select(financial_year, location, n), by = join_by(financial_year, location)) %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(plus65tot / n * 100, 1)) %>%
  drop_na(year)

latest_ppa_65plus <- ppa_65plus %>% filter(location == LOCALITY, year == max(year)) %>% pull(data)

ppa_under65 <- ppa_under65_hscp_aggregates %>%
  filter(location %in% c(LOCALITY, HSCP, HB, "Scotland")) %>%
  left_join(ppa_hscp_aggregates %>% select(financial_year, location, n), by = join_by(financial_year, location)) %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(under65tot / n * 100, 1)) %>%
  drop_na(year)

latest_ppa_under65 <- ppa_under65 %>% filter(location == LOCALITY, year == max(year)) %>% pull(data)

ppa_areas <- ppa_hscp_aggregates %>%
  filter(location %in% c(LOCALITY, HSCP, HB, "Scotland")) %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  mutate(location = factor(location, levels = c(LOCALITY, HSCP, HB, "Scotland"))) %>%
  arrange(location) %>%
  drop_na(year)

ppa_loc_ts <- area_trend_usc(
  data_for_plot = ppa_areas,
  plot_title = paste("Potentially Preventable Emergency Admissions per 100,000 by residence"),
  yaxis_title = "PPA rate\nper 100,000 population",
  source = "Source: PHS SMR01"
)

# Text objects
max_year_ppa_areas <- max(ppa_areas$financial_year)
min_year_ppa_areas <- min(ppa_areas$financial_year)
latest_ppa_loc_dat <- ppa_areas %>% filter(location == LOCALITY, year == max(year) | year == min(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
latest_ppa_loc1 <- latest_ppa_loc_dat$formatted_data[2]
ppa_diff <- percent_change_calc(latest_ppa_loc_dat$data[2], latest_ppa_loc_dat$data[1])
ppa_word_change <- word_change_calc(latest_ppa_loc_dat$data[2], latest_ppa_loc_dat$data[1])
latest_ppa_loc <- latest_ppa_loc_dat # for Rmd compatibility

hscp_ppa_loc <- ppa_areas %>% filter(location == HSCP, year == max(year) | year == min(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
ppa_diff_hscp <- percent_change_calc(hscp_ppa_loc$data[2], hscp_ppa_loc$data[1])
ppa_word_change_hscp <- word_change_calc(hscp_ppa_loc$data[2], hscp_ppa_loc$data[1])
hscp_ppa <- hscp_ppa_loc # for Rmd compatibility

scot_ppa_loc <- ppa_areas %>% filter(location == "Scotland", year == max(year) | year == min(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
diff_scot_ppa <- percent_change_calc(scot_ppa_loc$data[2], scot_ppa_loc$data[1])
word_change_scot_ppa <- word_change_calc(scot_ppa_loc$data[2], scot_ppa_loc$data[1])
scot_ppa <- scot_ppa_loc # for Rmd compatibility

hb_ppa_loc <- ppa_areas %>% filter(location == HB, year == max(year) | year == min(year)) %>% mutate(formatted_data = format(data, big.mark = ","))
diff_hb_ppa <- percent_change_calc(hb_ppa_loc$data[2], hb_ppa_loc$data[1])
word_change_hb_ppa <- word_change_calc(hb_ppa_loc$data[2], hb_ppa_loc$data[1])
hb_ppa <- hb_ppa_loc # for Rmd compatibility

other_loc_ppa <- ppa_hscp_aggregates %>%
  filter(area_type == "Locality", location %in% other_locs$hscp_locality) %>%
  left_join(pop_hscp_aggregates %>% filter(area_type == "Locality", age_group == "Total"), by = join_by(financial_year, location)) %>%
  filter(year == max(year)) %>%
  mutate(data = format(round_half_up(n / pop * 100000), big.mark = ",")) %>%
  select(location, data) %>%
  rename(hscp_locality = location) %>%
  pivot_wider(names_from = hscp_locality, values_from = data)

# 9. Psychiatric hospital admissions (ScotPHO) ----
# ___________________________________________________________________________

check_missing_data_scotpho(psych_hosp_raw)
latest_period_psych_hosp <- unique(filter(psych_hosp_raw, year == max(year))$period_short)

psych_hosp_time_trend <- psych_hosp_raw %>%
  scotpho_time_trend(
    data = .,
    chart_title = "Psychiatric Patient Hospitalisations Time Trend",
    xaxis_title = "Financial Year Groups (3-year aggregates)",
    yaxis_title = "Psychiatric patient hospitalisations\n(Standardised rates per 100,000)",
    string_wrap = 10,
    rotate_xaxis = TRUE
  )

# Text objects
psych_hosp_latest <- round_half_up(filter(psych_hosp_raw, year == max(year) & (area_name == LOCALITY & area_type == "Locality"))$measure, 1)
hscp_psych_hosp_val <- round_half_up(filter(psych_hosp_raw, year == max(year) & (area_name == HSCP & area_type == "HSCP"))$measure, 1)
scot_psych_hosp_val <- round_half_up(filter(psych_hosp_raw, year == max(year) & area_name == "Scotland")$measure, 1)

list_years <- unique(psych_hosp_time_trend$data[5])
list_years_latest <- list_years$period

loc_psych_hosp <- psych_hosp_raw %>%
  filter(period %in% list_years_latest, area_name == LOCALITY, area_type == "Locality", year == min(year) | year == max(year)) %>%
  mutate(measure2 = format(measure, big.mark = ","))
diff_loc_psych <- percent_change_calc(loc_psych_hosp$measure[2], loc_psych_hosp$measure[1])
word_change_loc_psych <- word_change_calc(loc_psych_hosp$measure[2], loc_psych_hosp$measure[1])

hscp_psych_hosp <- psych_hosp_raw %>%
  filter(period %in% list_years_latest, area_name == HSCP, area_type == "HSCP", year == min(year) | year == max(year)) %>%
  mutate(measure2 = format(measure, big.mark = ","))
diff_hscp_psych <- percent_change_calc(hscp_psych_hosp$measure[2], hscp_psych_hosp$measure[1])
word_change_hscp_psych <- word_change_calc(hscp_psych_hosp$measure[2], hscp_psych_hosp$measure[1])

hb_psych_hosp <- psych_hosp_raw %>%
  filter(period %in% list_years_latest, area_name == HB, area_type == "Health board", year == min(year) | year == max(year)) %>%
  mutate(measure2 = format(measure, big.mark = ","))
diff_hb_psych <- percent_change_calc(hb_psych_hosp$measure[2], hb_psych_hosp$measure[1])
word_change_hb_psych <- word_change_calc(hb_psych_hosp$measure[2], hb_psych_hosp$measure[1])

scot_psych_hosp <- psych_hosp_raw %>%
  filter(period %in% list_years_latest, area_name == "Scotland", area_type == "Scotland", year == min(year) | year == max(year)) %>%
  mutate(measure2 = format(measure, big.mark = ","))
diff_scot_psych <- percent_change_calc(scot_psych_hosp$measure[2], scot_psych_hosp$measure[1])
word_change_scot_psych <- word_change_calc(scot_psych_hosp$measure[2], scot_psych_hosp$measure[1])

other_locs_psych_hosp <- psych_hosp_raw %>%
  filter(year == max(year), area_type == "Locality", area_name != LOCALITY) %>%
  rename(hscp_locality = area_name) %>%
  inner_join(other_locs, by = join_by(hscp_locality)) %>%
  arrange(hscp_locality) %>%
  select(hscp_locality, measure) %>%
  mutate(measure = as.character(round_half_up(measure, 1))) %>%
  pivot_wider(names_from = hscp_locality, values_from = measure)

# Summary items for Excel/SDC/Downstream
emergency_adm <- emergency_adm_raw # for excel output
bed_days <- bed_days_raw # for excel output
bed_days_mh <- bed_days_mh_raw # for excel output
ae_attendances <- ae_attendances_raw # for excel output
delayed_disch <- delayed_disch_raw # for excel output
falls <- falls_raw # for excel output
readmissions <- readmissions_raw # for excel output
ppa <- ppa_raw # for excel output
psych_hosp <- psych_hosp_raw # for excel output
