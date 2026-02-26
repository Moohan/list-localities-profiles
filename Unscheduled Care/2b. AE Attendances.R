# 3. A&E Attendances ----
# _________________________________________________________________________

ae_attendances <- read_parquet(paste0(
  import_folder,
  "ae_attendances_msg.parquet"
)) %>%
  filter(financial_year <= max_fy)

# Plotting by age
ae_att_age <- ae_attendances %>%
  filter(
    hscp_locality == LOCALITY,
    age_group != "NA"
  ) %>%
  group_by(financial_year, age_group) %>%
  summarise(attendances = sum(attendances)) %>%
  ungroup() %>%
  left_join(loc_pop_age1, by = join_by(financial_year, age_group)) %>%
  mutate(data = round_half_up(attendances / pop * 100000)) %>%
  drop_na(year)


AandE_age_ts <- age_group_trend_usc(
  data_for_plot = ae_att_age,
  plot_title = paste(
    "A&E attendances per 100,000 over time by age group\n for",
    LOCALITY
  ),
  yaxis_title = "A&E attendance rate\n per 100,000 population",
  source = "Source: PHS A&E Datamart"
)


# Plotting by area
ae_att_areas <- ae_attendances %>%
  rename(n = attendances) %>%
  aggregate_usc_area_data() %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  drop_na(year)


AandE_loc_ts <- area_trend_usc(
  data_for_plot = ae_att_areas,
  plot_title = paste("A&E attendances per 100,000 over time by residence"),
  yaxis_title = "A&E attendance rate\n per 100,000 population",
  source = "Source: PHS A&E Datamart"
)

# Objects for text and summary table- age

min_year_ae_age <- min(ae_att_age$financial_year)
max_year_ae_age <- max(ae_att_age$financial_year)


latest_ae_att_max_age <- ae_att_age %>%
  filter(
    year == max(year)
  ) %>%
  filter(
    data == max(data)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_ae_att_loc1_age <- latest_ae_att_max_age %>% pull(formatted_data)
latest_ae_att_loc2_age <- latest_ae_att_max_age %>% pull(data)

age_group_max <- latest_ae_att_max_age %>% pull(age_group)

first_ae_att_max_age <- ae_att_age %>%
  filter(
    year == min(year),
    age_group == age_group_max
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

first_ae_att_max_age_data <- first_ae_att_max_age %>% pull(data)

percent_rate_change_ae_age <- percent_change_calc(
  latest_ae_att_loc2_age,
  first_ae_att_max_age_data
)
word_change_rate_ae_age <- word_change_calc(
  latest_ae_att_loc2_age,
  first_ae_att_max_age_data
)

latest_ae_att_min_age <- ae_att_age %>%
  filter(
    year == max(year)
  ) %>%
  filter(
    data == min(data)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_ae_att_loc1_age_min <- latest_ae_att_min_age %>% pull(formatted_data)
latest_ae_att_loc2_age_min <- latest_ae_att_min_age %>% pull(data)
age_group_min <- latest_ae_att_min_age %>% pull(age_group)

first_ae_att_min_age <- ae_att_age %>%
  filter(
    year == min(year),
    age_group == age_group_min
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

first_ae_att_min_data <- first_ae_att_min_age %>% pull(data)

percent_rate_change_ae_age2 <- percent_change_calc(
  latest_ae_att_loc2_age_min,
  first_ae_att_min_data
)
word_change_rate_ae_age2 <- word_change_calc(
  latest_ae_att_loc2_age_min,
  first_ae_att_min_data
)


# Objects for text and summary table- area

min_year_ae_area <- min(ae_att_areas$financial_year)
max_year_ae_area <- max(ae_att_areas$financial_year)

first_fy_rate_ae_areas <- filter(
  ae_att_areas,
  financial_year == min(financial_year),
  location == LOCALITY,
  area_type == "Locality"
)$data

latest_ae_att_loc <- ae_att_areas %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_ae_att_loc1 <- latest_ae_att_loc %>% pull(formatted_data)
latest_ae_att_loc2 <- latest_ae_att_loc %>% pull(data)

percent_rate_change_ae_areas <- percent_change_calc(
  latest_ae_att_loc2,
  first_fy_rate_ae_areas
)
word_change_rate_ae_areas <- word_change_calc(
  latest_ae_att_loc2,
  first_fy_rate_ae_areas
)

hscp_ae_att <- ae_att_areas %>%
  filter(
    location == HSCP,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hscp_ae_att1 <- hscp_ae_att %>% pull(formatted_data)
hscp_ae_att2 <- hscp_ae_att %>% pull(data)

first_fy_hscp_ae <- filter(
  ae_att_areas,
  financial_year == min(financial_year),
  area_type == "HSCP"
)$data

percent_rate_change_ae_areas_hscp <- percent_change_calc(
  hscp_ae_att2,
  first_fy_hscp_ae
)
word_change_rate_ae_areas_hscp <- word_change_calc(
  hscp_ae_att2,
  first_fy_hscp_ae
)

scot_ae_att <- ae_att_areas %>%
  filter(
    location == "Scotland",
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))
scot_ae_att1 <- scot_ae_att %>% pull(formatted_data)
scot_ae_att2 <- scot_ae_att %>% pull(data)

first_fy_scot_ae <- filter(
  ae_att_areas,
  financial_year == min(financial_year),
  location == "Scotland"
)$data

percent_rate_change_ae_areas_scot <- percent_change_calc(
  scot_ae_att2,
  first_fy_scot_ae
)
word_change_rate_ae_areas_scot <- word_change_calc(
  scot_ae_att2,
  first_fy_scot_ae
)

# NHS health board
hb_ae_att <- ae_att_areas %>%
  filter(
    location == HB,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hb_ae1 <- hb_ae_att %>% pull(formatted_data)
hb_ae2 <- hb_ae_att %>% pull(data)
first_fy_hb_ae <- filter(
  ae_att_areas,
  financial_year == min(financial_year),
  location == HB
)$data

hb_rate_change_ae <- percent_change_calc(hb_ae2, first_fy_hb_ae)
word_change_hb_ae <- word_change_calc(hb_ae2, first_fy_hb_ae)

other_loc_ae_att <- ae_attendances %>%
  group_by(financial_year, hscp_locality) %>%
  summarise(attendances = sum(attendances)) %>%
  ungroup() %>%
  right_join(pops_other_locs, by = join_by(financial_year, hscp_locality)) %>%
  mutate(attendances = replace_na(attendances, 0)) %>%
  mutate(data = round_half_up(attendances / pop * 100000)) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  select(hscp_locality, data) %>%
  pivot_wider(names_from = hscp_locality, values_from = data)
