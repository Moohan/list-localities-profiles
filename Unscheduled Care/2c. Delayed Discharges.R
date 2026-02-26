# 4. Delayed Discharges ----
# _________________________________________________________________________

delayed_disch <- read_parquet(paste0(
  import_folder,
  "delayed_discharges_msg.parquet"
)) %>%
  filter(financial_year <= max_fy) %>%
  filter(age_group %in% c("65 - 74", "75+")) %>%
  group_by(financial_year, hscp2019name, hscp_locality) %>%
  summarise(
    dd_people = sum(dd_people),
    dd_bed_days = sum(dd_bed_days)
  ) %>%
  ungroup()


# Plotting by area
delayed_disch_areas <- delayed_disch %>%
  rename(n = dd_bed_days) %>%
  aggregate_usc_area_data() %>%
  left_join(pop_areas_65plus, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  drop_na(year)

DD_loc_ts <- area_trend_usc(
  data_for_plot = delayed_disch_areas,
  plot_title = paste0(
    "Delayed discharge bed days per 100,000 population aged over 65\n",
    "over time by residence"
  ),
  yaxis_title = "Delayed discharge bed day rate\n per 100,000 population aged 65+",
  source = "Source: PHS Delayed Discharges"
)


# Objects for text and summary table
min_year_dd <- min(delayed_disch_areas$financial_year)
max_year_dd <- max(delayed_disch_areas$financial_year)

latest_dd_loc <- delayed_disch_areas %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_dd_loc1 <- latest_dd_loc %>% pull(formatted_data)
latest_dd_loc2 <- latest_dd_loc %>% pull(data)

first_dd_loc <- delayed_disch_areas %>%
  filter(
    location == LOCALITY,
    year == min(year)
  ) %>%
  pull(data)

percent_rate_change_dd_loc <- percent_change_calc(latest_dd_loc2, first_dd_loc)
word_change_rate_dd_loc <- word_change_calc(latest_dd_loc2, first_dd_loc)


hscp_dd <- delayed_disch_areas %>%
  filter(
    location == HSCP,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hscp_dd1 <- hscp_dd %>% pull(formatted_data)
hscp_dd2 <- hscp_dd %>% pull(data)

first_hscp_dd <- delayed_disch_areas %>%
  filter(
    location == HSCP,
    year == min(year)
  ) %>%
  pull(data)

percent_rate_change_dd_hscp <- percent_change_calc(hscp_dd2, first_hscp_dd)
word_change_rate_dd_hscp <- word_change_calc(hscp_dd2, first_hscp_dd)


scot_dd <- delayed_disch_areas %>%
  filter(
    location == "Scotland",
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

scot_dd1 <- scot_dd %>% pull(formatted_data)
scot_dd2 <- scot_dd %>% pull(data)

first_scot_dd <- delayed_disch_areas %>%
  filter(
    location == "Scotland",
    year == min(year)
  ) %>%
  pull(data)

percent_rate_change_dd_scot <- percent_change_calc(scot_dd2, first_scot_dd)
word_change_rate_dd_scot <- word_change_calc(scot_dd2, first_scot_dd)

# NHS health board
hb_dd <- delayed_disch_areas %>%
  filter(
    location == HB,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hb_dd1 <- hb_dd %>% pull(formatted_data)
hb_dd2 <- hb_dd %>% pull(data)
first_fy_hb_dd <- filter(
  delayed_disch_areas,
  financial_year == min(financial_year),
  location == HB
)$data

hb_rate_change_dd <- percent_change_calc(hb_dd2, first_fy_hb_dd)
word_change_hb_dd <- word_change_calc(hb_dd2, first_fy_hb_dd)


other_loc_dd <- delayed_disch %>%
  group_by(financial_year, hscp_locality) %>%
  summarise(dd_bed_days = sum(dd_bed_days)) %>%
  ungroup() %>%
  right_join(
    pops_other_locs_65plus,
    by = join_by(financial_year, hscp_locality)
  ) %>%
  mutate(dd_bed_days = replace_na(dd_bed_days, 0)) %>%
  mutate(data = round_half_up(dd_bed_days / pop * 100000)) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  select(hscp_locality, data) %>%
  pivot_wider(names_from = hscp_locality, values_from = data)
