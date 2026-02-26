##### 2d Hospitalisations from diseases #####

disease_hosp <- bind_rows(
  filter(asthma_hosp, year == max(year)),
  filter(chd_hosp, year == max(year)),
  filter(copd_hosp, year == max(year))
) %>%
  filter(
    (area_name == LOCALITY & area_type == "Locality") |
      (area_name == HSCP & area_type == "HSCP") |
      area_name == HB |
      area_name == "Scotland"
  ) %>%
  mutate(
    area_type = factor(
      area_type,
      levels = c("Locality", "HSCP", "Health board", "Scotland")
    ),
    area_name = fct_reorder(as.factor(area_name), as.numeric(area_type))
  ) %>%
  mutate(
    indicator = case_when(
      str_detect(indicator, fixed("Asthma")) ~ "Asthma",
      str_detect(indicator, fixed("CHD")) ~ "Coronary Heart Disease",
      str_detect(indicator, fixed("COPD")) ~ "COPD"
    )
  ) %>%
  mutate(measure = round_half_up(measure, 1))

highest_hosp_disease <- disease_hosp %>%
  filter(
    area_name == LOCALITY,
    area_type == "Locality"
  ) %>%
  filter(measure == max(measure))

disease_hosp_table <- disease_hosp |>
  mutate(
    area_order = case_when(
      area_name == LOCALITY ~ 1L,
      area_name == HSCP ~ 2L,
      str_starts(area_name, fixed("NHS")) ~ 4L,
      area_name == "Scotland" ~ 5L,
      .default = 2L
    )
  ) |>
  arrange(area_order) |>
  select(indicator, period_short, area_name, measure) |>
  pivot_wider(names_from = area_name, values_from = measure) |>
  rename(
    "Disease" = indicator,
    "Latest time period" = period_short
  )

table8_year_title <- max(disease_hosp_table[["Latest time period"]])

##### 2e Prescriptions for Anxiety, Depression and Psychosis #####

## Time objects
latest_period_adp_presc <- unique(
  filter(adp_presc, year == max(adp_presc$year))$period_short
)
prev_period_adp_presc <- unique(
  filter(adp_presc, year == max(adp_presc$year) - 10)$period_short
)

## Time trend
adp_presc_time_trend <- scotpho_time_trend(
  data = adp_presc,
  chart_title = "Anxiety, Depression and Psychosis Prescriptions Time Trend",
  xaxis_title = "Financial Year",
  yaxis_title = "Population prescribed\n medication (%)",
  string_wrap = 20,
  rotate_xaxis = TRUE
)


## Bar chart
adp_presc_bar <- adp_presc %>%
  scotpho_bar_chart(
    data = .,
    chart_title = paste0(
      "Anxiety, Depression and Psychosis Prescriptions, ",
      max(.$period_short)
    ),
    xaxis_title = "Population prescribed medication (%)"
  )


## Numbers for text

adp_presc_latest <- filter(
  adp_presc,
  year == max(adp_presc$year),
  area_name == LOCALITY,
  area_type == "Locality"
)$measure

adp_presc_earliest <- filter(
  adp_presc,
  year == (max(adp_presc$year) - 10),
  area_name == LOCALITY,
  area_type == "Locality"
)$measure

adp_presc_perc_change <- abs(
  (adp_presc_latest - adp_presc_earliest) * 100 / adp_presc_earliest
)
adp_presc_changeword <- if_else(
  adp_presc_latest > adp_presc_earliest,
  "increase",
  "decrease"
)

scot_adp_presc <- filter(
  adp_presc,
  year == max(adp_presc$year),
  area_name == "Scotland"
)$measure

adp_presc_diff_scot <- if_else(
  adp_presc_latest > scot_adp_presc,
  "larger",
  "smaller"
)


############################ 3) SLF DATA (LTCs) ####################################

# Extract SLF adjusted populations
slf_pops <- distinct(ltc, age_group, hscp_locality, hscp2019name, slf_adj_pop)

slf_pop_loc <- filter(slf_pops, hscp_locality == LOCALITY)

# Determine year
latest_year_ltc <- ltc[["year"]][1]

## Create Scotland totals
ltc_scot <- ltc %>%
  select(-year, -hscp2019name, -hscp_locality, -slf_adj_pop) %>%
  group_by(total_ltc, age_group) %>%
  summarise(across(everything(), sum)) %>%
  ungroup()

###### 3a Waffle Chart Infographic ######

# Load images
# under 65
