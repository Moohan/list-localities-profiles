# 5. Fall Admissions ----
# _________________________________________________________________________

falls <- read_parquet(paste0(import_folder, "falls_smr.parquet")) %>%
  filter(financial_year <= max_fy) %>%
  filter(age_group %in% c("65 - 74", "75+"))

# Plotting by area
falls_areas <- falls %>%
  rename(n = admissions) %>%
  aggregate_usc_area_data() %>%
  left_join(pop_areas_65plus, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  drop_na(year)

Falls_loc_ts <- area_trend_usc(
  data_for_plot = falls_areas,
  plot_title = paste0(
    "Emergency admissions from falls per 100,000 population aged over 65\n",
    "over time by residence"
  ),
  yaxis_title = "Emergency admissions from falls rate\nper 100,000 population aged 65+",
  source = "Source: PHS SMR01"
)


# Objects for text and summary table
min_year_falls <- min(falls_areas$financial_year)
max_year_falls <- max(falls_areas$financial_year)

latest_falls_loc <- falls_areas %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_falls_loc1 <- latest_falls_loc %>% pull(formatted_data)
latest_falls_loc2 <- latest_falls_loc %>% pull(data)

first_falls_loc <- falls_areas %>%
  filter(
    location == LOCALITY,
    year == min(year)
  ) %>%
  pull(data)

percent_rate_change_falls_loc <- percent_change_calc(
  latest_falls_loc2,
  first_falls_loc
)
word_change_rate_falls_loc <- word_change_calc(
  latest_falls_loc2,
  first_falls_loc
)

hscp_falls <- falls_areas %>%
  filter(
    location == HSCP,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hscp_falls1 <- hscp_falls %>% pull(formatted_data)
hscp_falls2 <- hscp_falls %>% pull(data)

first_falls_hscp <- falls_areas %>%
  filter(
    location == HSCP,
    year == min(year)
  ) %>%
  pull(data)

percent_rate_change_falls_hscp <- percent_change_calc(
  hscp_falls2,
  first_falls_hscp
)
word_change_rate_falls_hscp <- word_change_calc(hscp_falls2, first_falls_hscp)

scot_falls <- falls_areas %>%
  filter(
    location == "Scotland",
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

scot_falls1 <- scot_falls %>% pull(formatted_data)
scot_falls2 <- scot_falls %>% pull(data)

first_falls_scot <- falls_areas %>%
  filter(
    location == "Scotland",
    year == min(year)
  ) %>%
  pull(data)

percent_rate_change_falls_scot <- percent_change_calc(
  scot_falls2,
  first_falls_scot
)
word_change_rate_falls_scot <- word_change_calc(scot_falls2, first_falls_scot)

# NHS health board
hb_falls <- falls_areas %>%
  filter(
    location == HB,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hb_falls1 <- hb_falls %>% pull(formatted_data)
hb_falls2 <- hb_falls %>% pull(data)

first_fy_hb_falls <- filter(
  falls_areas,
  financial_year == min(financial_year),
  location == HB
)$data

hb_rate_change_falls <- round(
  abs(hb_falls2 - first_fy_hb_falls) / first_fy_hb_falls * 100,
  digits = 1
)
word_change_hb_falls <- word_change_calc(hb_falls2, first_fy_hb_falls)

# 6. Readmissions (28 days) ----
# _________________________________________________________________________

readmissions <- read_parquet(paste0(
  import_folder,
  "readmissions_smr.parquet"
)) %>%
  filter(financial_year <= max_fy)

# Plotting by age

readmissions_age <- readmissions %>%
  filter(hscp_locality == LOCALITY) %>%
  drop_na(age_group) %>%
  group_by(financial_year, age_group) %>%
  summarise(
    read_28 = sum(read_28),
    discharges = sum(discharges)
  ) %>%
  ungroup() %>%
  mutate(data = round_half_up(read_28 / discharges * 1000, 1))

read_age_ts <- age_group_trend_usc(
  data_for_plot = readmissions_age,
  plot_title = paste(
    "Readmission rate (28 days) per 1,000 discharges by age group\n for",
    LOCALITY
  ),
  yaxis_title = "Readmission rate (28 days)\n per 1,000 discharges",
  source = "Source: PHS SMR01"
)


# Plotting by area

# First use aggregating function on readmissions, then on discharges, then join

read1 <- readmissions %>%
  select(-discharges) %>%
  rename(n = read_28) %>%
  aggregate_usc_area_data() %>%
  rename(read_28 = n)

read2 <- readmissions %>%
  select(-read_28) %>%
  rename(n = discharges) %>%
  aggregate_usc_area_data() %>%
  rename(discharges = n)

readmissions_areas <- left_join(
  read1,
  read2,
  by = join_by(financial_year, location, area_type)
) %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(read_28 / discharges * 1000, 1)) %>%
  drop_na(year)

rm(read1, read2)

read_loc_ts <- area_trend_usc(
  data_for_plot = readmissions_areas,
  plot_title = paste(
    "Readmission rate (28 days) per 1,000 discharges over time by residence"
  ),
  yaxis_title = "Readmission rate (28 days)\n per 1,000 discharges",
  source = "Source: PHS SMR01"
)

# Objects for text and summary table-age
min_year_re_age <- min(readmissions_age$financial_year)
max_year_re_age <- max(readmissions_age$financial_year)

latest_re_max_age <- readmissions_age %>%
  filter(
    financial_year == max(financial_year)
  ) %>%
  filter(
    data == max(data)
  )

latest_re_max_age_data <- latest_re_max_age %>% pull(data)
latest_re_max_age_group <- latest_re_max_age %>% pull(age_group)

first_re_max_age <- readmissions_age %>%
  filter(
    financial_year == min(financial_year),
    age_group == latest_re_max_age_group
  ) %>%
  pull(data)


percent_rate_change_re_age <- percent_change_calc(
  latest_re_max_age_data,
  first_re_max_age
)
word_change_rate_re_age <- word_change_calc(
  latest_re_max_age_data,
  first_re_max_age
)

latest_re_min_age <- readmissions_age %>%
  filter(
    financial_year == max(financial_year)
  ) %>%
  filter(data == min(data))

latest_re_min_age_data <- latest_re_min_age %>% pull(data)
latest_re_min_age_group <- latest_re_min_age %>% pull(age_group)

first_re_min_age <- readmissions_age %>%
  filter(
    financial_year == min(financial_year),
    age_group == latest_re_min_age_group
  ) %>%
  pull(data)

percent_rate_change_re_age_min <- percent_change_calc(
  latest_re_min_age_data,
  first_re_min_age
)
word_change_rate_re_age_min <- word_change_calc(
  latest_re_min_age_data,
  first_re_min_age
)

# Objects for text and summary table-area
min_year_re_area <- min(readmissions_areas$financial_year)
max_year_re_area <- max(readmissions_areas$financial_year)

first_read_loc <- readmissions_areas %>%
  filter(
    location == LOCALITY,
    year == min(year)
  )

first_read_loc1 <- first_read_loc %>% pull(data)

latest_read_loc <- readmissions_areas %>%
  filter(
    location == LOCALITY,
    year == max(year)
  )

latest_read_loc1 <- latest_read_loc %>% pull(data)

percent_rate_change_re_area <- percent_change_calc(
  latest_read_loc1,
  first_read_loc1
)
word_change_rate_re_area <- word_change_calc(latest_read_loc1, first_read_loc1)

first_hscp_read <- readmissions_areas %>%
  filter(
    location == HSCP,
    year == min(year)
  ) %>%
  pull(data)

hscp_read <- readmissions_areas %>%
  filter(
    location == HSCP,
    year == max(year)
  ) %>%
  pull(data)

percent_rate_change_re_area_hscp <- percent_change_calc(
  hscp_read,
  first_hscp_read
)
word_change_rate_re_area_hscp <- word_change_calc(hscp_read, first_hscp_read)

first_scot_read <- readmissions_areas %>%
  filter(
    location == "Scotland",
    year == min(year)
  ) %>%
  pull(data)


scot_read <- readmissions_areas %>%
  filter(
    location == "Scotland",
    year == max(year)
  ) %>%
  pull(data)

percent_rate_change_re_area_scot <- percent_change_calc(
  scot_read,
  first_scot_read
)
word_change_rate_re_area_scot <- word_change_calc(scot_read, first_scot_read)

# NHS health board
hb_read <- readmissions_areas %>%
  filter(
    location == HB,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hb_read1 <- hb_read %>% pull(formatted_data)
hb_read2 <- hb_read %>% pull(data)

first_fy_hb_read <- filter(
  readmissions_areas,
  financial_year == min(financial_year),
  location == HB
)$data

hb_rate_change_read <- percent_change_calc(hb_read2, first_fy_hb_read)
word_change_hb_read <- word_change_calc(hb_read2, first_fy_hb_read)

# 7. Comm 6 months ----
# _________________________________________________________________________________
#
# comm_6mo <- readRDS(paste0(import_folder, "comm_6mo_smr.rds")) %>%
#   rename(financial_year = financial_death) %>%
#   filter(financial_year <= max_fy)
#
# # Aggregate data
# # First use aggregating function on bed days, then on deaths, then join
#
# comm_6mo1 <- comm_6mo %>%
#   select(-total_deaths) %>%
#   rename(n = total_bddys) %>%
#   aggregate_usc_area_data() %>%
#   rename(total_bddys = n)
#
# comm_6mo2 <- comm_6mo %>%
#   select(-total_bddys) %>%
#   rename(n = total_deaths) %>%
#   aggregate_usc_area_data() %>%
#   rename(total_deaths = n)
#
# comm_6mo_areas <- left_join(comm_6mo1, comm_6mo2) %>%
#   left_join(pop_areas_all_ages) %>%
#   mutate(data = round_half_up(100*(1 - total_bddys/total_deaths/182.5), 1))
#
# #Locality Table
#
# comm_6mo_loc_table <- comm_6mo_areas %>%
#   filter(location == LOCALITY) %>%
#   mutate(data = paste0(data, "%"),
#          financial_year = paste0("**", financial_year, "**"),
#          financial_year = fct_reorder(financial_year, year)) %>%
#   select(location, financial_year, data) %>%
#   pivot_wider(names_from = financial_year, values_from = data) %>%
#   select(-location)
#
# # Bar plot comparing areas in latest year
#
# comm_loc_bar <- comm_6mo_areas %>%
#   filter(year == max(year)) %>%
#   mutate(location = fct_reorder(as.factor(str_wrap(location, 28)), as.numeric(area_type))) %>%
#
#   ggplot(aes(x = location, y = data, fill = location, weight = data)) +
#   geom_col(position = position_dodge()) +
#   geom_text(aes(y = data, label = round_half_up(data, 1)),
#             position=position_dodge(width=0.9),
#             vjust=-0.25, color = "#4a4a4a", size = 4, fontface = "bold") +
#   scale_y_continuous(labels = comma, limits = c(0, 1.1*max(comm_6mo_areas$data))) +
#   scale_fill_manual(values = palette) +
#   theme_profiles() +
#   theme(panel.grid.major.y = element_line(color = "grey85"),
#         panel.grid.major.x = element_blank(),
#         axis.title.x = element_blank(),
#         axis.text.x = element_text(size = 10)) +
#   labs(y = "% Last 6 months in community",
#        caption = "Source: NRS Death Records, PHS SMR01, SMR01E and SMR04") +
#   guides(fill= "none")
#
#
# # Objects for text and summary table
# latest_comm_6mo_loc <- comm_6mo_areas %>%
#   filter(location == LOCALITY,
#          year == max(year)) %>%
#   pull(data)
#
# hscp_comm_6mo <- comm_6mo_areas %>%
#   filter(location == HSCP,
#          year == max(year)) %>%
#   pull(data)
#
# scot_comm_6mo <- comm_6mo_areas %>%
#   filter(location == "Scotland",
#          year == max(year)) %>%
#   pull(data)
#
# other_loc_comm_6mo <- comm_6mo %>%
#   group_by(financial_year, hscp_locality) %>%
#   summarise(total_bddys = sum(total_bddys),
#             total_deaths = sum(total_deaths)) %>%
#   ungroup() %>%
#   right_join(pops_other_locs) %>%
#   mutate(total_bddys = replace_na(total_bddys, 0),
#          total_deaths = replace_na(total_deaths, 0)) %>%
#   mutate(data = as.character(round_half_up(100*(1 - total_bddys/total_deaths/182.5), 1))) %>%
#   select(hscp_locality, data) %>%
#   pivot_wider(names_from = hscp_locality, values_from = data)

# 8. Potentially Preventable Admissions ----
# _______________________________________________________________________________________________________

ppa <- read_parquet(paste0(import_folder, "ppa_smr.parquet")) %>%
  filter(financial_year <= max_fy)

# % PPAs in locality under and over 65
ppa_total <- ppa %>%
  rename(n = admissions) %>%
  aggregate_usc_area_data()

ppa_65plus <- ppa %>%
  filter(age_group %in% c("65 - 74", "75+")) %>%
  rename(n = admissions) %>%
  aggregate_usc_area_data() %>%
  rename(plus65tot = n) %>%
  left_join(ppa_total, by = join_by(financial_year, location)) %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(plus65tot / n * 100, 1)) %>%
  drop_na(year)

latest_ppa_65plus <- ppa_65plus %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  pull(data)

ppa_under65 <- ppa %>%
  filter(age_group %in% c("0 - 17", "18 - 44", "45 - 64")) %>%
  rename(n = admissions) %>%
  aggregate_usc_area_data() %>%
  rename(under65tot = n) %>%
  left_join(ppa_total, by = join_by(financial_year, location)) %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(under65tot / n * 100, 1)) %>%
  drop_na(year)

latest_ppa_under65 <- ppa_under65 %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  pull(data)


# Plotting by area
ppa_areas <- ppa %>%
  rename(n = admissions) %>%
  aggregate_usc_area_data() %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  mutate(
    location = factor(location, levels = c(LOCALITY, HSCP, HB, "Scotland"))
  ) %>%
  arrange(location) %>%
  drop_na(year)


ppa_loc_ts <- area_trend_usc(
  data_for_plot = ppa_areas,
  plot_title = paste(
    "Potentially Preventable Emergency Admissions per 100,000 by residence"
  ),
  yaxis_title = "PPA rate\nper 100,000 population",
  source = "Source: PHS SMR01"
)


# Objects for text and summary table
max_year_ppa_areas <- max(ppa_areas$financial_year)
min_year_ppa_areas <- min(ppa_areas$financial_year)


latest_ppa_loc <- ppa_areas %>%
  filter(
    location == LOCALITY,
    year == max(year) | year == min(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_ppa_loc1 <- latest_ppa_loc$formatted_data[2]

ppa_diff <- percent_change_calc(latest_ppa_loc$data[2], latest_ppa_loc$data[1])
ppa_word_change <- word_change_calc(
  latest_ppa_loc$data[2],
  latest_ppa_loc$data[1]
)

hscp_ppa <- ppa_areas %>%
  filter(
    location == HSCP,
    year == max(year) | year == min(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

ppa_diff_hscp <- percent_change_calc(hscp_ppa$data[2], hscp_ppa$data[1])
ppa_word_change_hscp <- word_change_calc(hscp_ppa$data[2], hscp_ppa$data[1])

scot_ppa <- ppa_areas %>%
  filter(
    location == "Scotland",
    year == max(year) | year == min(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

diff_scot_ppa <- percent_change_calc(scot_ppa$data[2], scot_ppa$data[1])
word_change_scot_ppa <- word_change_calc(scot_ppa$data[2], scot_ppa$data[1])

# NHS health board
hb_ppa <- ppa_areas %>%
  filter(
    location == HB,
    year == max(year) | year == min(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

diff_hb_ppa <- percent_change_calc(hb_ppa$data[2], hb_ppa$data[1])
word_change_hb_ppa <- word_change_calc(hb_ppa$data[2], hb_ppa$data[1])

other_loc_ppa <- ppa %>%
  group_by(financial_year, hscp_locality) %>%
  summarise(admissions = sum(admissions)) %>%
  ungroup() %>%
  right_join(pops_other_locs, by = join_by(financial_year, hscp_locality)) %>%
  mutate(admissions = replace_na(admissions, 0)) %>%
  mutate(data = round_half_up(admissions / pop * 100000)) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  select(hscp_locality, data) %>%
  pivot_wider(names_from = hscp_locality, values_from = data)


# 9. Psychiatric hospital admissions (ScotPHO) ----
# ___________________________________________________________________________

psych_hosp <- read_csv(paste0(
  import_folder,
  "scotpho_data_extract_psychiatric_admissions.csv"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 18), fixed = TRUE))

check_missing_data_scotpho(psych_hosp)

## Create variables for latest year
latest_period_psych_hosp <- unique(
  filter(psych_hosp, year == max(psych_hosp$year))$period_short
)

## Time trend
psych_hosp_time_trend <- psych_hosp %>%
  scotpho_time_trend(
    data = .,
    chart_title = "Psychiatric Patient Hospitalisations Time Trend",
    xaxis_title = "Financial Year Groups (3-year aggregates)",
    yaxis_title = "Psychiatric patient hospitalisations\n(Standardised rates per 100,000)",
    string_wrap = 10,
    rotate_xaxis = TRUE
  )


## Objects for text and summary table

# Locality latest value
psych_hosp_latest <- round_half_up(
  filter(
    psych_hosp,
    year == max(psych_hosp$year) &
      (area_name == LOCALITY & area_type == "Locality")
  )$measure,
  1
)

other_locs_psych_hosp <- psych_hosp %>%
  filter(year == max(year)) %>%
  filter(area_type == "Locality") %>%
  rename("hscp_locality" = "area_name") %>%
  right_join(other_locs, by = join_by(hscp_locality)) %>%
  arrange(hscp_locality) %>%
  select(hscp_locality, measure) %>%
  mutate(measure = as.character(round_half_up(measure, 1))) %>%
  pivot_wider(names_from = hscp_locality, values_from = measure)

hscp_psych_hosp <- round_half_up(
  filter(
    psych_hosp,
    year == max(year) & (area_name == HSCP & area_type == "HSCP")
  )$measure,
  1
)

scot_psych_hosp <- round_half_up(
  filter(psych_hosp, year == max(year) & area_name == "Scotland")$measure,
  1
)


list_years <- unique(psych_hosp_time_trend$data[5])
list_years_latest <- list_years$period


# Locality
loc_psych_hosp <- psych_hosp %>%
  filter(period %in% list_years_latest) %>%
  filter(
    area_name == LOCALITY &
      area_type == "Locality",
    year == min(year) | year == max(year)
  ) %>%
  mutate(measure2 = format(measure, big.mark = ","))

diff_loc_psych <- percent_change_calc(
  loc_psych_hosp$measure[2],
  loc_psych_hosp$measure[1]
)
word_change_loc_psych <- word_change_calc(
  loc_psych_hosp$measure[2],
  loc_psych_hosp$measure[1]
)

# HSCP
hscp_psych_hosp <- psych_hosp %>%
  filter(period %in% list_years_latest) %>%
  filter(
    area_name == HSCP &
      area_type == "HSCP",
    year == min(year) | year == max(year)
  ) %>%
  mutate(measure2 = format(measure, big.mark = ","))

diff_hscp_psych <- percent_change_calc(
  hscp_psych_hosp$measure[2],
  hscp_psych_hosp$measure[1]
)
word_change_hscp_psych <- word_change_calc(
  hscp_psych_hosp$measure[2],
  hscp_psych_hosp$measure[1]
)

# NHS health board
hb_psych_hosp <- psych_hosp %>%
  filter(period %in% list_years_latest) %>%
  filter(
    area_name == HB &
      area_type == "Health board",
    year == min(year) | year == max(year)
  ) %>%
  mutate(measure2 = format(measure, big.mark = ","))

diff_hb_psych <- percent_change_calc(
  hb_psych_hosp$measure[2],
  hb_psych_hosp$measure[1]
)
word_change_hb_psych <- word_change_calc(
  hb_psych_hosp$measure[2],
  hb_psych_hosp$measure[1]
)

# Scotland
scot_psych_hosp <- psych_hosp %>%
  filter(period %in% list_years_latest) %>%
  filter(
    area_name == "Scotland" &
      area_type == "Scotland",
    year == min(year) | year == max(year)
  ) %>%
  mutate(measure2 = format(measure, big.mark = ","))

diff_scot_psych <- percent_change_calc(
  scot_psych_hosp$measure[2],
  scot_psych_hosp$measure[1]
)
word_change_scot_psych <- word_change_calc(
  scot_psych_hosp$measure[2],
  scot_psych_hosp$measure[1]
)

# Housekeeping ----
# These objects are left over after the script is run
# but don't appear to be used in any 'downstream' process:
# Main markdown, Summary Table, Excel data tables, SDC output.
# TODO: Investigate if these can be removed earlier or not created at all.
rm(
  ae_attendances,
  age_group_trend_usc,
  aggregate_usc_area_data,
  area_trend_usc,
  bed_days,
  bed_days_mh,
  delayed_disch,
  emergency_adm,
  falls,
  first_ae_att_max_age,
  first_ae_att_max_age_data,
  first_ae_att_min_age,
  first_ae_att_min_data,
  first_bd_mh_max_age,
  first_bd_mh_min_age,
  first_bd_mh_min_age1,
  first_bed_days_mh_loc,
  first_dd_loc,
  first_ea_max_age,
  first_ea_min_age,
  first_ea_min_age1,
  first_falls_hscp,
  first_falls_loc,
  first_falls_scot,
  first_fy_hb,
  first_fy_hb_ae,
  first_fy_hb_dd,
  first_fy_hb_falls,
  first_fy_hb_mh,
  first_fy_hb_read,
  first_fy_hb_ubd,
  first_fy_hscp,
  first_fy_hscp_ae,
  first_fy_hscp_ubd,
  first_fy_rate,
  first_fy_rate_ae_areas,
  first_fy_rate_ubd,
  first_fy_scot,
  first_fy_scot_ae,
  first_fy_scot_ubd,
  first_hscp_bed_days_mh,
  first_hscp_dd,
  first_hscp_read,
  first_re_max_age,
  first_re_min_age,
  first_read_loc,
  first_read_loc1,
  first_scot_bed_days_mh,
  first_scot_dd,
  first_scot_read,
  first_ubd_max_age,
  first_ubd_min_age,
  first_ubd_min_age1,
  hb_ae_att,
  hb_ae2,
  hb_bed_days,
  hb_bed_days2,
  hb_dd,
  hb_dd2,
  hb_emergency_adm,
  hb_emergency_adm2,
  hb_falls,
  hb_falls2,
  hb_mh_beddays,
  hb_mh_beddays2,
  hb_read,
  hb_read2,
  hscp_ae_att2,
  hscp_bed_days_mh2,
  hscp_bed_days2,
  hscp_dd2,
  hscp_emergency_adm2,
  hscp_falls,
  hscp_falls2,
  import_folder,
  latest_ae_att_loc,
  latest_ae_att_loc2,
  latest_ae_att_loc2_age,
  latest_ae_att_loc2_age_min,
  latest_ae_att_max_age,
  latest_ae_att_min_age,
  latest_bd_mh_max_age,
  latest_bd_mh_max_age2,
  latest_bd_mh_min_age,
  latest_bd_mh_min_age2,
  latest_bed_days_loc,
  latest_bed_days_loc2,
  latest_bed_days_mh_loc2,
  latest_dd_loc,
  latest_dd_loc2,
  latest_ea_max_age,
  latest_ea_max_age2,
  latest_ea_min_age,
  latest_ea_min_age2,
  latest_emergency_adm_loc,
  latest_emergency_adm_loc2,
  latest_falls_loc,
  latest_falls_loc2,
  latest_re_max_age,
  latest_re_min_age,
  latest_read_loc,
  latest_ubd_max_age,
  latest_ubd_max_age2,
  latest_ubd_min_age,
  latest_ubd_min_age2,
  list_years,
  list_years_latest,
  loc_pop,
  loc_pop_age1,
  loc_pop_age2,
  localities,
  # max_fy,
  min_year_ea_age1,
  min_year_ubd_age1,
  percent_change_calc,
  pop_areas_65plus,
  pop_areas_all_ages,
  pops_other_locs,
  pops_other_locs_65plus,
  populations,
  populations_proxy_year,
  ppa,
  ppa_65plus,
  ppa_total,
  ppa_under65,
  readmissions,
  scot_ae_att2,
  scot_bed_days_mh2,
  scot_bed_days2,
  scot_dd2,
  scot_emergency_adm2,
  scot_falls,
  scot_falls2,
  word_change_calc
)
gc()

## Stat disclosure control

# writexl::write_xlsx(x = list("Emergency Adm" = emergency_adm,
#                              "Unsch Bed Days" = bed_days,
#                              "Unsch Bed Days (SMR4)" = bed_days_mh,
#                              "A&E Att" = ae_attendances,
#                              "Delayed Disch" = delayed_disch),
#                     path = paste0(lp_path, "Publishing/MSG Data.xlsx"))

# falls_sdc <- falls %>%
#   group_by(financial_year, hscp2019name, hscp_locality) %>%
#   summarise(falls_admission = sum(admissions)) %>%
#   ungroup()

# writexl::write_xlsx(x = list("Falls" = falls_sdc,
#                              "Readmissions 28" = readmissions,
#                              "PPA" = ppa),
#                     path = paste0(lp_path, "Publishing/SMR Data.xlsx"))
