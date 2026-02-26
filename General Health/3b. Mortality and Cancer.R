##### 2b Deaths aged 15-44 #####

## Create variables for latest year
latest_period_deaths_15_44 <- unique(
  filter(deaths_15_44, year == max(deaths_15_44$year))$period_short
)

## Time trend
deaths_15_44_time_trend <- scotpho_time_trend(
  data = deaths_15_44,
  chart_title = "Deaths Aged 15 to 44 Time Trend",
  xaxis_title = "Year Groups (3-year aggregates)",
  yaxis_title = "Deaths, aged 15 to 44\n(Standardised rates per 100,000)",
  string_wrap = 10
)


## Bar chart
deaths_15_44_bar <- scotpho_bar_chart(
  data = deaths_15_44,
  chart_title = paste0(
    "Deaths, Aged 15 to 44 by area, ",
    max(deaths_15_44[["period_short"]])
  ),
  xaxis_title = "Deaths (Standardised rates per 100,000)"
)


## Numbers for text
deaths_15_44_latest <- filter(
  deaths_15_44,
  year == max(deaths_15_44$year),
  area_name == LOCALITY,
  area_type == "Locality"
)$measure

scot_deaths_15_44 <- filter(
  deaths_15_44,
  year == max(deaths_15_44$year),
  area_name == "Scotland"
)$measure

deaths_15_44_diff_scot <- if_else(
  deaths_15_44_latest > scot_deaths_15_44,
  "higher",
  "lower"
)


##### 2c Cancer #####

### Cancer Registrations

## Time objects
latest_period_cancer_reg <- unique(
  filter(cancer_reg, year == max(cancer_reg$year))$period_short
)
prev_period_cancer_reg <- unique(
  filter(cancer_reg, year == max(cancer_reg$year) - 1)$period_short
)

## Time trend
cancer_reg_time_trend <- scotpho_time_trend(
  data = cancer_reg,
  chart_title = "Cancer Registrations Time Trend",
  xaxis_title = "Year Groups (3-year aggregates)",
  yaxis_title = "Cancer registrations \n(Standardised rates per 100,000)",
  string_wrap = 10
)


## Numbers for text
cancer_reg_rate_latest <- filter(
  cancer_reg,
  year == max(cancer_reg$year),
  area_name == LOCALITY,
  area_type == "Locality"
)$measure

cancer_reg_total_latest <- filter(
  cancer_reg,
  year == max(cancer_reg$year),
  area_name == LOCALITY,
  area_type == "Locality"
)$numerator


### Early deaths from cancer

latest_period_early_deaths_cancer <- unique(
  filter(
    early_deaths_cancer,
    year == max(early_deaths_cancer$year)
  )$period_short
)

early_deaths_cancer_rate_latest <- filter(
  early_deaths_cancer,
  year == max(early_deaths_cancer$year),
  area_name == LOCALITY,
  area_type == "Locality"
)$measure

## Time trend for cancer deaths
early_deaths_cancer_time_trend <- scotpho_time_trend(
  data = early_deaths_cancer,
  chart_title = "Early Deaths from Cancer Time Trend",
  xaxis_title = "Year Groups (3-year aggregates)",
  yaxis_title = "Early deaths from cancer\n(Standardised rates per 100,000)",
  string_wrap = 10
)


## Figures for text
early_deaths_cancer_rate_earliest <- filter(
  early_deaths_cancer,
  year == (max(early_deaths_cancer$year) - 10),
  area_name == LOCALITY,
  area_type == "Locality"
)$measure

cancer_deaths_perc_change <- if_else(
  early_deaths_cancer_rate_earliest == 0,
  NA_real_,
  abs(
    (early_deaths_cancer_rate_latest - early_deaths_cancer_rate_earliest) *
      100 /
      early_deaths_cancer_rate_earliest
  )
)

cancer_deaths_changeword <- if_else(
  early_deaths_cancer_rate_latest > early_deaths_cancer_rate_earliest,
  "increase",
  "decrease"
)
