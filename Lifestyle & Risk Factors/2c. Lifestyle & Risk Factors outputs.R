############################################################################################# .
#                                                                                           #
#                   LOCALITY PROFILES LIFESTYLE & RISK FACTORS OUTPUTS CODE                 #
#                                                                                           #
############################################################################################# .

## Code used to create infographics, charts, and figures for the Lifestyle & Risk factors
#  section of the locality profiles.

# Determine other localities based on LOCALITY object for charts (excluding current locality)
other_locs <- lookup %>%
  select(hscp_locality, hscp2019name) %>%
  filter(hscp2019name == HSCP & hscp_locality != LOCALITY) %>%
  arrange(hscp_locality)

############################### 2) OUTPUTS ####################################

##### 2a Drug-related hospital admissions #####

## Create variables for latest year
max_year_drug_hosp <- max(drug_hosp[["year"]])
min_year_drug_hosp <- min(drug_hosp[["year"]])
latest_period_drug_hosp <- drug_hosp[["period_short"]][which.max(drug_hosp[[
  "year"
]])]
earliest_period_drug_hosp <- drug_hosp[["period_short"]][which.min(drug_hosp[[
  "year"
]])]
# ScotPHO time trend will only be latest 10 years
trend_years <- 10
earliest_period_drug_hosp_trend <- drug_hosp[["period_short"]][match(
  max_year_drug_hosp - trend_years,
  drug_hosp[["year"]]
)]


## Time trend
drug_hosp_time_trend <- scotpho_time_trend(
  data = drug_hosp,
  chart_title = "Drug-related Hospital Admissions Time Trend",
  xaxis_title = "Financial Year Groups (3-year aggregates)",
  yaxis_title = "Drug-related admissions\n(Standardised rates per 100,000)",
  string_wrap = 10,
  trend_years = trend_years,
  rotate_xaxis = TRUE
)

# drug_hosp_time_trend

## Bar chart
drug_hosp_bar <- drug_hosp %>%
  scotpho_bar_chart(
    chart_title = paste0(
      "Drug-related Hospital Admissions by Area, ",
      latest_period_drug_hosp
    ),
    xaxis_title = "Drug-related admissions (Standardised rates per 100,000)"
  )

# drug_hosp_bar

## Numbers for text
drug_hosp_latest <- filter(
  drug_hosp,
  year == max_year_drug_hosp,
  area_name == LOCALITY,
  area_type == "Locality"
) |>
  pull(measure)

drug_hosp_earliest <- filter(
  drug_hosp,
  year == min_year_drug_hosp,
  area_name == LOCALITY,
  area_type == "Locality"
) |>
  pull(measure)

drug_hosp_change <- abs(
  (drug_hosp_latest - drug_hosp_earliest) / drug_hosp_earliest * 100
)
drug_hosp_change_word <- if_else(
  drug_hosp_latest > drug_hosp_earliest,
  "increase",
  "decrease"
)

drug_hosp_diff_scot <- if_else(
  drug_hosp_latest > scot_drug_hosp,
  "higher",
  "lower"
)


##### 2b Alcohol-related hospital admissions #####

## Create variables for latest year
latest_period_alcohol_hosp <- unique(
  filter(alcohol_hosp, year == max(alcohol_hosp$year))$period_short
)
earliest_period_alcohol_hosp <- unique(
  filter(alcohol_hosp, year == min(alcohol_hosp$year))$period_short
)


## Time trend
alcohol_hosp_time_trend <- scotpho_time_trend(
  data = alcohol_hosp,
  chart_title = "Alcohol-related Hospital Admissions Time Trend",
  xaxis_title = "Financial Year",
  yaxis_title = "Alcohol-related admissions\n(Standardised rates per 100,000)",
  string_wrap = 10,
  rotate_xaxis = TRUE
)

# alcohol_hosp_time_trend

## Bar chart
alcohol_hosp_bar <- alcohol_hosp %>%
  scotpho_bar_chart(
    data = .,
    chart_title = paste0(
      "Alcohol-related Hospital Admissions by Area, ",
      max(.$period_short)
    ),
    xaxis_title = "Alcohol-related admissions (Standardised rates per 100,000)"
  )

# alcohol_hosp_bar

## Numbers for text

alcohol_hosp_latest <- filter(
  alcohol_hosp,
  year == max(alcohol_hosp$year) &
    (area_name == LOCALITY & area_type == "Locality")
)$measure

alcohol_hosp_earliest <- filter(
  alcohol_hosp,
  (year == min(alcohol_hosp$year)) &
    (area_name == LOCALITY & area_type == "Locality")
)$measure

alcohol_hosp_change <- abs(
  (alcohol_hosp_latest - alcohol_hosp_earliest) / alcohol_hosp_earliest * 100
)
alcohol_hosp_change_word <- if_else(
  alcohol_hosp_latest > alcohol_hosp_earliest,
  "increase",
  "decrease"
)

alcohol_hosp_diff_scot <- if_else(
  alcohol_hosp_latest > scot_alcohol_hosp,
  "higher",
  "lower"
)


##### 2c Alcohol specific deaths #####

## Create variables for latest year
latest_period_alcohol_deaths <- unique(
  filter(alcohol_deaths, year == max(alcohol_deaths$year))$period_short
)
earliest_period_alcohol_deaths <- unique(
  filter(alcohol_deaths, year == min(alcohol_deaths$year))$period_short
)


## Time trend
alcohol_deaths_time_trend <- scotpho_time_trend(
  data = alcohol_deaths,
  chart_title = "Alcohol-specific Deaths Time Trend",
  xaxis_title = "Year Groups (5-year aggregates)",
  yaxis_title = "Alcohol-specific deaths\n(Standardised rates per 100,000)",
  string_wrap = 10
)

# alcohol_deaths_time_trend

## Bar chart
alcohol_deaths_bar <- alcohol_deaths %>%
  scotpho_bar_chart(
    data = .,
    chart_title = paste0(
      "Alcohol-specific Deaths by Area, ",
      max(.$period_short)
    ),
    xaxis_title = "Alcohol-specific deaths (Standardised rates per 100,000)"
  )

# alcohol_deaths_bar

## Numbers for text

alcohol_deaths_latest <- filter(
  alcohol_deaths,
  year == max(alcohol_deaths$year) &
    (area_name == LOCALITY & area_type == "Locality")
)$measure

alcohol_deaths_earliest <- filter(
  alcohol_deaths,
  (year == min(alcohol_deaths$year)) &
    (area_name == LOCALITY & area_type == "Locality")
)$measure

alcohol_deaths_change <- abs(
  (alcohol_deaths_latest - alcohol_deaths_earliest) /
    alcohol_deaths_earliest *
    100
)
alcohol_deaths_change_word <- if_else(
  alcohol_deaths_latest > alcohol_deaths_earliest,
  "higher",
  "lower"
)

alcohol_deaths_diff_scot <- if_else(
  alcohol_deaths_latest > scot_alcohol_deaths,
  "higher",
  "lower"
)


##### 2d Bowel Screening Uptake #####

## Create variables for latest year
latest_period_bowel_screening <- unique(
  filter(bowel_screening, year == max(bowel_screening$year))$period_short
)
earliest_period_bowel_screening <- unique(
  filter(bowel_screening, year == min(bowel_screening$year))$period_short
)


## Time trend
bowel_screening_time_trend <- scotpho_time_trend(
  data = bowel_screening,
  chart_title = "Bowel Screening Uptake Time Trend",
  xaxis_title = "Year Groups (3-year aggregates)",
  yaxis_title = "Bowel screening uptake (%)",
  string_wrap = 10
)

# bowel_screening_time_trend

## Bar chart
bowel_screening_bar <- bowel_screening %>%
  scotpho_bar_chart(
    data = .,
    chart_title = paste0(
      "Bowel Screening Uptake by Area, ",
      max(.$period_short)
    ),
    xaxis_title = "Bowel screening uptake (%)"
  )

# bowel_screening_bar

## Numbers for text

bowel_screening_latest <- filter(
  bowel_screening,
  year == max(bowel_screening$year) &
    (area_name == LOCALITY & area_type == "Locality")
)$measure

bowel_screening_earliest <- filter(
  bowel_screening,
  (year == min(bowel_screening$year)) &
    (area_name == LOCALITY & area_type == "Locality")
)$measure

bowel_screening_change <- abs(
  (bowel_screening_latest - bowel_screening_earliest) /
    bowel_screening_earliest *
    100
)
bowel_screening_change_word <- if_else(
  bowel_screening_latest > bowel_screening_earliest,
  "increase",
  "decrease"
)

bowel_screening_diff_scot <- if_else(
  bowel_screening_latest > scot_bowel_screening,
  "higher",
  "lower"
)
