############################################################################################# .
#                                                                                           #
#                       LOCALITY PROFILES GENERAL HEALTH OUTPUTS CODE                       #
#                                                                                           #
############################################################################################# .

## Code used to create infographics, charts, and figures for the General Health section of
#  the locality profiles. This script handles locality-specific calculations.

### Geographical objects for locality ----

# Determine other localities based on LOCALITY object
other_locs <- all_locs_in_hscp |>
  dplyr::filter(hscp2019name == HSCP & hscp_locality != LOCALITY) |>
  dplyr::arrange(hscp_locality)

# Extract SLF adjusted populations for Locality
slf_pop_loc <- slf_pops_all |>
  dplyr::filter(hscp_locality == LOCALITY)

ltc_pops_total_loc <- sum(slf_pop_loc$slf_adj_pop)

############################### 2) SCOTPHO DATA ####################################

##### 2a Life expectancy #####

# Time objects

latest_year_life_exp_loc <- life_exp |>
  dplyr::filter(area_type == "Locality") |>
  dplyr::pull(year) |>
  max()

latest_period_life_exp_loc <- life_exp |>
  dplyr::filter(
    area_type == "Locality",
    year == latest_year_life_exp_loc
  ) |>
  dplyr::pull(period_short) |>
  unique()

# Create time trend
life_exp_trend <- life_exp |>
  dplyr::filter(
    area_name == LOCALITY,
    area_type == "Locality",
    year >= max(year) - 10
  ) |>
  dplyr::mutate(
    measure = phsmethods::round_half_up(measure, 1)
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = period_short,
    y = measure,
    group = sex,
    linetype = sex,
    shape = sex,
    colour = sex
  )) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_colour_manual(values = palette) +
  theme_profiles() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::expand_limits(y = 0) +
  ggplot2::labs(
    title = stringr::str_wrap(
      glue::glue("Average Life Expectancy in {LOCALITY} locality"),
      width = 65
    ),
    x = "Year Groups (5-year aggregates)",
    y = stringr::str_wrap("Average Life Expectancy (in years)", width = 35),
    caption = "Source: ScotPHO"
  ) +
  ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, 0, 1), "cm")) +
  ggplot2::guides(
    linetype = "none",
    shape = "none",
    colour = ggplot2::guide_legend(
      override.aes = list(shape = c(21, 24), fill = palette[1:2])
    )
  )


# Make a table to compare with other areas

life_exp_table <- life_exp |>
  dplyr::filter(
    (year == latest_year_life_exp_loc &
      (area_name == LOCALITY & area_type == "Locality")) |
      year == latest_year_life_exp_otherareas &
        ((area_name == HSCP & area_type == "HSCP") |
          area_name == HB |
          area_name == "Scotland")
  ) |>
  dplyr::mutate(
    measure = phsmethods::round_half_up(measure, 1),
    area_type = ordered(
      area_type,
      levels = c("Locality", "HSCP", "Health board", "Scotland")
    )
  ) |>
  dplyr::arrange(area_type) |>
  dplyr::select("Sex" = sex, area_name, measure) |>
  tidyr::pivot_wider(names_from = area_name, values_from = measure)


## Numbers for text

if (LOCALITY %in% check_missing_data_scotpho(life_exp)$area_name) {
  avg_life_exp_latest_male <- NA_real_
  avg_life_exp_latest_fem <- NA_real_
} else {
  avg_life_exp_latest <- dplyr::filter(
    life_exp,
    year == latest_year_life_exp_loc,
    area_name == LOCALITY,
    area_type == "Locality"
  )

  avg_life_exp_latest_male <- avg_life_exp_latest |>
    dplyr::filter(sex == "Male") |>
    dplyr::pull(measure) |>
    phsmethods::round_half_up(digits = 1)
  avg_life_exp_latest_fem <- avg_life_exp_latest |>
    dplyr::filter(sex == "Female") |>
    dplyr::pull(measure) |>
    phsmethods::round_half_up(digits = 1)
  rm(avg_life_exp_latest)
}


##### 2b Deaths aged 15-44 #####

## Create variables for latest year
latest_period_deaths_15_44 <- unique(
  dplyr::filter(deaths_15_44, year == max(deaths_15_44$year))$period_short
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
deaths_15_44_latest <- dplyr::filter(
  deaths_15_44,
  year == max(deaths_15_44$year),
  area_name == LOCALITY,
  area_type == "Locality"
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
  dplyr::filter(cancer_reg, year == max(cancer_reg$year))$period_short
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
cancer_reg_rate_latest <- dplyr::filter(
  cancer_reg,
  year == max(cancer_reg$year),
  area_name == LOCALITY,
  area_type == "Locality"
)$measure

cancer_reg_total_latest <- dplyr::filter(
  cancer_reg,
  year == max(cancer_reg$year),
  area_name == LOCALITY,
  area_type == "Locality"
)$numerator


### Early deaths from cancer

latest_period_early_deaths_cancer <- unique(
  dplyr::filter(
    early_deaths_cancer,
    year == max(early_deaths_cancer$year)
  )$period_short
)

early_deaths_cancer_rate_latest <- dplyr::filter(
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
early_deaths_cancer_rate_earliest <- dplyr::filter(
  early_deaths_cancer,
  year == (max(early_deaths_cancer$year) - 10),
  area_name == LOCALITY,
  area_type == "Locality"
)$measure

cancer_deaths_perc_change <- abs(
  (early_deaths_cancer_rate_latest - early_deaths_cancer_rate_earliest) *
    100 /
    early_deaths_cancer_rate_earliest
)

cancer_deaths_changeword <- if_else(
  early_deaths_cancer_rate_latest > early_deaths_cancer_rate_earliest,
  "increase",
  "decrease"
)


##### 2d Hospitalisations from diseases #####

disease_hosp <- dplyr::bind_rows(
  dplyr::filter(asthma_hosp, year == max(year)),
  dplyr::filter(chd_hosp, year == max(year)),
  dplyr::filter(copd_hosp, year == max(year))
) |>
  dplyr::filter(
    (area_name == LOCALITY & area_type == "Locality") |
      (area_name == HSCP & area_type == "HSCP") |
      area_name == HB |
      area_name == "Scotland"
  ) |>
  dplyr::mutate(
    area_type = factor(
      area_type,
      levels = c("Locality", "HSCP", "Health board", "Scotland")
    ),
    area_name = ggplot2::fct_reorder(
      as.factor(area_name),
      as.numeric(area_type)
    )
  ) |>
  dplyr::mutate(
    indicator = dplyr::case_when(
      stringr::str_detect(indicator, stringr::fixed("Asthma")) ~ "Asthma",
      stringr::str_detect(
        indicator,
        stringr::fixed("CHD")
      ) ~ "Coronary Heart Disease",
      stringr::str_detect(indicator, stringr::fixed("COPD")) ~ "COPD"
    )
  ) |>
  dplyr::mutate(measure = phsmethods::round_half_up(measure, 1))

highest_hosp_disease <- disease_hosp |>
  dplyr::filter(
    area_name == LOCALITY,
    area_type == "Locality"
  ) |>
  dplyr::filter(measure == max(measure))

disease_hosp_table <- disease_hosp |>
  dplyr::mutate(
    area_order = dplyr::case_when(
      area_name == LOCALITY ~ 1L,
      area_name == HSCP ~ 2L,
      stringr::str_starts(area_name, stringr::fixed("NHS")) ~ 4L,
      area_name == "Scotland" ~ 5L,
      .default = 2L
    )
  ) |>
  dplyr::arrange(area_order) |>
  dplyr::select(indicator, period_short, area_name, measure) |>
  tidyr::pivot_wider(names_from = area_name, values_from = measure) |>
  dplyr::rename(
    "Disease" = indicator,
    "Latest time period" = period_short
  )


##### 2e Prescriptions for Anxiety, Depression and Psychosis #####

## Time objects
latest_period_adp_presc <- unique(
  dplyr::filter(adp_presc, year == max(adp_presc$year))$period_short
)
prev_period_adp_presc <- unique(
  dplyr::filter(adp_presc, year == max(adp_presc$year) - 10)$period_short
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
adp_presc_bar <- adp_presc |>
  scotpho_bar_chart(
    data = _,
    chart_title = paste0(
      "Anxiety, Depression and Psychosis Prescriptions, ",
      max(adp_presc$period_short)
    ),
    xaxis_title = "Population prescribed medication (%)"
  )


## Numbers for text

adp_presc_latest <- dplyr::filter(
  adp_presc,
  year == max(adp_presc$year),
  area_name == LOCALITY,
  area_type == "Locality"
)$measure

adp_presc_earliest <- dplyr::filter(
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

adp_presc_diff_scot <- if_else(
  adp_presc_latest > scot_adp_presc,
  "larger",
  "smaller"
)


############################ 3) SLF DATA (LTCs) ####################################

###### 3a Waffle Chart Infographic ######

# Set percentage with LTC for infographic
ltc_infographic <- ltc |>
  dplyr::filter(hscp_locality == LOCALITY) |>
  dplyr::filter(total_ltc > 0) |>
  dplyr::group_by(hscp_locality, age_group) |>
  dplyr::summarise(people = sum(people), .groups = "drop") |>
  dplyr::left_join(
    slf_pop_loc,
    by = dplyr::join_by(hscp_locality, age_group)
  ) |>
  dplyr::mutate(
    perc_with_ltc = phsmethods::round_half_up(people / slf_adj_pop, 2)
  )

# objects for each percentage for text + cropping images
ltc.percent.u65 <- dplyr::filter(
  ltc_infographic,
  age_group == "Under 65"
)$perc_with_ltc
ltc.percent.6574 <- dplyr::filter(
  ltc_infographic,
  age_group == "65-74"
)$perc_with_ltc
ltc.percent.7584 <- dplyr::filter(
  ltc_infographic,
  age_group == "75-84"
)$perc_with_ltc
ltc.percent.o85 <- dplyr::filter(
  ltc_infographic,
  age_group == "85+"
)$perc_with_ltc

## Crop images - use raw images from global load

# under65
dm1 <- dim(ppl_bold_u65_raw)
ppl_bold_u65 <- ppl_bold_u65_raw[1:dm1[1], 1:floor(dm1[2] * ltc.percent.u65), ]
dm2 <- dim(ppl_faint_u65_raw)
ppl_faint_u65 <- ppl_faint_u65_raw[
  1:dm2[1],
  ceiling(dm2[2] * ltc.percent.u65):dm2[2],
]

# 65-74
dm1 <- dim(ppl_bold_6574_raw)
ppl_bold_6574 <- ppl_bold_6574_raw[
  1:dm1[1],
  1:floor(dm1[2] * ltc.percent.6574),
]
dm2 <- dim(ppl_faint_6574_raw)
ppl_faint_6574 <- ppl_faint_6574_raw[
  1:dm2[1],
  ceiling(dm2[2] * ltc.percent.6574):dm2[2],
]

# 75-84
dm1 <- dim(ppl_bold_7584_raw)
ppl_bold_7584 <- ppl_bold_7584_raw[
  1:dm1[1],
  1:floor(dm1[2] * ltc.percent.7584),
]
dm2 <- dim(ppl_faint_7584_raw)
ppl_faint_7584 <- ppl_faint_7584_raw[
  1:dm2[1],
  ceiling(dm2[2] * ltc.percent.7584):dm2[2],
]

# over65
dm1 <- dim(ppl_bold_o85_raw)
ppl_bold_o85 <- ppl_bold_o85_raw[1:dm1[1], 1:floor(dm1[2] * ltc.percent.o85), ]
dm2 <- dim(ppl_faint_o85_raw)
ppl_faint_o85 <- ppl_faint_o85_raw[
  1:dm2[1],
  ceiling(dm2[2] * ltc.percent.o85):dm2[2],
]


waffle.u65 <- create_infographic(
  image1 = ppl_faint_u65,
  image2 = ppl_bold_u65,
  perc_ltc = ltc.percent.u65,
  col = palette[1],
  age_label1 = "under 65",
  age_label2 = "UNDER 65"
)

waffle.6574 <- create_infographic(
  image1 = ppl_faint_6574,
  image2 = ppl_bold_6574,
  perc_ltc = ltc.percent.6574,
  col = palette[2],
  age_label1 = "65 to 74",
  age_label2 = "65 - 74"
)

waffle.7584 <- create_infographic(
  image1 = ppl_faint_7584,
  image2 = ppl_bold_7584,
  perc_ltc = ltc.percent.7584,
  col = palette[3],
  age_label1 = "75 to 84",
  age_label2 = "75 - 84"
)

waffle.o85 <- create_infographic(
  image1 = ppl_faint_o85,
  image2 = ppl_bold_o85,
  perc_ltc = ltc.percent.o85,
  col = palette[4],
  age_label1 = "over 85",
  age_label2 = "OVER 85"
)


## Combine images
ltc_waffles <- cowplot::plot_grid(
  waffle.u65,
  waffle.6574,
  waffle.7584,
  waffle.o85,
  nrow = 2
)


## Numbers for text
ltc_percent_total_latest <- (sum(ltc_infographic$people) /
  sum(ltc_infographic$slf_adj_pop)) *
  100


# Remove unnecessary objects
rm(
  ppl_bold_u65,
  ppl_faint_u65,
  ppl_faint_o85,
  ppl_bold_o85,
  ppl_faint_7584,
  ppl_bold_7584,
  ppl_faint_6574,
  ppl_bold_6574,
  ltc.percent.u65,
  ltc.percent.6574,
  ltc.percent.7584,
  ltc.percent.o85,
  dm1,
  dm2,
  waffle.u65,
  waffle.6574,
  waffle.7584,
  waffle.o85
)


###### 3b Multi-morbidity LTC Table ######

## Create df with under 65 vs over 65 - will be used for rest of LTC work
ltc_age_grouped <- ltc |>
  dplyr::select(-year) |>
  dplyr::mutate(
    age_group = if_else(age_group == "Under 65", "Under 65", "65+")
  ) |>
  dplyr::group_by(hscp2019name, hscp_locality, age_group, total_ltc) |>
  dplyr::summarise(dplyr::across(everything(), sum), .groups = "drop")

ltc_multimorbidity <- ltc_age_grouped |>
  dplyr::na.omit(ltc_age_grouped) |>
  dplyr::filter(
    hscp_locality == LOCALITY,
    total_ltc != 0
  ) |>
  dplyr::mutate(
    total_ltc = dplyr::case_when(
      total_ltc == 1 ~ "1 LTC",
      total_ltc == 2 ~ "2 LTCs",
      total_ltc == 3 ~ "3 LTCs",
      total_ltc >= 4 ~ "4 or more LTCs"
    )
  ) |>
  dplyr::mutate(
    total_ltc = factor(
      total_ltc,
      levels = c("1 LTC", "2 LTCs", "3 LTCs", "4 or more LTCs")
    )
  ) |>
  dplyr::group_by(age_group, total_ltc) |>
  dplyr::summarise(people = sum(people), .groups = "drop") |>
  dplyr::mutate(
    ltc_pop = if_else(
      age_group == "Under 65",
      dplyr::filter(slf_pop_loc, age_group == "Under 65")$slf_adj_pop,
      sum(dplyr::filter(slf_pop_loc, age_group != "Under 65")$slf_adj_pop)
    )
  ) |>
  dplyr::group_by(age_group) |>
  dplyr::mutate(
    percent = phsmethods::round_half_up(people / ltc_pop * 100, 1)
  ) |>
  dplyr::ungroup()


ltc_multimorbidity_table <- ltc_multimorbidity |>
  dplyr::select(age_group, total_ltc, percent) |>
  tidyr::pivot_wider(names_from = age_group, values_from = percent) |>
  dplyr::rename(
    " " = total_ltc,
    "Percentage under 65" = "Under 65",
    "Percentage over 65" = "65+"
  )


## Figures for text
ltc_multimorbidity_un65_perc <- sum(
  dplyr::filter(
    ltc_multimorbidity,
    total_ltc != "1 LTC",
    age_group == "Under 65"
  )$percent
)

ltc_multimorbidity_ov65_perc <- sum(
  dplyr::filter(
    ltc_multimorbidity,
    total_ltc != "1 LTC",
    age_group == "65+"
  )$percent
)


# ###### 3c Prevalence of LTC Types ######
ltc_types <- ltc_age_grouped |>
  dplyr::select(-hscp2019name, -total_ltc, -people) |>
  dplyr::filter(hscp_locality == LOCALITY) |>
  dplyr::group_by(hscp_locality, age_group) |>
  dplyr::summarise(dplyr::across(everything(), sum), .groups = "drop") |>
  tidyr::pivot_longer(
    cols = "Arthritis":"Renal failure",
    names_to = "key",
    values_to = "value"
  )

# Create negative values for chart
ltc_types_temp <- ltc_types |>
  dplyr::filter(age_group == "Under 65") |>
  dplyr::mutate(
    percent = (value /
      (dplyr::filter(slf_pop_loc, age_group == "Under 65")$slf_adj_pop) *
      -100)
  )

ltc_types <- ltc_types |>
  dplyr::filter(age_group == "65+") |>
  dplyr::mutate(
    percent = (value /
      sum(dplyr::filter(slf_pop_loc, age_group != "Under 65")$slf_adj_pop) *
      100)
  ) |>
  dplyr::bind_rows(ltc_types_temp)

rm(ltc_types_temp)


#### lollipop with 3 separate plots put together

## create conditionals for expand limits
max_ltc_types_pct <- max(ltc_types$percent)

lims.un65 <- dplyr::case_when(
  max_ltc_types_pct < 20 ~ -10,
  dplyr::between(max_ltc_types_pct, 20, 24) ~ -12,
  max_ltc_types_pct > 24 ~ -15
)
lims.ov65 <- dplyr::case_when(
  max_ltc_types_pct < 20 ~ 20,
  dplyr::between(max_ltc_types_pct, 20, 24) ~ 24,
  max_ltc_types_pct > 24 ~ 30
)

rm(max_ltc_types_pct)

ltc_plot_left <- ltc_types |>
  dplyr::filter(age_group == "Under 65") |>
  ggplot2::ggplot(ggplot2::aes(
    x = percent,
    y = key,
    label = phsmethods::round_half_up(percent, 1)
  )) +
  ggplot2::geom_point(colour = palette[1], size = 3) +
  ggplot2::geom_segment(
    ggplot2::aes(x = 0, y = key, xend = percent, yend = key),
    linewidth = 0.4
  ) +
  ggplot2::labs(
    x = "People under 65 with\nthe condition (%)",
    y = "",
    title = "UNDER 65"
  ) +
  ggplot2::scale_x_continuous(breaks = seq(-100, 0, 2), labels = abs) +
  ggplot2::expand_limits(x = lims.un65) +
  theme_profiles() +
  ggplot2::theme(
    title = ggplot2::element_text(colour = palette[1]),
    plot.margin = ggplot2::unit(c(0.5, 0, 0, 0), "cm"),
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  ) +
  ggplot2::scale_y_discrete(limits = rev(levels(as.factor(ltc_types$key))))

ltc_axis <- ltc_types |>
  dplyr::filter(age_group == "Under 65") |>
  ggplot2::ggplot(ggplot2::aes(x = 0, y = key, label = key)) +
  ggplot2::geom_text() +
  ggplot2::scale_y_discrete(limits = rev(levels(as.factor(ltc_types$key)))) +
  ggplot2::theme_void()

ltc_plot_right <- ltc_types |>
  dplyr::filter(age_group == "65+") |>
  ggplot2::ggplot(ggplot2::aes(
    x = percent,
    y = key,
    label = phsmethods::round_half_up(percent, 1)
  )) +
  ggplot2::geom_point(colour = palette[2], size = 3) +
  ggplot2::geom_segment(
    ggplot2::aes(x = 0, y = key, xend = percent, yend = key),
    linewidth = 0.4
  ) +
  ggplot2::labs(
    x = "People over 65 with\nthe condition (%)",
    y = "",
    title = "OVER 65"
  ) +
  ggplot2::scale_x_continuous(breaks = seq(0, 100, 2)) +
  ggplot2::expand_limits(x = lims.ov65) +
  theme_profiles() +
  ggplot2::theme(
    title = ggplot2::element_text(colour = palette[2]),
    plot.margin = ggplot2::unit(c(0.5, 0, 0, 0), "cm"),
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  ) +
  ggplot2::scale_y_discrete(limits = rev(levels(as.factor(ltc_types$key))))

title <- cowplot::ggdraw() +
  cowplot::draw_label(
    stringr::str_wrap(
      glue::glue(
        "Prevalence estimates for {latest_year_ltc} of Physical Long-Term Conditions in the {LOCALITY} Locality"
      ),
      width = 65
    ),
    size = 11,
    fontface = "bold"
  )

caption <- cowplot::ggdraw() +
  cowplot::draw_label(
    "Source: SPARRA via the Source Linkage Files",
    size = 10,
    hjust = -0.5
  )

# Combine plots into 1
ltc_types_plot <- cowplot::plot_grid(
  title,
  cowplot::plot_grid(
    ltc_plot_left,
    ltc_axis,
    ltc_plot_right,
    ncol = 3,
    align = "h",
    rel_widths = c(0.5, 0.6, 1)
  ),
  caption,
  nrow = 3,
  rel_heights = c(3, 20, 1)
)


rm(
  ltc_plot_left,
  ltc_axis,
  ltc_plot_right,
  title,
  caption,
  lims.ov65,
  lims.un65
)


##### 3d Top LTCs Table #####

# Top 5 locality
top5ltc_loc <- ltc |>
  dplyr::filter(hscp_locality == LOCALITY) |>
  dplyr::select(
    -year,
    -hscp_locality,
    -hscp2019name,
    -total_ltc,
    -slf_adj_pop
  ) |>
  dplyr::group_by(age_group) |>
  dplyr::summarise(dplyr::across(everything(), sum), .groups = "drop") |>
  dplyr::select(-age_group, -people) |>
  dplyr::summarise(dplyr::across(everything(), sum)) |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "topltc",
    values_to = "value"
  ) |>
  dplyr::slice_max(n = 5, order_by = value, with_ties = FALSE) |>
  dplyr::mutate(
    percent = phsmethods::round_half_up((value / ltc_pops_total_loc) * 100, 2)
  ) |>
  dplyr::left_join(ltc_colours_global, by = dplyr::join_by(topltc)) |>
  dplyr::mutate(
    Prevalence = stringr::str_c(topltc, paste(percent, "%"), sep = "\n")
  )


## Create column headers

loc.ltc.table <- stringr::str_wrap(
  glue::glue("{LOCALITY} Locality"),
  width = if_else(n_loc < 5, 30, 25)
)

hscp.ltc.table <- stringr::str_wrap(glue::glue("{HSCP} HSCP"), width = 25)

# Top5 LTC table as a table (instead of an image)
top5_ltc_table <- dplyr::bind_cols(
  dplyr::select(top5ltc_loc, {{ loc.ltc.table }} := Prevalence),
  dplyr::select(top5ltc_hscp, {{ hscp.ltc.table }} := Prevalence),
  dplyr::select(top5ltc_scot, "Scotland" = Prevalence)
) |>
  flextable::flextable() |>
  lp_flextable_theme() |>
  flextable::bg(j = 1, bg = top5ltc_loc$colours) |>
  flextable::bg(j = 2, bg = top5ltc_hscp$colours) |>
  flextable::bg(j = 3, bg = top5ltc_scot$colours) |>
  flextable::font(fontname = "Arial", part = "all") |>
  flextable::color(color = "white", part = "body") |>
  flextable::bold(part = "header") |>
  flextable::border(
    border = officer::fp_border(color = "white", width = 5),
    part = "all"
  )

rm(
  top5ltc_loc,
  loc.ltc.table,
  hscp.ltc.table
)

## Objects for text

ltc_diff_scot <- if_else(
  ltc_percent_total_latest > ltc_perc_scot,
  "higher",
  "lower"
)


############################### 4) CODE FOR SUMMARY TABLE ###############################

## Make GH objects table for other localities in the partnership

other_locs_summary_table_local <- function(data, latest_year) {
  data |>
    dplyr::filter(
      year == latest_year,
      area_type == "Locality"
    ) |>
    dplyr::rename("hscp_locality" = "area_name") |>
    dplyr::right_join(other_locs, by = dplyr::join_by(hscp_locality)) |>
    dplyr::arrange(hscp_locality) |>
    dplyr::select(hscp_locality, measure) |>
    dplyr::mutate(measure = phsmethods::round_half_up(measure, 1)) |>
    tidyr::pivot_wider(names_from = hscp_locality, values_from = measure)
}

# 1. Other localities

# male life expectancy
other_locs_life_exp_male <- other_locs_summary_table_local(
  data = dplyr::filter(life_exp, sex == "Male"),
  latest_year = latest_year_life_exp_loc
)

# female life exp
other_locs_life_exp_fem <- other_locs_summary_table_local(
  data = dplyr::filter(life_exp, sex == "Female"),
  latest_year = latest_year_life_exp_loc
)

## deaths 15-44
other_locs_deaths_15_44 <- other_locs_summary_table_local(
  deaths_15_44,
  latest_year = max(deaths_15_44$year)
)


## Cancer
other_locs_cancer <- other_locs_summary_table_local(
  cancer_reg,
  latest_year = max(cancer_reg$year)
)

## ADP
other_locs_adp <- other_locs_summary_table_local(
  adp_presc,
  latest_year = max(adp_presc$year)
)


## ltc
otherloc_ltc_pops <- slf_pops_all |>
  dplyr::inner_join(other_locs, by = "hscp_locality") |>
  dplyr::group_by(hscp_locality) |>
  dplyr::summarise(slf_adj_pop = sum(slf_adj_pop), .groups = "drop")

other_locs_ltc <- ltc |>
  dplyr::inner_join(
    other_locs,
    by = dplyr::join_by(hscp2019name, hscp_locality)
  ) |>
  dplyr::select(hscp_locality, total_ltc, people) |>
  dplyr::filter(total_ltc >= 1) |>
  dplyr::group_by(hscp_locality) |>
  dplyr::summarise(ltc_people = sum(people), .groups = "drop") |>
  dplyr::left_join(otherloc_ltc_pops, by = "hscp_locality") |>
  dplyr::mutate(
    percent = phsmethods::round_half_up(ltc_people / slf_adj_pop * 100, 1)
  ) |>
  dplyr::arrange(hscp_locality) |>
  dplyr::select(hscp_locality, percent) |>
  tidyr::pivot_wider(names_from = hscp_locality, values_from = percent)

# Housekeeping ----
# These objects are left over after the script is run
# but don't appear to be used in any 'downstream' process:
# Main markdown, Summary Table, Excel data tables, SDC output.
rm(
  disease_hosp,
  early_deaths_cancer_rate_earliest,
  latest_year_life_exp_loc,
  ltc_age_grouped,
  ltc_infographic,
  ltc_multimorbidity,
  ltc_pops_total_loc,
  other_locs,
  other_locs_summary_table_local,
  otherloc_ltc_pops
)
gc()
