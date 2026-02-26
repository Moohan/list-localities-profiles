##### 2a Life expectancy #####

# Time objects

latest_year_life_exp_loc <- life_exp |>
  filter(area_type == "Locality") |>
  pull(year) |>
  max()
latest_year_life_exp_otherareas <- max(life_exp[["year"]])

latest_period_life_exp_loc <- life_exp |>
  filter(
    area_type == "Locality",
    year == latest_year_life_exp_loc
  ) |>
  pull(period_short) |>
  unique()
latest_period_life_exp_otherareas <- life_exp |>
  filter(
    area_type == "Scotland",
    year == latest_year_life_exp_otherareas
  ) |>
  pull(period_short) |>
  unique()


# Create time trend
life_exp_trend <- life_exp %>%
  filter(
    area_name == LOCALITY,
    area_type == "Locality",
    year >= max(year) - 10
  ) %>%
  mutate(
    period_short = period_short,
    measure = round_half_up(measure, 1)
  ) %>%
  ggplot(aes(
    x = period_short,
    y = measure,
    group = sex,
    linetype = sex,
    shape = sex,
    colour = sex
  )) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_colour_manual(values = palette) +
  theme_profiles() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  expand_limits(y = 0) +
  labs(
    title = str_wrap(
      glue("Average Life Expectancy in {LOCALITY} locality"),
      width = 65
    ),
    x = "Year Groups (5-year aggregates)",
    y = str_wrap("Average Life Expectancy (in years)", width = 35),
    caption = "Source: ScotPHO"
  ) +
  theme(plot.margin = unit(c(0, 0, 0, 1), "cm")) +
  guides(
    linetype = "none",
    shape = "none",
    colour = guide_legend(
      override.aes = list(shape = c(21, 24), fill = palette[1:2])
    )
  )


# Make a table to compare with other areas

life_exp_table <- life_exp %>%
  filter(
    (year == latest_year_life_exp_loc &
      (area_name == LOCALITY & area_type == "Locality")) |
      year == latest_year_life_exp_otherareas &
        ((area_name == HSCP & area_type == "HSCP") |
          area_name == HB |
          area_name == "Scotland")
  ) %>%
  mutate(
    measure = round_half_up(measure, 1),
    area_type = ordered(
      area_type,
      levels = c("Locality", "HSCP", "Health board", "Scotland")
    )
  ) %>%
  arrange(area_type) %>%
  select("Sex" = sex, area_name, measure) %>%
  pivot_wider(names_from = area_name, values_from = measure)


## Numbers for text

if (LOCALITY %in% check_missing_data_scotpho(life_exp)$area_name) {
  avg_life_exp_latest_male <- NA_real_
  avg_life_exp_latest_fem <- NA_real_
} else {
  avg_life_exp_latest <- filter(
    life_exp,
    year == latest_year_life_exp_loc,
    area_name == LOCALITY,
    area_type == "Locality"
  )

  avg_life_exp_latest_male <- avg_life_exp_latest |>
    filter(sex == "Male") |>
    pull(measure) |>
    round_half_up(digits = 1)
  avg_life_exp_latest_fem <- avg_life_exp_latest |>
    filter(sex == "Female") |>
    pull(measure) |>
    round_half_up(digits = 1)
  rm(avg_life_exp_latest)
}
