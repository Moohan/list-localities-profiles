ppl_bold_u65 <- readPNG(path(
  lp_path,
  "General Health",
  "infographics",
  "people bold under 65.png"
))
ppl_faint_u65 <- readPNG(path(
  lp_path,
  "General Health",
  "infographics",
  "people faint under 65.png"
))
# 65-74
ppl_bold_6574 <- readPNG(path(
  lp_path,
  "General Health",
  "infographics",
  "people bold 65-74.png"
))
ppl_faint_6574 <- readPNG(path(
  lp_path,
  "General Health",
  "infographics",
  "people faint 65-74.png"
))
# 75-84
ppl_bold_7584 <- readPNG(path(
  lp_path,
  "General Health",
  "infographics",
  "people bold 75-84.png"
))
ppl_faint_7584 <- readPNG(path(
  lp_path,
  "General Health",
  "infographics",
  "people faint 75-84.png"
))
# over 85
ppl_bold_o85 <- readPNG(path(
  lp_path,
  "General Health",
  "infographics",
  "people bold over 85.png"
))
ppl_faint_o85 <- readPNG(path(
  lp_path,
  "General Health",
  "infographics",
  "people faint over 85.png"
))

# LTC infographic waffle chart
create_infographic <- function(
  image1,
  image2,
  perc_ltc,
  col,
  age_label1,
  age_label2
) {
  ggplot() +
    scale_x_continuous(name = "x") +
    scale_y_continuous(name = "y") +
    geom_rect(
      data = data.frame(x1 = 0, x2 = 1, y1 = 0, y2 = 1.3),
      mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
      color = "white",
      fill = "white"
    ) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.box.background = element_rect(fill = "transparent", colour = NA)
    ) +
    annotation_raster(
      image1,
      ymin = 0.02,
      ymax = 0.99,
      xmin = 0.99 * perc_ltc,
      xmax = 0.99
    ) +
    annotation_raster(
      image2,
      ymin = 0.02,
      ymax = 0.99,
      xmin = 0.01,
      xmax = 0.99 * perc_ltc
    ) +
    coord_fixed(ratio = 0.3) +
    annotate(
      geom = "text",
      x = 0.5,
      y = 0.02,
      size = 3.8,
      label = paste0(
        round_half_up(10 * perc_ltc, 1),
        " in 10 people aged ",
        age_label1,
        " have at least 1 LTC"
      )
    ) +
    annotate(
      geom = "text",
      x = 0.5,
      y = 1.08,
      size = 4,
      colour = col,
      fontface = "bold",
      label = paste0(age_label2, " YEARS OLD")
    )
}

# Set percentage with LTC for infographic
ltc_infographic <- ltc %>%
  filter(hscp_locality == LOCALITY) %>%
  filter(total_ltc > 0) %>%
  group_by(hscp_locality, age_group) %>%
  summarise(people = sum(people)) %>%
  ungroup() %>%
  left_join(slf_pop_loc, by = join_by(hscp_locality, age_group)) %>%
  mutate(perc_with_ltc = round_half_up(people / slf_adj_pop, 2))

# objects for each percentage for text + cropping images
ltc.percent.u65 <- filter(
  ltc_infographic,
  age_group == "Under 65"
)$perc_with_ltc
ltc.percent.6574 <- filter(ltc_infographic, age_group == "65-74")$perc_with_ltc
ltc.percent.7584 <- filter(ltc_infographic, age_group == "75-84")$perc_with_ltc
ltc.percent.o85 <- filter(ltc_infographic, age_group == "85+")$perc_with_ltc

## Crop images

# under65
dm1 <- dim(ppl_bold_u65)
ppl_bold_u65 <- ppl_bold_u65[1:dm1[1], 1:floor(dm1[2] * ltc.percent.u65), ]
dm2 <- dim(ppl_faint_u65)
ppl_faint_u65 <- ppl_faint_u65[
  1:dm2[1],
  ceiling(dm2[2] * ltc.percent.u65):dm2[2],
]

# 65-74
dm1 <- dim(ppl_bold_6574)
ppl_bold_6574 <- ppl_bold_6574[1:dm1[1], 1:floor(dm1[2] * ltc.percent.6574), ]
dm2 <- dim(ppl_faint_6574)
ppl_faint_6574 <- ppl_faint_6574[
  1:dm2[1],
  ceiling(dm2[2] * ltc.percent.6574):dm2[2],
]

# 75-84
dm1 <- dim(ppl_bold_7584)
ppl_bold_7584 <- ppl_bold_7584[1:dm1[1], 1:floor(dm1[2] * ltc.percent.7584), ]
dm2 <- dim(ppl_faint_7584)
ppl_faint_7584 <- ppl_faint_7584[
  1:dm2[1],
  ceiling(dm2[2] * ltc.percent.7584):dm2[2],
]

# over65
dm1 <- dim(ppl_bold_o85)
ppl_bold_o85 <- ppl_bold_o85[1:dm1[1], 1:floor(dm1[2] * ltc.percent.o85), ]
dm2 <- dim(ppl_faint_o85)
ppl_faint_o85 <- ppl_faint_o85[
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
ltc_waffles <- plot_grid(
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
ltc_age_grouped <- ltc %>%
  select(-year) %>%
  mutate(age_group = if_else(age_group == "Under 65", "Under 65", "65+")) %>%
  group_by(hscp2019name, hscp_locality, age_group, total_ltc) %>%
  summarise(across(everything(), sum)) %>%
  ungroup()

ltc_multimorbidity <- ltc_age_grouped %>%
  na.omit(ltc_age_grouped) %>%
  filter(
    hscp_locality == LOCALITY,
    total_ltc != 0
  ) %>%
  mutate(
    total_ltc = case_when(
      total_ltc == 1 ~ "1 LTC",
      total_ltc == 2 ~ "2 LTCs",
      total_ltc == 3 ~ "3 LTCs",
      total_ltc >= 4 ~ "4 or more LTCs"
    )
  ) %>%
  mutate(
    total_ltc = factor(
      total_ltc,
      levels = c("1 LTC", "2 LTCs", "3 LTCs", "4 or more LTCs")
    )
  ) %>%
  group_by(age_group, total_ltc) %>%
  summarise(people = sum(people)) %>%
  ungroup() %>%
  mutate(
    ltc_pop = if_else(
      age_group == "Under 65",
      filter(slf_pop_loc, age_group == "Under 65")$slf_adj_pop,
      sum(filter(slf_pop_loc, age_group != "Under 65")$slf_adj_pop)
    )
  ) %>%
  group_by(age_group) %>%
  mutate(percent = round_half_up(people / ltc_pop * 100, 1)) %>%
  ungroup()


ltc_multimorbidity_table <- ltc_multimorbidity %>%
  select(age_group, total_ltc, percent) %>%
  pivot_wider(names_from = age_group, values_from = percent) %>%
  rename(
    " " = total_ltc,
    "Percentage under 65" = "Under 65",
    "Percentage over 65" = "65+"
  )


## Figures for text
ltc_multimorbidity_un65_perc <- sum(
  filter(
    ltc_multimorbidity,
    total_ltc != "1 LTC",
    age_group == "Under 65"
  )$percent
)

ltc_multimorbidity_ov65_perc <- sum(
  filter(
    ltc_multimorbidity,
    total_ltc != "1 LTC",
    age_group == "65+"
  )$percent
)


# ###### 3c Prevalence of LTC Types ######
ltc_types <- ltc_age_grouped %>%
  select(-hscp2019name, -total_ltc, -people) %>%
  filter(hscp_locality == LOCALITY) %>%
  group_by(hscp_locality, age_group) %>%
  summarise(across(everything(), sum)) %>%
  ungroup() |>
  pivot_longer(
    cols = "Arthritis":"Renal failure",
    names_to = "key",
    values_to = "value"
  )

# Create negative values for chart
ltc_types_temp <- ltc_types %>%
  filter(age_group == "Under 65") %>%
  mutate(
    percent = (value /
      (filter(slf_pop_loc, age_group == "Under 65")$slf_adj_pop) *
      -100)
  )

ltc_types <- ltc_types %>%
  filter(age_group == "65+") %>%
  mutate(
    percent = (value /
      sum(filter(slf_pop_loc, age_group != "Under 65")$slf_adj_pop) *
      100)
  ) %>%
  bind_rows(ltc_types_temp)

rm(ltc_types_temp)


#### lollipop with 3 separate plots put together

## create conditionals for expand limits
max_ltc_types_pct <- max(ltc_types$percent)

lims.un65 <- case_when(
  max_ltc_types_pct < 20 ~ -10,
  between(max_ltc_types_pct, 20, 24) ~ -12,
  max_ltc_types_pct > 24 ~ -15
)
lims.ov65 <- case_when(
  max_ltc_types_pct < 20 ~ 20,
  between(max_ltc_types_pct, 20, 24) ~ 24,
  max_ltc_types_pct > 24 ~ 30
)

rm(max_ltc_types_pct)

ltc_plot_left <- ltc_types %>%
  filter(age_group == "Under 65") %>%
  ggplot(aes(x = percent, y = key, label = round_half_up(percent, 1))) +
  geom_point(colour = palette[1], size = 3) +
  geom_segment(
    aes(x = 0, y = key, xend = percent, yend = key),
    linewidth = 0.4
  ) +
  labs(
    x = "People under 65 with\nthe condition (%)",
    y = "",
    title = "UNDER 65"
  ) +
  scale_x_continuous(breaks = seq(-100, 0, 2), labels = abs) +
  expand_limits(x = lims.un65) +
  theme_profiles() +
  theme(
    title = element_text(colour = palette[1]),
    plot.margin = unit(c(0.5, 0, 0, 0), "cm"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_y_discrete(limits = rev(levels(as.factor(ltc_types$key))))

ltc_axis <- ltc_types %>%
  filter(age_group == "Under 65") %>%
  ggplot(aes(x = 0, y = key, label = key)) +
  geom_text() +
  scale_y_discrete(limits = rev(levels(as.factor(ltc_types$key)))) +
  theme_void()

ltc_plot_right <- ltc_types %>%
  filter(age_group == "65+") %>%
  ggplot(aes(x = percent, y = key, label = round_half_up(percent, 1))) +
  geom_point(colour = palette[2], size = 3) +
  geom_segment(
    aes(x = 0, y = key, xend = percent, yend = key),
    linewidth = 0.4
  ) +
  labs(
    x = "People over 65 with\nthe condition (%)",
    y = "",
    title = "OVER 65"
  ) +
  scale_x_continuous(breaks = seq(0, 100, 2)) +
  expand_limits(x = lims.ov65) +
  theme_profiles() +
  theme(
    title = element_text(colour = palette[2]),
    plot.margin = unit(c(0.5, 0, 0, 0), "cm"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_y_discrete(limits = rev(levels(as.factor(ltc_types$key))))

title <- ggdraw() +
  draw_label(
    str_wrap(
      glue(
        "Prevalence estimates for {latest_year_ltc} of Physical Long-Term Conditions in the {LOCALITY} Locality"
      ),
      width = 65
    ),
    size = 11,
    fontface = "bold"
  )

caption <- ggdraw() +
  draw_label(
    "Source: SPARRA via the Source Linkage Files",
    size = 10,
    hjust = -0.5
  )

# Combine plots into 1
ltc_types_plot <- plot_grid(
  title,
  plot_grid(
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

# Most common LTC all round
ltc_totals <- ltc_age_grouped |>
  filter(total_ltc != 0) |>
  select(-hscp2019name, -total_ltc, -age_group) |>
  group_by(hscp_locality) |>
  summarise(across(everything(), sum)) |>
  ungroup() |>
  left_join(
    select(lookup, hscp_locality, hscp2019name),
    by = join_by(hscp_locality),
    relationship = "one-to-one"
  )

# Extract population totals to make %
ltc_pops_total_loc <- sum(slf_pop_loc$slf_adj_pop)
ltc_pops_total_scot <- sum(slf_pops$slf_adj_pop)
ltc_pops_total_hscp <- sum(filter(slf_pops, hscp2019name == HSCP)$slf_adj_pop)

# Colour lookup for table
ltc_colours <- ltc_scot |>
  select(!c(total_ltc, age_group, people)) |>
  summarise(across(everything(), sum)) |>
  pivot_longer(
    cols = everything(),
    names_to = "topltc",
    values_to = "value"
  ) |>
  arrange(desc(value)) |>
  mutate(
    colours = c(
      palette,
      c(
        "navy",
        "lightsalmon4",
        "deeppink4",
        "forestgreen",
        "steelblue",
        "purple3",
        "red4"
      )
    )
  )

# Top 5 locality
top5ltc_loc <- ltc_totals |>
  filter(hscp_locality == LOCALITY) |>
  select(-hscp_locality, -hscp2019name, -people, -slf_adj_pop) |>
  pivot_longer(
    cols = everything(),
    names_to = "topltc",
    values_to = "value"
  ) |>
  slice_max(n = 5, order_by = value, with_ties = FALSE) |>
  mutate(percent = round_half_up((value / ltc_pops_total_loc) * 100, 2)) |>
  select(-value) |>
  left_join(ltc_colours, by = join_by(topltc)) |>
  mutate(Prevalence = str_c(topltc, paste(percent, "%"), sep = "\n"))

# Top 5 HSCP
top5ltc_hscp <- ltc_totals |>
  filter(hscp2019name == HSCP) |>
  select(-hscp_locality, -hscp2019name, -people, -slf_adj_pop) |>
  summarise(across(everything(), sum)) |>
  pivot_longer(
    cols = everything(),
    names_to = "topltc",
    values_to = "value"
  ) |>
  slice_max(n = 5, order_by = value, with_ties = FALSE) |>
  mutate(percent = round_half_up((value / ltc_pops_total_hscp) * 100, 2)) |>
  select(-value) |>
  left_join(ltc_colours, by = join_by(topltc)) |>
  mutate(Prevalence = str_c(topltc, paste(percent, "%"), sep = "\n"))


# Top 5 Scotland
top5ltc_scot <- ltc_totals |>
  select(-hscp_locality, -hscp2019name, -people, -slf_adj_pop) |>
  summarise(across(everything(), sum)) |>
  pivot_longer(
    cols = everything(),
    names_to = "topltc",
    values_to = "value"
  ) |>
  slice_max(n = 5, order_by = value, with_ties = FALSE) |>
  mutate(percent = round_half_up((value / ltc_pops_total_scot) * 100, 2)) |>
  select(-value) |>
  left_join(ltc_colours, by = join_by(topltc)) |>
  mutate(Prevalence = str_c(topltc, paste(percent, "%"), sep = "\n"))


## Create column headers

loc.ltc.table <- str_wrap(
  glue("{LOCALITY} Locality"),
  width = if_else(n_loc < 5, 30, 25)
)

hscp.ltc.table <- str_wrap(glue("{HSCP} HSCP"), width = 25)

# Top5 LTC table as a table (instead of an image)
top5_ltc_table <- bind_cols(
  select(top5ltc_loc, {{ loc.ltc.table }} := Prevalence),
  select(top5ltc_hscp, {{ hscp.ltc.table }} := Prevalence),
  select(top5ltc_scot, "Scotland" = Prevalence)
) |>
  flextable(cwidth = 2) |>
  lp_flextable_theme() |>
  bg(j = 1, bg = top5ltc_loc$colours) |>
  bg(j = 2, bg = top5ltc_hscp$colours) |>
  bg(j = 3, bg = top5ltc_scot$colours) |>
  font(fontname = "Arial", part = "all") |>
  color(color = "white", part = "body") |>
  bold(part = "header") |>
  border(border = fp_border(color = "white", width = 5), part = "all")

rm(
  ltc_colours,
  ltc_pops_total_loc,
  loc.ltc.table,
  hscp.ltc.table,
  top5ltc_loc,
  top5ltc_hscp,
  top5ltc_scot
)

## Objects for text

ltc_perc_scot <- round_half_up(
  (sum(filter(ltc_scot, total_ltc > 0)$people) / ltc_pops_total_scot) * 100,
  1
)

ltc_diff_scot <- if_else(
  ltc_percent_total_latest > ltc_perc_scot,
  "higher",
  "lower"
)


############################### 4) CODE FOR SUMMARY TABLE ###############################

## Make GH objects table for hscp, scot AND other localities in the partnership

# Function to get latest data from scotpho

# 1. Other localities

# male life expectancy
other_locs_life_exp_male <- other_locs_summary_table(
  data = filter(life_exp, sex == "Male"),
  latest_year = latest_year_life_exp_loc
)

# female life exp
other_locs_life_exp_fem <- other_locs_summary_table(
  data = filter(life_exp, sex == "Female"),
  latest_year = latest_year_life_exp_loc
)

## deaths 15-44
other_locs_deaths_15_44 <- other_locs_summary_table(
  deaths_15_44,
  latest_year = max(deaths_15_44$year)
)


## Cancer
other_locs_cancer <- other_locs_summary_table(
  cancer_reg,
  latest_year = max(cancer_reg$year)
)

## ADP
other_locs_adp <- other_locs_summary_table(
  adp_presc,
  latest_year = max(adp_presc$year)
)


## ltc
otherloc_ltc_pops <- slf_pops %>%
  inner_join(other_locs, by = "hscp_locality") %>%
  group_by(hscp_locality) %>%
  summarise(slf_adj_pop = sum(slf_adj_pop)) %>%
  ungroup()

other_locs_ltc <- ltc |>
  inner_join(other_locs, by = join_by(hscp2019name, hscp_locality)) %>%
  select(hscp_locality, total_ltc, people) %>%
  filter(total_ltc >= 1) %>%
  group_by(hscp_locality) %>%
  summarise(ltc_people = sum(people)) %>%
  ungroup() %>%
  left_join(otherloc_ltc_pops, by = "hscp_locality") %>%
  mutate(percent = round_half_up(ltc_people / slf_adj_pop * 100, 1)) %>%
  arrange(hscp_locality) %>%
  select(hscp_locality, percent) %>%
  pivot_wider(names_from = hscp_locality, values_from = percent)


# 2. HSCP

if (HSCP == "Clackmannanshire and Stirling") {
  hscp_life_exp_male <- NA_real_
  hscp_life_exp_fem <- NA_real_
} else {
  hscp_life_exp_male <- hscp_scot_summary_table(
    data = filter(life_exp, sex == "Male"),
    latest_year = latest_year_life_exp_otherareas,
    area = HSCP
  )

  hscp_life_exp_fem <- hscp_scot_summary_table(
    data = filter(life_exp, sex == "Female"),
    latest_year = latest_year_life_exp_otherareas,
    area = HSCP
  )
}


hscp_deaths_15_44 <- hscp_scot_summary_table(
  deaths_15_44,
  latest_year = max(deaths_15_44$year),
  area = HSCP
)
hscp_cancer <- hscp_scot_summary_table(
  cancer_reg,
  latest_year = max(cancer_reg$year),
  area = HSCP
)
hscp_adp <- hscp_scot_summary_table(
  adp_presc,
  latest_year = max(adp_presc$year),
  area = HSCP
)

ltc_hscp <- sum(filter(ltc, hscp2019name == HSCP, total_ltc > 0)$people)
hscp_ltc <- round_half_up(ltc_hscp / ltc_pops_total_hscp * 100, 1)

# 3. Scotland

scot_life_exp_male <- hscp_scot_summary_table(
  data = filter(life_exp, sex == "Male"),
  latest_year = latest_year_life_exp_otherareas,
  area = "Scotland"
)

scot_life_exp_fem <- hscp_scot_summary_table(
  data = filter(life_exp, sex == "Female"),
  latest_year = latest_year_life_exp_otherareas,
  area = "Scotland"
)

scot_deaths_15_44 <- hscp_scot_summary_table(
  deaths_15_44,
  latest_year = max(deaths_15_44$year),
  area = "Scotland"
)
scot_cancer <- hscp_scot_summary_table(
  cancer_reg,
  latest_year = max(cancer_reg$year),
  area = "Scotland"
)
scot_cancer_deaths <- hscp_scot_summary_table(
  early_deaths_cancer,
  latest_year = max(early_deaths_cancer$year),
  area = "Scotland"
)
scot_adp_presc <- hscp_scot_summary_table(
  adp_presc,
  latest_year = max(adp_presc$year),
  area = "Scotland"
)

# Housekeeping ----
# These objects are left over after the script is run
# but don't appear to be used in any 'downstream' process:
# Main markdown, Summary Table, Excel data tables, SDC output.
# TODO: Investigate if these can be removed earlier or not created at all.
rm(
  create_infographic,
  disease_hosp,
  early_deaths_cancer_rate_earliest,
  gen_health_data_dir,
  hscp_scot_summary_table,
  latest_year_life_exp_loc,
  ltc_age_grouped,
  ltc_infographic,
  ltc_pops_total_hscp,
  ltc_pops_total_scot,
  ltc_hscp,
  ltc_scot,
  ltc_totals,
  other_locs,
  other_locs_summary_table,
  otherloc_ltc_pops,
  prev_period_cancer_reg,
  slf_pop_loc,
  slf_pops,
  table8_year_title
)
gc()
