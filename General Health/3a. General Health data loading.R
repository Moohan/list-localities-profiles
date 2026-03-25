############################################################################################# .
#                                                                                           #
#                       LOCALITY PROFILES GENERAL HEALTH DATA LOADING                       #
#                                                                                           #
############################################################################################# .

## This script performs global-level data loading and preparation for the General Health
#  chapter of the locality profiles. It should be sourced once at the start of a session.

# Set year of data extracts for folder
ext_year <- 2024

# Set file path for data directory
gen_health_data_dir <- fs::path(lp_path, "General Health", glue::glue("DATA {ext_year}"))

### Import + clean datasets ----

# Life expectancy
life_exp_male <- arrow::read_parquet(fs::path(
  gen_health_data_dir,
  "scotpho_data_extract_life_exp_male.parquet"
)) |>
  clean_scotpho_dat()

life_exp_fem <- arrow::read_parquet(fs::path(
  gen_health_data_dir,
  "scotpho_data_extract_life_exp_fem.parquet"
)) |>
  clean_scotpho_dat()

life_exp <- dplyr::bind_rows(life_exp_male, life_exp_fem) |>
  dplyr::mutate(
    sex = dplyr::case_match(
      indicator,
      "Life expectancy, males" ~ "Male",
      "Life expectancy, females" ~ "Female"
    )
  ) |>
  dplyr::mutate(
    period_short = stringr::str_replace(period, stringr::fixed(" to "), "-") |>
      stringr::str_sub(end = 9)
  )

rm(life_exp_fem, life_exp_male)

## Deaths aged 15-44
deaths_15_44 <- arrow::read_parquet(fs::path(
  gen_health_data_dir,
  "scotpho_data_extract_deaths_15_44.parquet"
)) |>
  clean_scotpho_dat() |>
  dplyr::mutate(period_short = gsub("to", "-", substr(period, 1, 12), fixed = TRUE))

## Cancer registrations
cancer_reg <- arrow::read_parquet(fs::path(
  gen_health_data_dir,
  "scotpho_data_extract_cancer_reg.parquet"
)) |>
  clean_scotpho_dat() |>
  dplyr::mutate(period_short = gsub("to", "-", substr(period, 1, 12), fixed = TRUE))

## Early deaths cancer
early_deaths_cancer <- arrow::read_parquet(fs::path(
  gen_health_data_dir,
  "scotpho_data_extract_early_deaths_cancer.parquet"
)) |>
  clean_scotpho_dat() |>
  dplyr::mutate(period_short = gsub("to", "-", substr(period, 1, 12), fixed = TRUE))

## Asthma hospitalisations
asthma_hosp <- arrow::read_parquet(fs::path(
  gen_health_data_dir,
  "scotpho_data_extract_asthma_hosp.parquet"
)) |>
  clean_scotpho_dat() |>
  dplyr::mutate(period_short = gsub("to", "-", substr(period, 1, 18), fixed = TRUE))

## CHD hospitalisations
chd_hosp <- arrow::read_parquet(fs::path(
  gen_health_data_dir,
  "scotpho_data_extract_chd_hosp.parquet"
)) |>
  clean_scotpho_dat() |>
  dplyr::mutate(period_short = gsub("to", "-", substr(period, 1, 18), fixed = TRUE))

## COPD hospitalisations
copd_hosp <- arrow::read_parquet(fs::path(
  gen_health_data_dir,
  "scotpho_data_extract_copd_hosp.parquet"
)) |>
  clean_scotpho_dat() |>
  dplyr::mutate(period_short = gsub("to", "-", substr(period, 1, 18), fixed = TRUE))

## Anxiety/depression/psychosis prescriptions
adp_presc <- arrow::read_parquet(fs::path(
  gen_health_data_dir,
  "scotpho_data_extract_adp_presc.parquet"
)) |>
  clean_scotpho_dat() |>
  dplyr::mutate(period_short = substr(period, 1, 7))

# Long-term conditions
ltc <- arrow::read_parquet(fs::path(gen_health_data_dir, "LTC_from_SLF.parquet")) |>
  dplyr::rename(
    "Arthritis" = "arth",
    "Asthma" = "asthma",
    "Atrial fibrillation" = "atrialfib",
    "Cancer" = "cancer",
    "Cardiovascular disease" = "cvd",
    "Liver disease" = "liver",
    "COPD*" = "copd",
    "Dementia" = "dementia",
    "Diabetes" = "diabetes",
    "Epilepsy" = "epilepsy",
    "Coronary heart disease" = "chd",
    "Heart failure" = "hefailure",
    "Multiple sclerosis" = "ms",
    "Parkinsons" = "parkinsons",
    "Renal failure" = "refailure"
  ) |>
  dplyr::mutate(
    hscp_locality = gsub("&", "and", hscp_locality, fixed = TRUE),
    year = paste0("20", substr(year, 1, 2), "/", substr(year, 3, 4))
  )

############################### SCOTLAND DATA ####################################

# Extract SLF adjusted populations for all
slf_pops_all <- dplyr::distinct(ltc, age_group, hscp_locality, hscp2019name, slf_adj_pop)
ltc_pops_total_scot <- sum(slf_pops_all$slf_adj_pop)

# Determine year
latest_year_ltc <- ltc[["year"]][1]

## Create Scotland totals
ltc_scot <- ltc |>
  dplyr::select(-year, -hscp2019name, -hscp_locality, -slf_adj_pop) |>
  dplyr::group_by(total_ltc, age_group) |>
  dplyr::summarise(dplyr::across(everything(), sum), .groups = "drop")

ltc_perc_scot <- phsmethods::round_half_up(
  (sum(dplyr::filter(ltc_scot, total_ltc > 0)$people) / ltc_pops_total_scot) * 100,
  1
)

# Scotland-level life expectancy summary
latest_year_life_exp_otherareas <- max(life_exp[["year"]])

scot_life_exp_male <- life_exp |>
  dplyr::filter(
    year == latest_year_life_exp_otherareas,
    area_name == "Scotland",
    area_type == "Scotland",
    sex == "Male"
  ) |>
  dplyr::pull(measure) |>
  phsmethods::round_half_up(digits = 1)

scot_life_exp_fem <- life_exp |>
  dplyr::filter(
    year == latest_year_life_exp_otherareas,
    area_name == "Scotland",
    area_type == "Scotland",
    sex == "Female"
  ) |>
  dplyr::pull(measure) |>
  phsmethods::round_half_up(digits = 1)

scot_deaths_15_44 <- deaths_15_44 |>
  dplyr::filter(
    year == max(deaths_15_44$year),
    area_name == "Scotland",
    area_type == "Scotland"
  ) |>
  dplyr::pull(measure) |>
  phsmethods::round_half_up(digits = 1)

scot_cancer <- cancer_reg |>
  dplyr::filter(
    year == max(cancer_reg$year),
    area_name == "Scotland",
    area_type == "Scotland"
  ) |>
  dplyr::pull(measure) |>
  phsmethods::round_half_up(digits = 1)

scot_cancer_deaths <- early_deaths_cancer |>
  dplyr::filter(
    year == max(early_deaths_cancer$year),
    area_name == "Scotland",
    area_type == "Scotland"
  ) |>
  dplyr::pull(measure) |>
  phsmethods::round_half_up(digits = 1)

scot_adp_presc <- adp_presc |>
  dplyr::filter(
    year == max(adp_presc$year),
    area_name == "Scotland",
    area_type == "Scotland"
  ) |>
  dplyr::pull(measure) |>
  phsmethods::round_half_up(digits = 1)

# Top 5 Scotland LTCs
ltc_totals_scot <- ltc |>
  dplyr::select(-year, -hscp_locality, -hscp2019name, -total_ltc, -slf_adj_pop) |>
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
  dplyr::mutate(percent = phsmethods::round_half_up((value / ltc_pops_total_scot) * 100, 2))

# Global colour lookup for LTC table
palette <- phsstyles::phs_colours(c(
  "phs-purple", "phs-magenta", "phs-blue", "phs-green",
  "phs-graphite", "phs-teal", "phs-liberty", "phs-rust"
))

ltc_colours_global <- ltc_scot |>
  dplyr::select(!dplyr::c(total_ltc, age_group, people)) |>
  dplyr::summarise(dplyr::across(everything(), sum)) |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "topltc",
    values_to = "value"
  ) |>
  dplyr::arrange(dplyr::desc(value)) |>
  dplyr::mutate(
    colours = c(
      palette,
      c(
        "navy", "lightsalmon4", "deeppink4", "forestgreen",
        "steelblue", "purple3", "red4"
      )
    )
  )

top5ltc_scot <- ltc_totals_scot |>
  dplyr::left_join(ltc_colours_global, by = dplyr::join_by(topltc)) |>
  dplyr::mutate(Prevalence = stringr::str_c(topltc, paste(percent, "%"), sep = "\n"))


############################ WAFFLE CHART IMAGES ####################################

# Load images
img_path <- fs::path(lp_path, "General Health", "infographics")

ppl_bold_u65_raw <- png::readPNG(fs::path(img_path, "people bold under 65.png"))
ppl_faint_u65_raw <- png::readPNG(fs::path(img_path, "people faint under 65.png"))
ppl_bold_6574_raw <- png::readPNG(fs::path(img_path, "people bold 65-74.png"))
ppl_faint_6574_raw <- png::readPNG(fs::path(img_path, "people faint 65-74.png"))
ppl_bold_7584_raw <- png::readPNG(fs::path(img_path, "people bold 75-84.png"))
ppl_faint_7584_raw <- png::readPNG(fs::path(img_path, "people faint 75-84.png"))
ppl_bold_o85_raw <- png::readPNG(fs::path(img_path, "people bold over 85.png"))
ppl_faint_o85_raw <- png::readPNG(fs::path(img_path, "people faint over 85.png"))

# LTC infographic waffle chart function
create_infographic <- function(
  image1,
  image2,
  perc_ltc,
  col,
  age_label1,
  age_label2
) {
  ggplot2::ggplot() +
    ggplot2::scale_x_continuous(name = "x") +
    ggplot2::scale_y_continuous(name = "y") +
    ggplot2::geom_rect(
      data = data.frame(x1 = 0, x2 = 1, y1 = 0, y2 = 1.3),
      mapping = ggplot2::aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
      color = "white",
      fill = "white"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.box.background = ggplot2::element_rect(fill = "transparent", colour = NA)
    ) +
    ggplot2::annotation_raster(
      image1,
      ymin = 0.02,
      ymax = 0.99,
      xmin = 0.99 * perc_ltc,
      xmax = 0.99
    ) +
    ggplot2::annotation_raster(
      image2,
      ymin = 0.02,
      ymax = 0.99,
      xmin = 0.01,
      xmax = 0.99 * perc_ltc
    ) +
    ggplot2::coord_fixed(ratio = 0.3) +
    ggplot2::annotate(
      geom = "text",
      x = 0.5,
      y = 0.02,
      size = 3.8,
      label = paste0(
        phsmethods::round_half_up(10 * perc_ltc, 1),
        " in 10 people aged ",
        age_label1,
        " have at least 1 LTC"
      )
    ) +
    ggplot2::annotate(
      geom = "text",
      x = 0.5,
      y = 1.08,
      size = 4,
      colour = col,
      fontface = "bold",
      label = paste0(age_label2, " YEARS OLD")
    )
}
