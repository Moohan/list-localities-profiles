######################### GLOBAL LOCALITY PROFILES CODE ########################

# Contains various settings and functions to be used in other locality profile scripts

# How to use this script:
# source("Master RMarkdown Document & Render Code/Global Script.R)

## Packages for functions ----
# (** note - should this contain all packages necessary for locality profiles?
# and automatically installing missing packages?)
library(data.table)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(ggmap)
library(sf)
library(stringr)
library(forcats)
library(purrr)
library(janitor)
library(glue)
library(fs)
library(arrow)
library(phsstyles)
library(scales)
library(flextable)
library(officer)
library(memoise)
library(phsmethods)

# Prefer dplyr functions if there's a conflict
conflicted::conflict_prefer_all("dplyr", quiet = TRUE)

lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

#### Colours & Formatting #### ----

## PHS colour palette from phsstyles
palette <- phsstyles::phs_colours(c(
  "phs-purple",
  "phs-magenta",
  "phs-blue",
  "phs-green",
  "phs-graphite",
  "phs-teal",
  "phs-liberty",
  "phs-rust"
))


## Function for formatting a value for R Markdown text
# First determines how many dps the value should have
# Then adds a comma for numbers over 1000 (becomes "1,000")

format_number_for_text <- function(x) {
  x <- ifelse(
    abs(x) < 1,
    round_half_up(x, 2), # if x < 1 then show 2dp
    ifelse(
      abs(x) < 100,
      round_half_up(x, 1), # if 1 =< x < 100 then 1dp
      round_half_up(x)
    )
  ) # if 10 =< x then no decimal places

  format(x, big.mark = ",")
}

# This will return the correct article depending on the (max 2-digit) number supplied
# e.g.
# 81.2 -> an
# 18 -> an
# 7.2 -> an
# To be used for "a xx increase" which could be "an xx increase"
get_article <- function(number) {
  # Cast as a character, so we are sure of the type
  number_chr <- as.character(number)

  if (identical(number_chr, character(0))) {
    # If the number wasn't calculated we still need to deal with it.
    return("-")
  }

  if (startsWith(number_chr, "8") || startsWith(number_chr, "18")) {
    return("an")
  } else {
    return("a")
  }
}

## Theme for charts ----
# This theme is similar to theme_phs() from phsstyles but adapted to locality profiles
# Differences include smaller text (to ensure names of areas always fit regardless of length)
# Code taken from phsstyles Github page
# https://github.com/Public-Health-Scotland/phsstyles/blob/master/R/theme_phs.R

theme_profiles <- function() {
  fontStyle <- "sans"
  gridLineColor <- grDevices::rgb(190 / 255, 190 / 255, 190 / 255)
  fontSize <- 11

  ggplot2::theme(
    # Text format:
    # This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(
      family = fontStyle,
      size = fontSize,
      face = "bold"
    ),

    # Legend format
    # This sets the position and alignment of the legend, removes a title and
    # background for it and sets the requirements for any text within the legend.
    # The legend may often need some more manual tweaking when it comes to its
    # exact position based on the plot coordinates.
    legend.position = "bottom",
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(
      family = fontStyle,
      size = fontSize,
      hjust = 0 # Replaces legend.text.align = 0
    ),

    # Axis format
    # This sets the text font, size and colour for the axis test, as well as
    # setting the margins and removes lines and ticks.
    # In some cases, axis lines and axis ticks are things we would want to have
    # in the chart - the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_text(
      family = fontStyle,
      size = fontSize
    ),
    axis.text = ggplot2::element_text(
      family = fontStyle,
      size = fontSize
    ),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),

    # Grid Lines
    # This removes all minor gridlines and adds major vertical gridlines.
    # In many cases you will want to change this to remove vertical gridlines
    # and add horizontal gridlines. The cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(color = gridLineColor),
    panel.grid.major.y = ggplot2::element_blank(),

    # Blank Background
    # This sets the panel background as blank, removing the standard grey ggplot
    # background colour from the plot
    panel.background = ggplot2::element_blank()
  )
}

# Map background
get_basemap_raw <- function(
  min_long,
  min_lat,
  max_long,
  max_lat,
  stadia_maps_key
) {
  ggmap::register_stadiamaps(
    key = stadia_maps_key
  )

  area <- c(
    left = min_long,
    bottom = min_lat,
    right = max_long,
    top = max_lat
  )

  ggmap::get_stadiamap(
    bbox = area,
    maptype = "alidade_smooth"
  )
}

get_basemap <- memoise::memoise(
  get_basemap_raw,
  cache = cachem::cache_disk(
    dir = fs::path(lp_path, "cache", "basemaps"),
    max_size = 0.5 * 1024^3, # 0.5 GB
    max_age = 365 * 24 * 60 * 60, # 1 year
    evict = "lru"
  )
)

#### Lookup #### ----

## Import the latest locality lookup from cl-out ----
# Argument dz_level: Allows you to choose whether lookup contains all datazones in localities
# default is F - datazones are not imported, there is one line per locality (125 rows)
# if changed to dz_level = TRUE, this shows all the datazones in each locality (6976 rows)

read_in_localities_raw <- function(dz_level = FALSE) {
  localities_lookup <- fs::dir_ls(
    path = "/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality",
    regexp = "HSCP Localities_DZ11_Lookup_.+?\\.rds$"
  ) |>
    # Read in the most up to date lookup version
    max() |>
    readr::read_rds() |>
    dplyr::select(
      datazone2011,
      hscp_locality,
      hscp2019name,
      hscp2019,
      hb2019name,
      hb2019
    ) |>
    dplyr::mutate(hscp_locality = sub("&", "and", hscp_locality, fixed = TRUE))

  if (!dz_level) {
    localities_lookup <- dplyr::distinct(
      localities_lookup,
      hscp_locality,
      hscp2019name,
      hscp2019,
      hb2019name,
      hb2019
    )
  }

  return(localities_lookup)
}
read_in_localities <- memoise(read_in_localities_raw)

count_localities <- function(locality_lookup, hscp_name) {
  return(sum(locality_lookup[["hscp2019name"]] == hscp_name))
}

## Function to read in latest SPD file ----

# No arguments needed, just use read_in_latest_postcodes()
# The function pulls the latest "Scottish_Postcode_Directory_year_version.rds"

read_in_postcodes_raw <- function() {
  sdp_data <- fs::dir_ls(
    path = "/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory",
    regexp = "\\.parquet$"
  ) |>
    # Read in the most up to date lookup version
    max() |>
    arrow::read_parquet(
      col_select = -c(hscp2019, hscp2019name, hb2019, hb2019name)
    )

  spd_with_localities <- dplyr::left_join(
    sdp_data,
    read_in_localities(dz_level = TRUE),
    by = dplyr::join_by(datazone2011),
    relationship = "many-to-one"
  )

  return(spd_with_localities)
}
read_in_postcodes <- memoise(read_in_postcodes_raw)

## Function to read in the latest population file by DZ ----

# No arguments needed, just use read_in_latest_dz_pops()
# Function pulls the latest DZ populations DataZone2011_pop_est_2011_Xyear.rds
# Then joins this with the localities lookup to match hscp_locality

read_in_dz_pops_raw <- function() {
  fs::dir_ls(
    glue(
      "/conf/linkage/output/lookups/Unicode/",
      "Populations/Estimates/"
    ),
    regexp = glue("DataZone2011_pop_est_2011_.+?\\.rds$")
  ) |>
    # Read in the most up-to-date lookup version
    max() |>
    read_rds() |>
    clean_names() |>
    select(
      -c(
        intzone2011,
        intzone2011name,
        ca2019,
        ca2019name,
        ca2018,
        ca2011,
        hscp2019,
        hscp2019name,
        hscp2018,
        hscp2016,
        hb2019,
        hb2019name,
        hb2018,
        hb2014
      )
    ) |>
    left_join(
      read_in_localities(dz_level = TRUE),
      by = join_by(datazone2011)
    ) |>
    mutate(year = as.integer(year))
}
read_in_dz_pops <- memoise(read_in_dz_pops_raw)

###No longer required with new re-based 2011 datazones

#read_in_dz_pops_proxy_year <- function() {
#  read_in_dz_pops() |>
#    filter(year == 2022L) |>
#    select(-year) |>
#    mutate(year = 2023L)
#}

## Function to read in latest population projections ----

# No arguments needed, just use read_in_pop_proj()
# Function pulls the latest projections HSCP2019_pop_proj....rds
# Then joins this with the hscp lookup to match hscp names

read_in_pop_proj_raw <- function() {
  pop_proj <- fs::dir_ls(
    glue(
      "/conf/linkage/output/lookups/Unicode/",
      "Populations/Projections/"
    ),
    regexp = glue("HSCP2019_pop_proj_20.+?_.+?\\.rds$")
  ) |>
    # Read in the most up-to-date lookup version
    max() |>
    read_rds() |>
    clean_names() |>
    select(year, hscp2019, age, sex, sex_name, pop) |>
    mutate(year = as.integer(year))

  # join with lookup so all hscp2019 names are the same
  hscp_lkp <- read_in_localities() |>
    select(hscp2019, hscp2019name) |>
    distinct()

  left_join(pop_proj, hscp_lkp, by = join_by(hscp2019))
}
read_in_pop_proj <- memoise(read_in_pop_proj_raw)

#### Functions for ScotPHO data ####

## ScotPHO data cleaning function ----

# Renames localities so they can match onto lookup
# Removes unwanted areas like council area and IZ

clean_scotpho_dat <- function(data) {
  data |>
    filter(area_type != "Council area", area_type != "Intermediate zone") |>
    mutate(
      area_name = gsub("&", "and", area_name, fixed = TRUE),
      area_name = if_else(
        area_name == "Renfrewshire West",
        "West Renfrewshire",
        area_name
      ),
      area_type = if_else(area_type == "HSC partnership", "HSCP", area_type),
      area_type = if_else(area_type == "HSC locality", "Locality", area_type)
    )
}

## Time trend function for ScotPHO data ----

# Creates a time trend for chosen locality, HSCP, HB and Scotland with confidence interval ribbons
# Data must first be cleaned using clean_scotpho_dat function
# Uses variable "period_short" which must be created - this is a shortened version of the "period" column in ScotPHO data
# ex: mutate(period_short = gsub("to", "-", substr(period, 1, 12)))
# Uses objects "LOCALITY", "HSCP" and "HB" to filter - these must be specified earlier in script

# Arguments:
# data: data to use for chart
# chart_title, xaxis_title, yaxis_title : titles for chart, x axis and y axis
# string_wrap: number of characters after which to "wrap" the x axis labels
# (ScotPHO data uses year aggregates which don't always fit on axis unless wrapped)
# rotate_xaxis: default F, if labels still don't fit even with wrapping (prev argument), labels can be rotated

scotpho_time_trend <- function(
  data,
  chart_title,
  xaxis_title,
  yaxis_title,
  string_wrap,
  rotate_xaxis = FALSE,
  trend_years = 10
) {
  # rotate axis criteria if T/F
  if (rotate_xaxis) {
    rotation <- element_text(angle = 45, hjust = 1)
  } else {
    rotation <- element_text(angle = 0)
  }

  # filter and reorder data
  data |>
    filter(
      (area_name == LOCALITY & area_type == "Locality") |
        (area_name == HSCP & area_type == "HSCP") |
        area_name == HB |
        area_name == "Scotland"
    ) |>
    filter(year >= max(year) - trend_years) |>
    mutate(
      area_type = factor(
        area_type,
        levels = c("Locality", "HSCP", "Health board", "Scotland")
      ),
      area_name = fct_reorder(
        as.factor(str_wrap(area_name, 23)),
        as.numeric(area_type)
      )
    ) |>
    # plot
    ggplot(aes(
      x = str_wrap(period_short, width = string_wrap),
      y = measure,
      group = area_name,
      fill = area_name,
      linetype = area_type
    )) +
    geom_line(aes(colour = area_name), linewidth = 1) +
    geom_point(aes(colour = area_name), size = 2) +
    geom_ribbon(
      aes(
        x = str_wrap(period_short, width = string_wrap),
        ymin = lower_confidence_interval,
        ymax = upper_confidence_interval
      ),
      alpha = 0.1
    ) +
    scale_fill_manual(values = palette) +
    scale_colour_manual(values = palette) +
    scale_y_continuous(labels = scales::comma) +
    theme_profiles() +
    expand_limits(y = 0) +
    labs(
      title = chart_title,
      x = xaxis_title,
      y = yaxis_title,
      caption = "Source: ScotPHO"
    ) +
    theme(axis.text.x = rotation) +
    guides(
      linetype = "none",
      shape = "none",
      fill = "none",
      colour = guide_legend(nrow = 1, byrow = TRUE)
    )
}


scotpho_time_trend_HSCP <- function(
  data,
  chart_title,
  xaxis_title,
  yaxis_title,
  string_wrap,
  rotate_xaxis = FALSE
) {
  # rotate axis criteria if T/F
  if (rotate_xaxis) {
    rotation <- element_text(angle = 45, hjust = 1)
  } else {
    rotation <- element_text(angle = 0)
  }

  # filter and reorder data
  data |>
    filter(
      (area_name == HSCP & area_type == "HSCP") |
        area_name == HB |
        area_name == "Scotland"
    ) |>
    filter(year >= max(year) - 10) |>
    mutate(
      area_type = factor(
        area_type,
        levels = c("HSCP", "Health board", "Scotland")
      ),
      area_name = fct_reorder(
        as.factor(str_wrap(area_name, 23)),
        as.numeric(area_type)
      )
    ) |>
    # plot
    ggplot(aes(
      x = str_wrap(period_short, width = string_wrap),
      y = measure,
      group = area_name,
      fill = area_name,
      linetype = area_type
    )) +
    geom_line(aes(colour = area_name), linewidth = 1) +
    geom_point(aes(colour = area_name), size = 2) +
    geom_ribbon(
      aes(
        x = str_wrap(period_short, width = string_wrap),
        ymin = lower_confidence_interval,
        ymax = upper_confidence_interval
      ),
      alpha = 0.1
    ) +
    scale_fill_manual(values = palette) +
    scale_colour_manual(values = palette) +
    theme_profiles() +
    expand_limits(y = 0) +
    labs(
      title = chart_title,
      x = xaxis_title,
      y = yaxis_title,
      caption = "Source: ScotPHO"
    ) +
    theme(axis.text.x = rotation) +
    guides(
      linetype = "none",
      shape = "none",
      fill = "none",
      colour = guide_legend(nrow = 1, byrow = TRUE)
    )
}

## Bar chart function for ScotPHO data ----

# Creates a horizontal bar chart comparing the last time period of data across
# all localities in a partnership, the HSCP, HB, and Scotland
# Data must first be cleaned using clean_scotpho_dat function
# Uses object "LOCALITY" and vector "other_locs" (other localities in HSCP) to filter
# these must be specified earlier in script

# Arguments:
# data: data to use for chart
# chart_title, xaxis_title : titles for chart and x axis

scotpho_bar_chart <- function(data, chart_title, xaxis_title) {
  data_for_plot <- data |>
    filter(year == max(year)) |>
    filter(
      (area_name %in%
        c(LOCALITY, other_locs$hscp_locality) &
        area_type == "Locality") |
        (area_name == HSCP & area_type == "HSCP") |
        area_name == HB |
        area_name == "Scotland"
    ) |>
    mutate(
      text_highlight = area_name == LOCALITY,
      area_type = factor(
        area_type,
        levels = c("Locality", "HSCP", "Health board", "Scotland")
      ),
      area_name = fct_reorder(as.factor(str_wrap(area_name, 28)), measure)
    ) |>
    arrange(area_name)

  ggplot(data_for_plot) +
    aes(y = area_name, fill = area_type, weight = measure) +
    geom_bar(colour = "white") +
    scale_fill_manual(values = palette) +
    theme_profiles() +
    theme(
      axis.text.y = element_text(
        colour = if_else(data_for_plot$text_highlight, "red", "black"),
        face = if_else(data_for_plot$text_highlight, "bold", "plain")
      )
    ) +
    labs(
      title = chart_title,
      x = xaxis_title,
      y = " ",
      fill = " ",
      caption = "Source: ScotPHO"
    ) +
    geom_errorbar(
      aes(xmin = lower_confidence_interval, xmax = upper_confidence_interval),
      width = 0.2,
      position = position_dodge(width = 1)
    )
}


scotpho_bar_chart_HSCP <- function(data, chart_title, xaxis_title) {
  data_for_plot <- data |>
    filter(year == max(year)) |>
    filter(
      (area_name %in% c(other_locs$hscp_locality) & area_type == "Locality") |
        (area_name == HSCP & area_type == "HSCP") |
        area_name == HB |
        area_name == "Scotland"
    ) |>
    mutate(
      text_highlight = area_name == HSCP,
      area_type = factor(
        area_type,
        levels = c("Locality", "HSCP", "Health board", "Scotland")
      ),
      area_name = fct_reorder(as.factor(str_wrap(area_name, 28)), measure)
    ) |>
    arrange(area_name)

  ggplot(data_for_plot) +
    aes(y = area_name, fill = area_type, weight = measure) +
    geom_bar(colour = "white") +
    scale_fill_manual(values = palette) +
    theme_profiles() +
    theme(
      axis.text.y = element_text(
        colour = if_else(data_for_plot$text_highlight, "red", "black")
      )
    ) +
    labs(
      title = chart_title,
      x = xaxis_title,
      y = " ",
      fill = " ",
      caption = "Source: ScotPHO"
    ) +
    geom_errorbar(
      aes(xmin = lower_confidence_interval, xmax = upper_confidence_interval),
      width = 0.2,
      position = position_dodge(width = 1)
    )
}

## Checking for missing data
check_missing_data_scotpho <- function(data) {
  data |>
    filter(area_type == "Locality") |>
    filter(year == max(year)) |>
    right_join(read_in_localities(), by = c("area_name" = "hscp_locality")) |>
    filter(is.na(indicator)) |>
    select(area_name, hscp2019name)
}


### Unscheduled care functions - can be used across other topics ### ----

# Reformat age groups to specific strings shown i.e. add spaces
age_group_1 <- function(age_group) {
  dplyr::case_match(
    age_group,
    c("<18", "0-17") ~ "0 - 17",
    c("18-24", "25-29", "30-34", "35-39", "40-44") ~ "18 - 44",
    c("45-49", "50-54", "55-59", "60-64") ~ "45 - 64",
    c("65-69", "70-74") ~ "65 - 74",
    c("75-79", "80-84", "85-89", "90-94", "95-99", "Over 100", "100+") ~ "75+",
    .default = "NA"
  )
}

# Bin ages in the required size for unscheduled care indicators
age_group_2 <- function(age) {
  dplyr::case_when(
    dplyr::between(age, 0, 17) ~ "0 - 17",
    dplyr::between(age, 18, 44) ~ "18 - 44",
    dplyr::between(age, 45, 64) ~ "45 - 64",
    dplyr::between(age, 65, 74) ~ "65 - 74",
    age >= 75 ~ "75+"
  )
}

# reformat partnership names # 1

ptsp <- function(partnership) {
  dplyr::case_match(
    partnership,
    "Borders" ~ "Scottish Borders",
    "Orkney" ~ "Orkney Islands",
    "Shetland" ~ "Shetland Islands",
    "Edinburgh City" ~ "Edinburgh",
    "City of Edinburgh" ~ "Edinburgh",
    "Perth & Kinross" ~ "Perth and Kinross",
    "Clackmannanshire" ~ "Clackmannanshire and Stirling",
    "Stirling" ~ "Clackmannanshire and Stirling",
    "Na h-Eileanan Siar" ~ "Western Isles",
    "Comhairle nan Eilean Siar" ~ "Western Isles",
    .default = partnership
  )
}

hbres <- function(hbres_currentdate) {
  dplyr::case_match(
    hbres_currentdate,
    "S08000015" ~ "NHS Ayrshire & Arran",
    "S08000016" ~ "NHS Borders",
    "S08000017" ~ "NHS Dumfries & Galloway",
    "S08000029" ~ "NHS Fife",
    "S08000019" ~ "NHS Forth Valley",
    "S08000020" ~ "NHS Grampian",
    "S08000031" ~ "NHS Greater Glasgow & Clyde",
    "S08000022" ~ "NHS Highland",
    "S08000032" ~ "NHS Lanarkshire",
    "S08000024" ~ "NHS Lothian",
    "S08000025" ~ "NHS Orkney",
    "S08000026" ~ "NHS Shetland",
    "S08000030" ~ "NHS Tayside",
    "S08000028" ~ "NHS Western Isles",
    .default = "Other"
  )
}

# Define a function to save multiple dataframes to an Excel workbook
save_dataframes_to_excel <- function(dataframes, sheet_names, file_path) {
  # Create a new workbook using openxlsx2
  wb <- openxlsx2::wb_workbook()

  # Loop over each dataframe and corresponding sheet name
  for (i in seq_along(dataframes)) {
    # Define the used columns
    cols_to_read <- seq_len(ncol(dataframes[[i]]))

    # Define the header range
    header_range <- openxlsx2::wb_dims(rows = 1, cols = cols_to_read)

    wb <- wb |>
      # Add a worksheet
      openxlsx2::wb_add_worksheet(sheet = sheet_names[[i]]) |>
      # Write data
      openxlsx2::wb_add_data(x = dataframes[[i]]) |>
      # Style the header bold
      openxlsx2::wb_add_font(dims = header_range, bold = TRUE) |>
      # Set column widths to auto
      openxlsx2::wb_set_col_widths(cols = cols_to_read, widths = "auto")
  }

  # Create the directories if they don't exist
  fs::dir_create(fs::path_dir(file_path), mode = "u=rwx,g=rwx,o=rx")

  # Save the workbook to a file
  openxlsx2::wb_save(wb, file = file_path, overwrite = TRUE)
}


# flextable function

lp_flextable_theme <- function(ft) {
  ft %>%
    bold(part = "header") %>%
    bg(bg = "#43358B", part = "header") %>%
    color(color = "white", part = "header") %>%
    height(height = 0.236, part = "body") %>%
    hrule(rule = "atleast", part = "body") %>%
    align(align = "center", part = "header") %>%
    valign(valign = "center", part = "header") %>%
    valign(valign = "top", part = "body") %>%
    colformat_num(big.mark = "") %>%
    fontsize(size = 10, part = "all") %>%
    border(
      border = fp_border_default(color = "#000000", width = 0.5),
      part = "all"
    )
}

add_cover_page <- function(
  document_path,
  cover_page_path,
  main_title,
  date = format(Sys.Date(), "%d/%m/%Y"),
  subtitle = ""
) {
  # Load and update cover page
  cover_page <- cover_page_path |>
    officer::read_docx() |>
    officer::body_replace_all_text("Publication title", main_title) |>
    officer::body_replace_all_text("Subtitle", subtitle) |>
    officer::body_replace_all_text("DD Month YYYY", date)

  # Extract document name and folder
  document_name <- fs::path_file(document_path)
  document_folder <- fs::path_dir(document_path)

  # Sanitize and apply custom replacements
  esc_char_document_name <- document_name |>
    fs::path_sanitize() |>
    stringr::str_replace_all("[&\\- ]", "_")

  # Build new path and rename original file
  esc_char_document_path <- fs::path(document_folder, esc_char_document_name)
  fs::file_move(document_path, esc_char_document_path)

  # Convert document to XML for merging
  xml_elt <- esc_char_document_path |>
    officer::block_pour_docx() |>
    officer::to_wml(add_ns = TRUE)

  # Ensure XML references match sanitized path
  if (stringr::str_detect(xml_elt, esc_char_document_path)) {
    xml_elt <- stringr::str_replace_all(
      xml_elt,
      stringr::fixed(esc_char_document_path),
      esc_char_document_path
    )
  }

  # Merge cover page and report
  cover_page |>
    officer::cursor_end() |>
    officer::body_add_break() |>
    officer::body_add_xml(str = xml_elt) |>
    officer::set_doc_properties(title = main_title) |>
    print(esc_char_document_path)

  # Restore original filename
  fs::file_move(esc_char_document_path, document_path)
}


create_testing_chapter <- function(chapters_oi, locality_oi, output_directory) {
  output_dir <- output_directory

  LOCALITY <- locality_oi

  lookup <- read_in_localities()

  if ("Summary-Table.Rmd" %in% chapters_oi) {
    # Demographics ----
    source("Demographics/1. Demographics - Population.R")
    source("Demographics/2. Demographics - SIMD.R")

    # Housing ----
    source("Households/Households Code.R")

    # Services ----
    source("Services/2. Services data manipulation & table.R")
    source("Services/3. Service HSCP map.R")

    # Population Health ----
    source("Population Health/3. Population Health Outputs.R")

    # Lifestyle & Risk Factors ----
    source("Lifestyle & Risk Factors/2. Lifestyle & Risk Factors Outputs.R")

    # Unscheduled Care ----
    source("Unscheduled Care/2. Unscheduled Care outputs.R")
  } else {
    if ("Demographics.Rmd" %in% chapters_oi) {
      # Demographics ----
      source("Demographics/1. Demographics - Population.R")
      source("Demographics/2. Demographics - SIMD.R")
    }

    if ("Housing.Rmd" %in% chapters_oi) {
      # Housing ----
      source("Households/Households Code.R")
    }

    if ("Services.Rmd" %in% chapters_oi) {
      # Services ----
      source("Services/2. Services data manipulation & table.R")
      source("Services/3. Service HSCP map.R")
    }

    if ("Population-Health.Rmd" %in% chapters_oi) {
      # Population Health ----
      source("Population Health/3. Population Health Outputs.R")
    }

    if ("Lifestyle-Risk-Factors.Rmd" %in% chapters_oi) {
      # Lifestyle & Risk Factors ----
      source("Lifestyle & Risk Factors/2. Lifestyle & Risk Factors Outputs.R")
    }

    if ("Unscheduled-Care.Rmd" %in% chapters_oi) {
      # Unscheduled Care ----
      source("Unscheduled Care/2. Unscheduled Care outputs.R")
    }
  }

  # Appendices ----
  source("Master RMarkdown Document & Render Code/Tables for Appendix.R")

  chapters_oi_name <- chapters_oi %>%
    gsub(".Rmd", "", .) %>%
    paste(collapse = " ")

  # read _bookdown.yaml file
  yaml_file <- yaml::read_yaml("lp_bookdown/_bookdown.yaml")

  # change included chapters to relevant chapter(s) only + index.Rmd (sets formatting)
  yaml_file$rmd_files <- unique(c("index.Rmd", chapters_oi, "Appendix.Rmd"))

  # write temporary yaml with relevant chapters to be used in rendering
  yaml::write_yaml(yaml_file, path(tempdir(), "_practice_chapter_temp.yaml"))

  output_doc_name <- glue(
    "{LOCALITY} - Locality Profile {chapters_oi_name} Practice Chapter.docx"
  )

  # render test chapter
  bookdown::render_book(
    input = "lp_bookdown",
    output_dir = output_dir,
    output_file = output_doc_name,
    new_session = FALSE,
    output_format = "bookdown::word_document2",
    config_file = path(tempdir(), "_practice_chapter_temp.yaml")
  )

  document_path <- path(output_dir, output_doc_name)

  if ("Summary-Table.Rmd" %in% chapters_oi) {
    orient(document_path)
  }

  return(document_path)
}


orient <- function(document_path) {
  doc <- officer::read_docx(document_path) |>
    officer::cursor_begin()

  # Define sections: each with a start keyword and orientation
  sections <- list(
    list(keyword = "{{contents_anchor}}", orientation = "portrait"),
    list(keyword = "{{summary_anchor}}", orientation = "landscape"),
    list(keyword = "{{main_anchor}}", orientation = "portrait")
  )

  # Loop through sections and apply orientation
  for (sec in sections) {
    # Move cursor to the start of this section
    doc <- officer::cursor_reach(doc, keyword = sec$keyword, fixed = TRUE)

    # Apply section break + orientation
    if (sec$orientation == "landscape") {
      doc <- officer::body_end_section_landscape(doc)
    } else {
      doc <- officer::body_end_section_portrait(doc)
    }

    # Remove anchors
    # Move cursor back to the anchor keyword
    doc <- officer::cursor_reach(doc, keyword = sec$keyword, fixed = TRUE)

    # remove anchors
    doc <- officer::body_replace_all_text(
      doc,
      old_value = sec$keyword,
      new_value = "",
      fixed = TRUE
    )
  }

  print(doc, target = document_path)
}


#### Word Locality Profile generation #### ----

profile_output_filename <- function(locality) {
  if (
    !is.character(locality) ||
      length(locality) != 1L ||
      is.na(locality) ||
      !nzchar(trimws(locality))
  ) {
    stop("A single non-empty locality name is required.", call. = FALSE)
  }

  # Preserve the naming convention used by Build Profiles.R.
  paste0(gsub("[/\\\\]", "-", locality), " - Locality Profile.docx")
}

validate_profile_selection <- function(localities, lookup) {
  if (
    !is.character(localities) ||
      length(localities) == 0L ||
      anyNA(localities) ||
      any(!nzchar(trimws(localities)))
  ) {
    stop("localities must be a non-empty character vector.", call. = FALSE)
  }
  if (anyDuplicated(localities)) {
    stop("localities must not contain duplicates.", call. = FALSE)
  }
  if (
    !is.data.frame(lookup) ||
      !all(c("hscp_locality", "hscp2019name") %in% names(lookup))
  ) {
    stop("lookup must contain hscp_locality and hscp2019name.", call. = FALSE)
  }

  areas <- unique(lookup[c("hscp_locality", "hscp2019name")])
  matches <- areas[areas$hscp_locality %in% localities, , drop = FALSE]
  counts <- table(factor(matches$hscp_locality, levels = localities))

  if (any(counts == 0L)) {
    stop(
      "Unknown localities: ",
      paste(localities[counts == 0L], collapse = ", "),
      call. = FALSE
    )
  }
  if (any(counts > 1L)) {
    stop(
      "Ambiguous locality names (more than one HSCP): ",
      paste(localities[counts > 1L], collapse = ", "),
      call. = FALSE
    )
  }

  matches <- matches[match(localities, matches$hscp_locality), , drop = FALSE]
  filenames <- purrr::map_chr(localities, profile_output_filename)
  if (anyDuplicated(filenames)) {
    stop(
      "Selected localities produce duplicate output filenames.",
      call. = FALSE
    )
  }

  data.frame(
    locality = localities,
    hscp = matches$hscp2019name,
    output_filename = filenames,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# Process every selected locality in order, collecting errors without stopping.
# progress(event) reports locality, hscp, position, total, status and any error.
run_profile_items <- function(
  items,
  processor,
  progress = function(event) NULL
) {
  stopifnot(is.data.frame(items), is.function(processor), is.function(progress))
  stopifnot(all(c("locality", "hscp", "output_filename") %in% names(items)))

  # purrr::safely() returns a result/error pair for each locality.
  safe_processor <- purrr::safely(function(item) {
    output_path <- processor(item)
    if (
      !is.character(output_path) ||
        length(output_path) != 1L ||
        is.na(output_path) ||
        !nzchar(output_path)
    ) {
      stop("Profile processor did not return an output path.")
    }
    output_path
  })

  results <- purrr::map(seq_len(nrow(items)), function(i) {
    event <- list(
      locality = items$locality[[i]],
      hscp = items$hscp[[i]],
      position = i,
      total = nrow(items),
      status = "running",
      output_path = NA_character_,
      error = NA_character_
    )
    progress(event)

    outcome <- safe_processor(items[i, , drop = FALSE])
    event$status <- if (is.null(outcome$error)) "succeeded" else "failed"
    event$output_path <- if (is.null(outcome$error)) {
      outcome$result
    } else {
      NA_character_
    }
    event$error <- if (is.null(outcome$error)) {
      NA_character_
    } else {
      conditionMessage(outcome$error)
    }
    progress(event)
    invisible(gc(verbose = FALSE))

    data.frame(
      locality = event$locality,
      hscp = event$hscp,
      status = event$status,
      output_path = event$output_path,
      error = event$error
    )
  }) |>
    purrr::list_rbind()

  list(
    total = nrow(results),
    succeeded = sum(results$status == "succeeded"),
    failed = sum(results$status == "failed"),
    results = results
  )
}

# Stage in the destination directory so a rename to the final path stays on
# the same filesystem. On the Linux server, file.rename() replaces an existing
# file atomically; if renaming fails, the old file is left untouched.
publish_profile_document <- function(
  source_path,
  destination_path,
  group = "LIST_analytics"
) {
  source_info <- file.info(source_path)
  if (
    !file.exists(source_path) ||
      is.na(source_info$size) ||
      source_info$size == 0L
  ) {
    stop("Completed Word document is missing or empty.", call. = FALSE)
  }

  destination_dir <- dirname(destination_path)
  if (
    !dir.exists(destination_dir) &&
      !dir.create(destination_dir, recursive = TRUE, showWarnings = FALSE)
  ) {
    stop("Cannot create output directory: ", destination_dir, call. = FALSE)
  }

  staged_path <- tempfile(
    pattern = ".lp-profile-stage-",
    tmpdir = destination_dir,
    fileext = ".docx"
  )
  on.exit(unlink(staged_path), add = TRUE)

  if (!file.copy(source_path, staged_path, overwrite = FALSE)) {
    stop("Could not stage Word document in output directory.", call. = FALSE)
  }

  # Apply the ownership before publication, while the old file is untouched.
  if (!is.null(group)) {
    fs::file_chown(staged_path, group_id = group)
  }

  if (!file.rename(staged_path, destination_path)) {
    stop(
      "Could not publish Word document; existing output was preserved.",
      call. = FALSE
    )
  }

  invisible(destination_path)
}

build_profiles <- function(
  localities,
  output_dir,
  progress = function(event) NULL,
  project_root = "."
) {
  if (
    !is.character(output_dir) ||
      length(output_dir) != 1L ||
      is.na(output_dir) ||
      !nzchar(output_dir)
  ) {
    stop("output_dir must be a single directory path.", call. = FALSE)
  }
  if (!is.function(progress)) {
    stop("progress must be a function accepting an event list.", call. = FALSE)
  }

  # Relative source paths and bookdown inputs are resolved at the project root.
  project_root <- normalizePath(project_root, mustWork = TRUE)
  old_wd <- getwd()
  setwd(project_root)
  on.exit(setwd(old_wd), add = TRUE)

  rlang::check_installed("phstemplates", version = "1.3.0")
  library(knitr)
  library(bookdown)
  library(phstemplates)

  # Reuse functions and memoised readers from Global Script.R.
  # Each locality gets its own child environment to prevent stale indicators.
  shared_env <- new.env(parent = environment(build_profiles))
  lookup <- read_in_localities()
  items <- validate_profile_selection(localities, lookup)
  output_dir <- normalizePath(output_dir, mustWork = FALSE)

  # Retain variables expected by the existing analysis and RMarkdown scripts.
  shared_env$lookup <- lookup
  shared_env$output_dir <- output_dir
  shared_env$hscp_list <- unique(items$hscp)

  job_root <- tempfile(pattern = "lp-profile-build-")
  dir.create(job_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(job_root, recursive = TRUE, force = TRUE), add = TRUE)

  input_dir <- file.path(job_root, "inputs")
  doc_dir <- file.path(job_root, "docs")
  dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)

  local_lp_bookdown <- file.path(input_dir, "lp_bookdown")
  fs::dir_copy("lp_bookdown", local_lp_bookdown)

  cover_page <- file.path(
    shared_env$lp_path,
    "templates",
    "phs-mngtinfo-coverV2.docx"
  )
  local_cover_page <- file.path(input_dir, "phs-mngtinfo-coverV2.docx")
  if (!file.copy(cover_page, local_cover_page, overwrite = TRUE)) {
    stop("Could not copy the Word cover-page template.", call. = FALSE)
  }

  old_cfg <- httr::set_config(httr::config(connecttimeout = 15, timeout = 90))
  on.exit(httr::set_config(old_cfg, override = TRUE), add = TRUE)

  source_scripts <- c(
    "Demographics/1. Demographics - Population.R",
    "Demographics/2. Demographics - SIMD.R",
    "Households/Households Code.R",
    "Services/2. Services data manipulation & table.R",
    "Services/3. Service HSCP map.R",
    "Population Health/3. Population Health Outputs.R",
    "Lifestyle & Risk Factors/2. Lifestyle & Risk Factors Outputs.R",
    "Unscheduled Care/2. Unscheduled Care outputs.R",
    "Master RMarkdown Document & Render Code/Tables for Appendix.R"
  )

  process_locality <- function(item) {
    # A new environment replaces the old rm(list = setdiff(...)) cleanup.
    analysis_env <- new.env(parent = shared_env)
    analysis_env$HSCP <- item$hscp[[1]]
    analysis_env$LOCALITY <- item$locality[[1]]

    # One existing section script sources another with source()'s default
    # local = FALSE. Keep nested sources inside this locality's environment
    # rather than allowing them to write into .GlobalEnv.
    analysis_env$source <- function(file, local = FALSE, ...) {
      base::source(file, local = analysis_env, ...)
    }

    # Remove previous output artefacts without removing memoised source data.
    unlink(doc_dir, recursive = TRUE)
    dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)

    purrr::walk(source_scripts, function(script) {
      sys.source(script, envir = analysis_env)
    })

    output_name <- item$output_filename[[1]]
    main_title <- sub("\\.docx$", "", output_name)
    temp_document <- file.path(doc_dir, output_name)
    final_document <- file.path(output_dir, output_name)

    bookdown::render_book(
      input = local_lp_bookdown,
      output_dir = doc_dir,
      output_file = output_name,
      new_session = FALSE,
      output_format = "bookdown::word_document2",
      config_file = "_bookdown.yaml",
      envir = analysis_env
    )

    shared_env$orient(temp_document)
    shared_env$add_cover_page(temp_document, local_cover_page, main_title)
    apply_sensitivity_label(temp_document, "OFFICIAL_SENSITIVE_VMO")

    publish_profile_document(temp_document, final_document)
  }

  run_profile_items(items, process_locality, progress)
}
