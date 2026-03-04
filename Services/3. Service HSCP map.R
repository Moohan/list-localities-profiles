# LOCALITY PROFILES SERVICES MAP CODE
# Code for creating the HSCP services map for the locality profiles

# 1. Read in locality shape files ----

shp <- sf::read_sf(
  "/conf/linkage/output/lookups/Unicode/Geography/Shapefiles/HSCP Locality (Datazone2011 Base)/HSCP_Locality.shp"
)
shp <- sf::st_transform(shp, 4326) |>
  dplyr::select(hscp_local, HSCP_name, Shape_Leng, Shape_Area, geometry)

shp <- shp |>
  dplyr::mutate(hscp_locality = gsub("&", "and", hscp_local, fixed = TRUE)) |>
  merge(lookup2, by = "hscp_locality")

shp_hscp <- shp |>
  dplyr::filter(hscp2019name == HSCP) |>
  dplyr::mutate(
    hscp_locality = stringr::str_wrap(hscp_locality, 24),
    hscp_local = stringr::str_wrap(hscp_local, 24)
  )

# 2. Map Code ----
# 2.1 Palettes ----

# Create colour palettes for different numbers of localities
if (n_loc < 5) {
  col_palette <- c("#3F3685", "#9B4393", "#0078D4", "#83BB26")
} else if (n_loc %in% c(5, 6)) {
  col_palette <- c(
    "#3F3685",
    "#9B4393",
    "#0078D4",
    "#83BB26",
    "#948DA3",
    "#1E7F84"
  )
} else if (n_loc == 7) {
  col_palette <- c(
    "#3F3685",
    "#9B4393",
    "#0078D4",
    "#83BB26",
    "#948DA3",
    "#1E7F84",
    "#6B5C85"
  )
} else if (n_loc == 8) {
  col_palette <- c(
    "#3F3685",
    "#9B4393",
    "#0078D4",
    "#83BB26",
    "#948DA3",
    "#1E7F84",
    "#6B5C85",
    "#C73918"
  )
} else if (n_loc == 9) {
  col_palette <- c(
    "#3F3685",
    "#9B4393",
    "#0078D4",
    "#83BB26",
    "#948DA3",
    "#1E7F84",
    "#6B5C85",
    "#C73918",
    "orchid3"
  )
}

# 2.2 Locality shapes ----
# Get latitude and longitude coordinates for each data locality, find min and max.
zones_coord <- shp_hscp |>
  sf::st_coordinates() |>
  tibble::as_tibble() |>
  dplyr::select("long" = X, "lat" = Y) |>
  dplyr::summarise(
    min_long = min(long),
    max_long = max(long),
    min_lat = min(lat),
    max_lat = max(lat)
  )

# Get min and max longitude for locality, add a 0.01 extra to add a border to map.
min_long <- zones_coord$min_long - 0.01
max_long <- zones_coord$max_long + 0.01
min_lat <- zones_coord$min_lat - 0.01
max_lat <- zones_coord$max_lat + 0.01

# get data zones in HSCP
hscp_loc <- readr::read_csv(
  "/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality/HSCP Localities_DZ11_Lookup_20240513.csv"
) |>
  dplyr::select(datazone2011, hscp2019name) |>
  dplyr::filter(hscp2019name == HSCP)

# get place names of cities, towns and villages within locality
places <- readr::read_csv(paste0(
  "/conf/linkage/output/lookups/Unicode/Geography/",
  "Shapefiles/Scottish Places/Places to Data Zone Lookup.csv"
)) |>
  dplyr::rename(datazone2011 = DataZone) |>
  dplyr::filter(datazone2011 %in% hscp_loc$datazone2011) |>
  # extra filter to remove place names with coordinates outwith locality
  dplyr::filter(
    Longitude >= min_long &
      Longitude <= max_long &
      Latitude >= min_lat &
      Latitude <= max_lat
  ) |>
  dplyr::group_by(name) |>
  dplyr::summarise(
    Longitude = dplyr::first(Longitude),
    Latitude = dplyr::first(Latitude),
    type = dplyr::first(type)
  ) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326) |>
  dplyr::filter(!grepl("_", name, fixed = TRUE)) |> # filter incorrect name types
  dplyr::filter(type != "hamlet" & type != "village") # remove smaller places

# 2.3 Background map ----
locality_map_id <- readr::read_csv(
  paste0(lp_path, "Services/", "locality_map_id.csv")
)
api_key <- locality_map_id$id
# upload map background from stadia maps, enter registration key, filter for max and min long/lat
ggmap::register_stadiamaps(key = api_key)
service_map_background <- ggmap::get_stadiamap(
  bbox = c(
    min_long,
    min_lat,
    max_long,
    max_lat
  ),
  maptype = "stamen_terrain_background"
)

# 2.4 Map markers ----
# add locality polygons and service markers to map where services are located
service_map <- ggmap::ggmap(service_map_background) +
  sf::geom_sf(
    data = shp_hscp,
    mapping = ggplot2::aes(
      fill = hscp_local
    ),
    colour = "black",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  ggplot2::labs(fill = "Locality")

# check if services markers exist for locality
if (nrow(markers_gp) > 0) {
  service_map <- service_map +
    ggplot2::geom_point(
      data = markers_gp,
      mapping = ggplot2::aes(
        x = longitude,
        y = latitude,
        colour = "GP Practice"
      ),
      size = 2,
      shape = 21,
      stroke = 0.5,
      fill = "red"
    )
}
if (nrow(markers_care_home) > 0) {
  service_map <- service_map +
    ggplot2::geom_point(
      data = markers_care_home,
      mapping = ggplot2::aes(
        x = longitude,
        y = latitude,
        colour = "Care Home"
      ),
      size = 2,
      shape = 22,
      stroke = 0.5,
      fill = "yellow"
    )
}
if (nrow(markers_emergency_dep) > 0) {
  service_map <- service_map +
    ggplot2::geom_point(
      data = markers_emergency_dep,
      mapping = ggplot2::aes(
        x = longitude,
        y = latitude,
        colour = "Emergency Department"
      ),
      size = 2,
      shape = 23,
      stroke = 0.5,
      fill = "blue"
    )
}
if (nrow(markers_miu) > 0) {
  service_map <- service_map +
    ggplot2::geom_point(
      data = markers_miu,
      mapping = ggplot2::aes(
        x = longitude,
        y = latitude,
        colour = "Minor Injuries Unit"
      ),
      size = 2,
      shape = 24,
      stroke = 0.5,
      fill = "green"
    )
}

# 2.5 Final map ----
# create final service map WITHOUT LEGEND

service_map <- service_map +
  ggplot2::labs(colour = "Service Type") +
  ggplot2::scale_color_manual(
    values = c(
      "GP Practice" = "black",
      "Care Home" = "black",
      "Emergency Department" = "black",
      "Minor Injuries Unit" = "black"
    )
  ) +
  ggplot2::theme(legend.title = ggplot2::element_blank()) +
  ggplot2::scale_fill_manual(values = col_palette) +
  ggrepel::geom_text_repel(
    data = places,
    ggplot2::aes(x = Longitude, y = Latitude, label = name),
    color = "black",
    size = 3.5,
    max.overlaps = 18,
    max.time = 2,
    max.iter = 100000
  ) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    rect = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank()
  ) +
  ggplot2::labs(caption = "Source: Public Health Scotland") +
  ggplot2::theme(legend.position = "none")

# Create Map of Just the Locality Areas in order to take its legend
service_map_1 <- ggmap::ggmap(service_map_background) +
  sf::geom_sf(
    data = shp_hscp,
    mapping = ggplot2::aes(
      fill = hscp_local
    ),
    colour = "black",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  ggplot2::labs(fill = "Locality") +
  ggplot2::theme(legend.title = ggplot2::element_blank()) +
  ggplot2::scale_fill_manual(values = col_palette) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    rect = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank()
  ) +
  ggplot2::labs(caption = "Source: Public Health Scotland")

leg1 <- cowplot::get_legend(service_map_1)

# Create Map of Just the Locations in order to use its legend
all_markers <- dplyr::select(markers_miu, name, latitude, longitude) |>
  dplyr::mutate(type = "Minor Injury Unit") |>
  dplyr::bind_rows(
    dplyr::select(markers_care_home, name, latitude, longitude) |>
      dplyr::mutate(type = "Care Home")
  ) |>
  dplyr::bind_rows(
    dplyr::select(markers_emergency_dep, name, latitude, longitude) |>
      dplyr::mutate(type = "Emergency Department")
  ) |>
  dplyr::bind_rows(
    dplyr::select(markers_gp, name = gp_practice_name, latitude, longitude) |>
      dplyr::mutate(type = "GP Practice")
  )

service_map_2 <- ggmap::ggmap(service_map_background) +
  ggplot2::geom_point(
    data = all_markers,
    ggplot2::aes(x = longitude, y = latitude, colour = type, fill = type, shape = type)
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "GP Practice" = "black",
      "Care Home" = "black",
      "Emergency Department" = "black",
      "Minor Injury Unit" = "black"
    )
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "GP Practice" = "red",
      "Care Home" = "yellow",
      "Emergency Department" = "blue",
      "Minor Injury Unit" = "green"
    )
  ) +
  ggplot2::scale_shape_manual(
    values = c(
      "GP Practice" = 21,
      "Care Home" = 22,
      "Emergency Department" = 23,
      "Minor Injury Unit" = 24
    )
  ) +
  ggplot2::theme(legend.title = ggplot2::element_blank()) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    rect = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank()
  ) +
  ggplot2::labs(caption = "Source: Public Health Scotland")


leg2 <- cowplot::get_legend(service_map_2)

# Create new legend which combines other legends ----

blank_leg <- patchwork::plot_spacer() + ggplot2::theme_void()

leg12 <- cowplot::plot_grid(
  blank_leg,
  leg1,
  blank_leg,
  leg2,
  blank_leg,
  ncol = 1
)

# Combine plot of locations and localities with corrected legends

service_map <- cowplot::plot_grid(
  service_map,
  leg12,
  nrow = 1,
  align = "h",
  axis = "t",
  rel_widths = c(1.7, 1.0)
)

# 3. Cleanup ----
# Objects that are not needed anymore
rm(
  all_markers,
  api_key,
  blank_leg,
  col_palette,
  hscp_loc,
  leg1,
  leg2,
  leg12,
  locality_map_id,
  max_lat,
  max_long,
  min_lat,
  min_long,
  places,
  service_map_1,
  service_map_2,
  service_map_background,
  shp,
  shp_hscp,
  zones_coord
)
