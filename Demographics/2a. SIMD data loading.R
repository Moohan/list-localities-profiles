# SIMD data loading (Global level)

## Locality/DZ lookup
lookup_dz <- read_in_localities(dz_level = TRUE)

# pop_raw_data is already loaded by 1a. Population data loading.R
pop_data <- pop_raw_data |>
  filter(year == max(year)) |>
  group_by(
    year,
    datazone2011,
    hscp_locality,
    hscp2019name,
    simd2020v2_sc_quintile
  ) |>
  summarise(total_pop = sum(total_pop)) |>
  ungroup()


## SIMD Domains
lookups_dir <- fs::path("/conf", "linkage", "output", "lookups", "Unicode")

# 2020
simd_2020_all <- read_rds(path(
  lookups_dir,
  "Deprivation",
  "DataZone2011_simd2020v2.rds"
)) |>
  select(datazone2011, simd = "simd2020v2_sc_quintile")

simd_2020_dom <- read_rds(path(
  lookups_dir,
  "Deprivation",
  "DataZone2011_domain_level_simd.rds"
)) |>
  clean_names() |>
  select(
    datazone2011,
    income = "simd2020v2_inc_quintile",
    employment = "simd2020v2_emp_quintile",
    education = "simd2020v2_educ_quintile",
    access = "simd2020v2_access_quintile",
    housing = "simd2020v2_house_quintile",
    health = "simd2020v2_hlth_quintile",
    crime = "simd2020v2_crime_quintile"
  )

simd2020 <- merge(simd_2020_all, simd_2020_dom, by = "datazone2011") |>
  left_join(pop_data, by = join_by(datazone2011))

# 2016
simd_2016_all <- read_rds(path(
  lookups_dir,
  "Deprivation",
  "DataZone2011_simd2016.rds"
)) |>
  select(datazone2011 = "DataZone2011", simd = "simd2016_sc_quintile")

simd_2016_dom <- read_rds(path(
  lookups_dir,
  "Deprivation",
  "DataZone2011_domain_level_simd.rds"
)) |>
  clean_names() |>
  select(
    datazone2011,
    income = "simd2016_inc_quintile",
    employment = "simd2016_emp_quintile",
    education = "simd2016_educ_quintile",
    access = "simd2016_access_quintile",
    housing = "simd2016_house_quintile",
    health = "simd2016_hlth_quintile",
    crime = "simd2016_crime_quintile"
  )

simd2016 <- merge(simd_2016_all, simd_2016_dom, by = "datazone2011") |>
  left_join(lookup_dz, by = join_by(datazone2011))

rm(simd_2020_all, simd_2020_dom, simd_2016_all, simd_2016_dom)

# load in shapefile for mapping
zones_global <- read_sf(path(
  lookups_dir,
  "Geography",
  "Shapefiles",
  "Data Zones 2011",
  "SG_DataZone_Bdry_2011.shp"
)) |>
  st_transform(4326) |>
  rename(datazone2011 = DataZone)

# merge lookup and shapefile
zones_global <- merge(zones_global, lookup_dz, by = "datazone2011")
