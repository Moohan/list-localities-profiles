##################### LOCALITY PROFILES DEMOGRAPHICS: SIMD LOADING ################

# Description: Global SIMD data and shapefile loading for locality profiles.
#              This script is intended to be run once per session.

## Locality/DZ lookup
lookup_dz_base <- read_in_localities(dz_level = TRUE)

## Population data (already loaded in 1a, but kept for robustness if run independently)
pop_raw_data_simd <- read_in_dz_pops()
pop_max_year_simd <- max(pop_raw_data_simd$year)

pop_data_base <- pop_raw_data_simd %>%
  filter(year == max(year)) %>%
  group_by(
    year,
    datazone2011,
    hscp_locality,
    hscp2019name,
    simd2020v2_sc_quintile
  ) %>%
  summarise(total_pop = sum(total_pop)) %>%
  ungroup()

## SIMD Domains
lookups_dir <- path("/conf/linkage/output/lookups/Unicode")

# 2020
simd_2020_all <- read_rds(path(
  lookups_dir,
  "Deprivation",
  "DataZone2011_simd2020v2.rds"
)) %>%
  select(datazone2011, simd = "simd2020v2_sc_quintile")

simd_2020_dom <- read_rds(path(
  lookups_dir,
  "Deprivation",
  "DataZone2011_domain_level_simd.rds"
)) %>%
  clean_names() %>%
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

simd2020_base <- merge(simd_2020_all, simd_2020_dom, by = "datazone2011") %>%
  left_join(pop_data_base, by = join_by(datazone2011))

# 2016
simd_2016_all <- read_rds(path(
  lookups_dir,
  "Deprivation",
  "DataZone2011_simd2016.rds"
)) %>%
  select(datazone2011 = "DataZone2011", simd = "simd2016_sc_quintile")

simd_2016_dom <- read_rds(path(
  lookups_dir,
  "Deprivation",
  "DataZone2011_domain_level_simd.rds"
)) %>%
  clean_names() %>%
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

simd2016_base <- merge(simd_2016_all, simd_2016_dom, by = "datazone2011") %>%
  left_join(lookup_dz_base, by = join_by(datazone2011))

# Load in shapefile for mapping
zones_base <- read_sf(path(
  lookups_dir,
  "Geography",
  "Shapefiles",
  "Data Zones 2011",
  "SG_DataZone_Bdry_2011.shp"
)) %>%
  st_transform(4326) %>%
  rename(datazone2011 = DataZone)

# Merge lookup and shapefile
zones_base <- merge(zones_base, lookup_dz_base, by = "datazone2011")

# Clean up temporary objects
rm(
  simd_2016_all,
  simd_2016_dom,
  simd_2020_all,
  simd_2020_dom,
  pop_raw_data_simd,
  pop_max_year_simd
)
