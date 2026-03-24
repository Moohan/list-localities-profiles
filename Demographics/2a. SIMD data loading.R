# SIMD data loading
# Loaded once per session in the outer loop (e.g., in Build Profiles.R)

## Locality/DZ lookup
lookup_dz_all <- read_in_localities(dz_level = TRUE)

## SIMD Domains
lookups_dir <- "/conf/linkage/output/lookups/Unicode"

# 2020
simd_2020_all <- readr::read_rds(fs::path(
  lookups_dir,
  "Deprivation",
  "DataZone2011_simd2020v2.rds"
)) |>
  select(datazone2011, simd = "simd2020v2_sc_quintile")

simd_2020_dom <- readr::read_rds(fs::path(
  lookups_dir,
  "Deprivation",
  "DataZone2011_domain_level_simd.rds"
)) |>
  janitor::clean_names() |>
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

simd2020_global <- merge(simd_2020_all, simd_2020_dom, by = "datazone2011")

# 2016
simd_2016_all <- readr::read_rds(fs::path(
  lookups_dir,
  "Deprivation",
  "DataZone2011_simd2016.rds"
)) |>
  select(datazone2011 = "DataZone2011", simd = "simd2016_sc_quintile")

simd_2016_dom <- readr::read_rds(fs::path(
  lookups_dir,
  "Deprivation",
  "DataZone2011_domain_level_simd.rds"
)) |>
  janitor::clean_names() |>
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

simd2016_global <- merge(simd_2016_all, simd_2016_dom, by = "datazone2011")

rm(simd_2020_all, simd_2020_dom, simd_2016_all, simd_2016_dom)
