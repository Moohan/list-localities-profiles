# Population data loading and global processing
# Loaded once per session in the outer loop (e.g., in Build Profiles.R)

## Locality/DZ lookup
lookup_all <- read_in_localities()

## Population data
pop_raw_data_all <- read_in_dz_pops()

## Population Projection Data
hscp_pop_proj_all <- read_in_pop_proj()

## Set year
pop_max_year <- max(pop_raw_data_all$year)
pop_min_year <- pop_max_year - 5

# Pre-calculate age bands for all datazones
pop_raw_data_all$Pop0_4 <- rowSums(subset(pop_raw_data_all, select = age0:age4))
pop_raw_data_all$Pop5_17 <- rowSums(subset(pop_raw_data_all, select = age5:age17))
pop_raw_data_all$Pop18_44 <- rowSums(subset(pop_raw_data_all, select = age18:age44))
pop_raw_data_all$Pop45_64 <- rowSums(subset(pop_raw_data_all, select = age45:age64))
pop_raw_data_all$Pop65_74 <- rowSums(subset(pop_raw_data_all, select = age65:age74))
pop_raw_data_all$Pop75_84 <- rowSums(subset(pop_raw_data_all, select = age75:age84))
pop_raw_data_all$Pop85Plus <- rowSums(subset(
  pop_raw_data_all,
  select = age85:age90plus
))
pop_raw_data_all$Pop65Plus <- rowSums(subset(
  pop_raw_data_all,
  select = age65:age90plus
))

# Global summary of populations
pops_global <- pop_raw_data_all |>
  select(
    year,
    sex,
    hscp2019name,
    hscp_locality,
    Pop0_4,
    Pop5_17,
    Pop18_44,
    Pop45_64,
    Pop65_74,
    Pop75_84,
    Pop85Plus,
    Pop65Plus,
    total_pop
  ) |>
  group_by(year, sex, hscp2019name, hscp_locality) |>
  summarise(across(everything(), sum), .groups = "drop")

# Add partnership totals
pops_global <- pops_global |>
  bind_rows(
    pops_global |>
      select(-hscp_locality) |>
      group_by(year, hscp2019name, sex) |>
      summarise(across(everything(), sum), .groups = "drop") |>
      mutate(hscp_locality = "Partnership Total")
  ) |>
  # Add a Scotland total
  bind_rows(
    pops_global |>
      select(-hscp_locality, -hscp2019name) |>
      group_by(year, sex) |>
      summarise(across(everything(), sum), .groups = "drop") |>
      mutate(hscp_locality = "Scotland Total", hscp2019name = "Scotland")
  )
