# Population data loading (Global level)

## Locality/DZ lookup
lookup <- read_in_localities()

## Population data
pop_raw_data <- read_in_dz_pops()

## Population Projection Data
hscp_pop_proj <- read_in_pop_proj()

## Set year
pop_max_year <- max(pop_raw_data[["year"]])
pop_min_year <- pop_max_year - 5

# compute age bands
pop_raw_data[["Pop0_4"]] <- rowSums(subset(pop_raw_data, select = age0:age4))
pop_raw_data[["Pop5_17"]] <- rowSums(subset(pop_raw_data, select = age5:age17))
pop_raw_data[["Pop18_44"]] <- rowSums(subset(pop_raw_data, select = age18:age44))
pop_raw_data[["Pop45_64"]] <- rowSums(subset(pop_raw_data, select = age45:age64))
pop_raw_data[["Pop65_74"]] <- rowSums(subset(pop_raw_data, select = age65:age74))
pop_raw_data[["Pop75_84"]] <- rowSums(subset(pop_raw_data, select = age75:age84))
pop_raw_data[["Pop85Plus"]] <- rowSums(subset(
  pop_raw_data,
  select = age85:age90plus
))
pop_raw_data[["Pop65Plus"]] <- rowSums(subset(
  pop_raw_data,
  select = age65:age90plus
))

pops <- select(
  pop_raw_data,
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
)

# Aggregate and add partnership + Scotland totals
pops <- pops |>
  group_by(year, sex, hscp2019name, hscp_locality) |>
  summarise(across(everything(), sum)) |>
  ungroup() |>
  # Add a partnership total
  bind_rows(
    pops |>
      select(-hscp_locality) |>
      group_by(year, hscp2019name, sex) |>
      summarise(across(everything(), sum)) |>
      ungroup() |>
      mutate(hscp_locality = "Partnership Total")
  ) |>
  # Add a Scotland total
  bind_rows(
    pops |>
      select(-hscp_locality, -hscp2019name) |>
      group_by(year, sex) |>
      summarise(across(everything(), sum)) |>
      ungroup() |>
      mutate(hscp_locality = "Scotland Total", hscp2019name = "Scotland")
  )
