##################### LOCALITY PROFILES UNSCHEDULED CARE: DATA MANIPULATION ######################

# Derive HSCP and HB if not already defined
localities <- read_in_localities()
if (!exists("HSCP") && exists("LOCALITY")) {
  HSCP <- as.character(
    filter(localities, hscp_locality == LOCALITY)$hscp2019name
  )
}
if (!exists("HB")) {
  if (exists("LOCALITY")) {
    HB <- as.character(filter(localities, hscp_locality == LOCALITY)$hb2019name)
  } else if (exists("HSCP")) {
    HB <- as.character(filter(localities, hscp2019name == HSCP)$hb2019name[1])
  }
}

# Helper function for aggregation at HSCP level
# This function calculates aggregates for:
# - All localities within the current HSCP
# - The HSCP itself
# - The Health Board
# - Scotland
aggregate_usc_all_areas <- function(data) {
  # Locality level for all localities in HSCP
  pts_localities <- data %>%
    filter(hscp2019name == HSCP) %>%
    group_by(financial_year, hscp_locality) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    mutate(location = hscp_locality, area_type = "Locality")

  pts_hscp <- data %>%
    filter(hscp2019name == HSCP) %>%
    group_by(financial_year) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    mutate(location = HSCP, area_type = "HSCP")

  pts_hb <- data %>%
    left_join(
      select(localities, hscp_locality, hb2019name),
      by = join_by(hscp_locality)
    ) %>%
    filter(hb2019name == HB) %>%
    group_by(financial_year) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    mutate(location = HB, area_type = "HB")

  pts_scot <- data %>%
    group_by(financial_year) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    mutate(location = "Scotland", area_type = "Scotland")

  bind_rows(pts_localities, pts_hscp, pts_hb, pts_scot) %>%
    mutate(
      area_type = factor(
        area_type,
        levels = c("Locality", "HSCP", "HB", "Scotland")
      )
    )
}

## 1. Population Aggregates for all areas

pop_hscp_aggregates <- pops %>%
  filter(hscp2019name == HSCP) %>%
  mutate(location = hscp_locality, area_type = "Locality") %>%
  bind_rows(
    pops %>%
      filter(hscp2019name == HSCP) %>%
      group_by(financial_year, year) %>%
      summarise(across(Pop0_17:total_pop, sum)) %>%
      ungroup() %>%
      mutate(location = HSCP, area_type = "HSCP")
  ) %>%
  bind_rows(
    pops %>%
      filter(hb2019name == HB) %>%
      group_by(financial_year, year) %>%
      summarise(across(Pop0_17:total_pop, sum)) %>%
      ungroup() %>%
      mutate(location = HB, area_type = "HB")
  ) %>%
  bind_rows(
    pops %>%
      group_by(financial_year, year) %>%
      summarise(across(Pop0_17:total_pop, sum)) %>%
      ungroup() %>%
      mutate(location = "Scotland", area_type = "Scotland")
  ) %>%
  pivot_longer(
    Pop0_17:total_pop,
    names_to = "age_group",
    values_to = "pop"
  ) %>%
  mutate(
    age_group = case_when(
      age_group == "Pop0_17" ~ "0 - 17",
      age_group == "Pop18_44" ~ "18 - 44",
      age_group == "Pop45_64" ~ "45 - 64",
      age_group == "Pop65_74" ~ "65 - 74",
      age_group == "Pop75Plus" ~ "75+",
      age_group == "Pop65Plus" ~ "65+",
      age_group == "total_pop" ~ "Total"
    )
  )

## 2. Emergency Admissions Aggregates
emergency_adm_hscp_aggregates <- emergency_adm_raw %>%
  rename(n = admissions) %>%
  aggregate_usc_all_areas()

## 3. Unscheduled Bed Days Aggregates
bed_days_hscp_aggregates <- bed_days_raw %>%
  rename(n = bed_days) %>%
  aggregate_usc_all_areas()

## 4. Unscheduled Bed Days MH Aggregates
bed_days_mh_hscp_aggregates <- bed_days_mh_raw %>%
  rename(n = bed_days) %>%
  aggregate_usc_all_areas()

## 5. A&E Attendances Aggregates
ae_attendances_hscp_aggregates <- ae_attendances_raw %>%
  rename(n = attendances) %>%
  aggregate_usc_all_areas()

## 6. Delayed Discharges Aggregates
delayed_disch_hscp_aggregates <- delayed_disch_raw %>%
  rename(n = dd_bed_days) %>%
  aggregate_usc_all_areas()

## 7. Falls Aggregates
falls_hscp_aggregates <- falls_raw %>%
  rename(n = admissions) %>%
  aggregate_usc_all_areas()

## 8. Readmissions Aggregates
read_n_aggregates <- readmissions_raw %>%
  rename(n = read_28) %>%
  aggregate_usc_all_areas() %>%
  rename(read_28 = n)

read_d_aggregates <- readmissions_raw %>%
  rename(n = discharges) %>%
  aggregate_usc_all_areas() %>%
  rename(discharges = n)

readmissions_hscp_aggregates <- left_join(
  read_n_aggregates,
  read_d_aggregates,
  by = join_by(financial_year, location, area_type)
)

## 9. PPA Aggregates
ppa_hscp_aggregates <- ppa_raw %>%
  rename(n = admissions) %>%
  aggregate_usc_all_areas()

ppa_65plus_hscp_aggregates <- ppa_raw %>%
  filter(age_group %in% c("65 - 74", "75+")) %>%
  rename(n = admissions) %>%
  aggregate_usc_all_areas() %>%
  rename(plus65tot = n)

ppa_under65_hscp_aggregates <- ppa_raw %>%
  filter(age_group %in% c("0 - 17", "18 - 44", "45 - 64")) %>%
  rename(n = admissions) %>%
  aggregate_usc_all_areas() %>%
  rename(under65tot = n)
