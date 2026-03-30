############################################################################################# .
#                                                                                           #
#                   LOCALITY PROFILES LIFESTYLE & RISK FACTORS DATA MANIPULATION            #
#                                                                                           #
############################################################################################# .

## Code used to calculate partnership-level objects and summary statistics for the
#  Lifestyle & Risk factors section of the locality profiles.

### Geographical lookups and objects ----

# Determine HSCP from LOCALITY if not already set (e.g. for testing chapters)
if (!exists("HSCP") && exists("LOCALITY")) {
  HSCP <- as.character(filter(lookup, hscp_locality == LOCALITY)$hscp2019name)
}

# Determine HB based on HSCP
HB <- as.character(filter(lookup, hscp2019name == HSCP)$hb2019name[1])

# Determine other localities based on HSCP object
other_locs <- lookup %>%
  select(hscp_locality, hscp2019name) %>%
  filter(hscp2019name == HSCP) %>%
  arrange(hscp_locality)

# Find number of locs per partnership
n_loc <- count_localities(lookup, HSCP)

############################### 3) CODE FOR SUMMARY TABLE ###############################

# 1. Other locs (calculated once per HSCP)

other_locs_drug_hosp <- other_locs_summary_table(
  drug_hosp,
  latest_year = max(drug_hosp$year)
)

other_locs_alcohol_hosp <- other_locs_summary_table(
  alcohol_hosp,
  latest_year = max(alcohol_hosp$year)
)

other_locs_alcohol_deaths <- other_locs_summary_table(
  alcohol_deaths,
  latest_year = max(alcohol_deaths$year)
)

other_locs_bowel_screening <- other_locs_summary_table(
  bowel_screening,
  latest_year = max(bowel_screening$year)
)

# 2. HSCP

hscp_drug_hosp <- round_half_up(
  filter(
    drug_hosp,
    year == max(year) &
      (area_name == HSCP & area_type == "HSCP")
  )$measure,
  1
)

hscp_alcohol_hosp <- round_half_up(
  filter(
    alcohol_hosp,
    year == max(year) &
      (area_name == HSCP & area_type == "HSCP")
  )$measure,
  1
)

hscp_alcohol_deaths <- round_half_up(
  filter(
    alcohol_deaths,
    year == max(year) &
      (area_name == HSCP & area_type == "HSCP")
  )$measure,
  1
)

hscp_bowel_screening <- round_half_up(
  filter(
    bowel_screening,
    year == max(year) &
      (area_name == HSCP & area_type == "HSCP")
  )$measure,
  1
)

# 3. Scotland

scot_drug_hosp <- round_half_up(
  filter(
    drug_hosp,
    year == max(year) &
      area_name == "Scotland"
  )$measure,
  1
)

scot_alcohol_hosp <- round_half_up(
  filter(
    alcohol_hosp,
    year == max(year) &
      area_name == "Scotland"
  )$measure,
  1
)

scot_alcohol_deaths <- round_half_up(
  filter(
    alcohol_deaths,
    year == max(year) &
      area_name == "Scotland"
  )$measure,
  1
)

scot_bowel_screening <- round_half_up(
  filter(
    bowel_screening,
    year == max(year) &
      area_name == "Scotland"
  )$measure,
  1
)
