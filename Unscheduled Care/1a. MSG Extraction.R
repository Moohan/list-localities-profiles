########################## SECTION 2: MSG Data ###############################

# Read in MSG data
msg_emerg_adm_raw <- read_parquet(paste0(
  "/conf/LIST_analytics/MSG/",
  latest_msg_folder,
  "/Breakdowns/1a-Admissions-breakdown.parquet"
))

msg_beddays_raw <- read_parquet(paste0(
  "/conf/LIST_analytics/MSG/",
  latest_msg_folder,
  "/Breakdowns/2a-Acute-Beddays-breakdown.parquet"
))

msg_ae_raw <- read_parquet(paste0(
  "/conf/LIST_analytics/MSG/",
  latest_msg_folder,
  "/Breakdowns/3-A&E-Breakdowns.parquet"
))

msg_dd_raw <- read_parquet(paste0(
  "/conf/LIST_analytics/MSG/",
  latest_msg_folder,
  "/Breakdowns/4-Delayed-Discharge-Breakdowns.parquet"
))

msg_mh_beddays_raw <- read_parquet(paste0(
  "/conf/LIST_analytics/MSG/",
  latest_msg_folder,
  "/Breakdowns/2c-MH-Beddays-breakdown.parquet"
))


# 1. Emergency Admissions ----
# _________________________________________________________________________

msg_emergency_adm <- msg_emerg_adm_raw %>%
  mutate(age_group = age_group_1(age_group)) %>%
  mutate(financial_year = phsmethods::extract_fin_year(month)) %>%
  mutate(hscp_locality = gsub("&", "and", locality, fixed = TRUE)) %>%
  # join with localities lookup to get hscp
  left_join(localities, by = "hscp_locality") %>%
  # aggregate
  group_by(financial_year, hscp2019name, hscp_locality, age_group) %>%
  summarise(admissions = sum(admissions)) %>%
  ungroup()


# 2a. Unscheduled bed days ----
# _________________________________________________________________________

msg_bed_days <- msg_beddays_raw %>%
  mutate(age_group = age_group_1(age_group)) %>%
  mutate(financial_year = phsmethods::extract_fin_year(month)) %>%
  mutate(hscp_locality = gsub("&", "and", locality, fixed = TRUE)) %>%
  # join with localities lookup to get hscp
  left_join(localities, by = "hscp_locality") %>%
  # aggregate
  group_by(financial_year, hscp2019name, hscp_locality, age_group) %>%
  summarise(bed_days = sum(unplanned_beddays)) %>%
  ungroup()


# 2b. Unscheduled bed days - Mental Health ----
# _________________________________________________________________________

msg_bed_days_mh <- msg_mh_beddays_raw %>%
  mutate(age_group = age_group_1(age_group)) %>%
  mutate(financial_year = phsmethods::extract_fin_year(month)) %>%
  mutate(hscp_locality = gsub("&", "and", locality, fixed = TRUE)) %>%
  # join with localities lookup to get hscp
  left_join(localities, by = "hscp_locality") %>%
  # aggregate
  group_by(financial_year, hscp2019name, hscp_locality, age_group) %>%
  summarise(bed_days = sum(unplanned_beddays)) %>%
  ungroup()


# 3. A&E Attendances ----
# _________________________________________________________________________

msg_ae <- msg_ae_raw %>%
  mutate(age_group = age_group_1(age_group)) %>%
  mutate(financial_year = phsmethods::extract_fin_year(month)) %>%
  mutate(hscp_locality = gsub("&", "and", locality, fixed = TRUE)) %>%
  # join with localities lookup to get hscp
  left_join(localities, by = "hscp_locality") %>%
  # aggregate
  group_by(financial_year, hscp2019name, hscp_locality, age_group) %>%
  summarise(attendances = sum(attendances)) %>%
  ungroup()


# 4. Delayed Discharges ----
# _________________________________________________________________________

msg_dd <- msg_dd_raw %>%
  mutate(age_group = age_group_1(age_group)) %>%
  mutate(financial_year = phsmethods::extract_fin_year(month)) %>%
  mutate(hscp_locality = gsub("&", "and", locality, fixed = TRUE)) %>%
  # this data set has some data with partnership but no locality, need to tidy names
  mutate(hscp2019name = gsub("&", "and", council, fixed = TRUE)) %>%
  mutate(hscp2019name = ptsp(hscp2019name)) %>%
  group_by(
    financial_year,
    hscp2019name,
    hscp_locality,
    age_group,
    reason_for_delay
  ) %>%
  summarise(
    dd_people = n(),
    dd_bed_days = sum(delayed_bed_days)
  ) %>%
  ungroup()


## Save Data ----
# _________________________________________________________________________

# Emergency admissions
write_parquet(
  msg_emergency_adm,
  paste0(exportfolder, "emergency_admissions_msg.parquet")
)

# Bed days
write_parquet(msg_bed_days, paste0(exportfolder, "bed_days_msg.parquet"))

# Bed days MH
write_parquet(msg_bed_days_mh, paste0(exportfolder, "bed_days_mh_msg.parquet"))

# A&E
write_parquet(msg_ae, paste0(exportfolder, "ae_attendances_msg.parquet"))

# Delayed discharges
write_parquet(msg_dd, paste0(exportfolder, "delayed_discharges_msg.parquet"))
