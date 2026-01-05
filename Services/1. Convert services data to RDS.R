##### Script to convert Excel data for Services section and save to RDS #####

## This script takes all the csv files in the data folder,
# saves RDS versions and deletes the csv versions to save space.

source("Master RMarkdown Document & Render Code/Global Script.R")

# Change year to be the year in the data folder name
ext_year <- 2024

lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

# Extract all file names from the CSV folder
my_files <- list.files(
  paste0(lp_path, "Services/DATA ", ext_year, "/CSV"),
  pattern = ".csv"
)

# Remove .csv from file names
file_names <- as.list(gsub(".csv", "", my_files))

# Apply "filt_and_save" function to each element of the file_names list
lapply(
  file_names,
  filt_and_save,
  dir_path = paste0(lp_path, "Services/DATA ", ext_year, "/CSV")
)
