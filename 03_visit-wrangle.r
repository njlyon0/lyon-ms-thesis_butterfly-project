##  ------------------------------------------------------------  ##
# Butterfly Project - Wrangle Nectar Resource Data
##  ------------------------------------------------------------  ##
# Purpose:
## Wrangle nectar flower data (e.g., do quality control, calculate metrics)

# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, supportR)

# Get set up
source("-setup.r")

# Clear environment
rm(list = ls()); gc()

##  ------------------------------------------  ##
# Load Data ----
##  ------------------------------------------  ##

# Identify the relevant file
bfly_file <- file.path("data", "raw", "2007-2018 GRG INVERTS MASTER DATA.xlsx")

# Check what sheets are in it
readxl::excel_sheets(path = bfly_file)

# Read in the relevant sheet
vst_v01 <- readxl::read_excel(path = bfly_file, sheet = "Sites",
  col_types = "text")

# Check structure
dplyr::glimpse(vst_v01)

##  ------------------------------------------  ##
# Streamline Column Names ----
##  ------------------------------------------  ##

# Get the column names in a better format
vst_v02 <- vst_v01 %>% 
  dplyr::rename_with(.fn = tolower) %>% 
  dplyr::rename_with(.fn = ~ gsub(pattern = "\\.", replacement = "_", x = .)) %>% 
  dplyr::rename_with(.fn = ~ gsub(pattern = "data_enterer", replacement = "data.enterer", x = .)) %>% 
  dplyr::rename_with(.fn = ~ gsub(pattern = "data_entry_date", replacement = "data.entry.date", x = .)) %>% 
  dplyr::rename(temperature_c = temp_c,
    cloud.cover_percent = cloud_cover_percent,
    transect.start_time = start_time,
    transect.end_time = end_time)

# Check new column names
names(vst_v02)

# Check structure
dplyr::glimpse(vst_v02)

##  ------------------------------------------  ##
# Fix Column Class ISsues ----
##  ------------------------------------------  ##

# Coerce columns to correct classes
vst_v03 <- vst_v02 %>% 
  dplyr::mutate(dplyr::across(
    .cols = dplyr::all_of(c("transect_id", "year", "wind_kph", "temperature_c", "cloud.cover_percent")),
    .fns = as.numeric))

# Check structure
dplyr::glimpse(vst_v03)

##  ------------------------------------------  ##
# Check Categorical Columns ----
##  ------------------------------------------  ##

# Check categorical columns
sort(unique(vst_v03$site))
sort(unique(vst_v03$month))
sort(unique(vst_v03$round))
sort(unique(vst_v03$butterfly_observer))
sort(unique(vst_v03$nectar_observer))

# Standardize and streamline those columns
vst_v04 <- vst_v03 %>% 
  dplyr::mutate(butterfly_observer = dplyr::case_when(
    butterfly_observer %in% c("???") ~ "unrecorded",
    butterfly_observer %in% c("rayy moranz") ~ "ray moranz",
    TRUE ~ butterfly_observer)) %>% 
  dplyr::mutate(butterfly_observer = gsub("mecko", "veronica mecko", x = butterfly_observer)) %>% 
  dplyr::mutate(butterfly_observer = gsub(" and ", "; ", x = butterfly_observer)) %>% 
  dplyr::mutate(nectar_observer = dplyr::case_when(
    nectar_observer %in% c("jason") ~ "jason NLN",
    TRUE ~ nectar_observer)) %>% 
  dplyr::mutate(nectar_observer = gsub("gatha", "gatha mortensen", x = nectar_observer)) %>% 
  dplyr::mutate(nectar_observer = gsub(" and |\\/", "; ", x = nectar_observer))

# Re-check modified categories
supportR::diff_check(old = unique(vst_v03$butterfly_observer), new = unique(vst_v04$butterfly_observer))
supportR::diff_check(old = unique(vst_v03$nectar_observer), new = unique(vst_v04$nectar_observer))

# Check structure
dplyr::glimpse(vst_v04)

##  ------------------------------------------  ##
# Remove Unwanted Rows/Columns
##  ------------------------------------------  ##

# Remove any rows/columns that are not likely to be useful
vst_v05 <- vst_v04 %>% 
  dplyr::filter(!site %in% c("ACCIDENTAL ROW")) %>% 
  dplyr::select(-month, -round, -dplyr::ends_with("_time"), -dplyr::contains("data.ent"), -comments)

# What was lost?
message(nrow(vst_v04) - nrow(vst_v05), " rows removed")
supportR::diff_check(old = names(vst_v04), new = names(vst_v05))

# Structure check
dplyr::glimpse(vst_v05)

##  ------------------------------------------  ##
# Fix Dates ----
##  ------------------------------------------  ##

# Do needed repairs to get date in reasonable format
vst_v06 <- vst_v05 %>% 
  tidyr::separate_wider_delim(cols = date, delim = ".",
    names = c("month", "day"), cols_remove = TRUE) %>%
  dplyr::mutate(month = as.numeric(month),
    day = as.numeric(day)) %>% 
  dplyr::relocate(month, day, .after = year) %>%
  dplyr::mutate(year = year + 2000) %>% 
  dplyr::mutate(date = as.Date(paste(day, month, year, sep = "-"), format = "%d-%m-%Y"),
    .after = day)

# Check structure
dplyr::glimpse(vst_v06)

##  ------------------------------------------  ##
# Export ----
##  ------------------------------------------  ##

# Make a final object
vst_v99 <- vst_v06

# Check structure
dplyr::glimpse(vst_v99)

# Export this locally
write.csv(x = vst_v99, row.names = FALSE, na = "",
  file = file.path("data", "tidy", "03_tidy-visit.csv"))

# End ----
