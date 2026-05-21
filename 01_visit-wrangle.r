##  ------------------------------------------------------------  ##
# Butterfly Project - Wrangle Visit Data
##  ------------------------------------------------------------  ##
# Purpose:
## Wrangle sampling event ("visit") data (e.g., do quality control, calculate metrics)

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

# Check patches within site
vst_v03 %>% 
  dplyr::group_by(site) %>% 
  dplyr::summarize(patch_ct = length(unique(patch)),
    patches = paste(unique(patch), collapse = "; "),
    .groups = "drop") %>% 
  dplyr::filter(patch_ct != 3)

# Standardize and streamline those columns
vst_v04 <- vst_v03 %>% 
  dplyr::mutate(dplyr::across(.cols = site:patch,
    .fns = ~ gsub(pattern = "RIN-C", replacement = "RIN-S", x = .))) %>% 
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
  dplyr::filter(!is.na(year)) %>% 
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
# Identify Management/Treatment Information ----
##  ------------------------------------------  ##

# Identify a number of critical treatments over the study period
vst_v07 <- vst_v06 %>% 
  dplyr::mutate(management_pyric.herbivory = dplyr::case_when(
    site %in% c("KLT", "RIN", "PAW") ~ "burn only",
    year < 2014 & site == "RCH" ~ "burn only",
    site %in% c("GIL", "LTR", "PYW", "STE") ~ "graze and burn",
    site %in% c("235", "KLN", "PYN", "PYS", "RIS") ~ "patch-burn graze",
    year >= 2014 & site == "RCH" ~ "patch-burn graze",
    site %in% c("BSH", "DUN", "RC2") ~ "none",
    TRUE ~ "unknown"), .after = patch) %>% 
  dplyr::mutate(management_anti.fescue = dplyr::case_when(
    year <= 2013 ~ "none",
    patch %in% c("BSH-N", "DUN-C", "GIL-S", "LTR-W", "PYW-S", "RC2-S", "STE-W") ~ "control",
    patch %in% c("BSH-C", "DUN-E", "GIL-N", "LTR-C", "PYW-C", "RC2-C", "STE-N") ~ "herbicide only",
    patch %in% c("BSH-S", "DUN-W", "GIL-C", "LTR-E", "PYW-N", "RC2-N", "STE-S") ~ "herbicide and seedmix addition",
    TRUE ~ "none"), .after = management_pyric.herbivory) %>% 
  dplyr::mutate(burn.cohort = dplyr::case_when(
    patch %in% c("KLN-W", "PYN-S", "PYS-N", "RIS-C") ~ "A",
    patch %in% c("KLN-C", "PYN-W", "PYS-S", "RIS-N") ~ "B",
    patch %in% c("KLN-E", "PYN-N", "PYS-W", "RIS-S") ~ "C",
    site %in% c("LTR", "PYW") ~ "A",
    TRUE ~ "X")) %>% 
  dplyr::mutate(time.since.fire_years = dplyr::case_when(
    burn.cohort == "A" & year %in% (2000 + c(9, 12, 15, 18)) ~ "0",
    burn.cohort == "A" & year %in% (2000 + c(10, 13, 16)) ~ "1",
    burn.cohort == "A" & year %in% (2000 + c(11, 14, 17)) ~ "2",
    burn.cohort == "B" & year %in% (2000 + c(8, 11, 14, 17)) ~ "0",
    burn.cohort == "B" & year %in% (2000 + c(9, 12, 15, 18)) ~ "1",
    burn.cohort == "B" & year %in% (2000 + c(10, 13, 16)) ~ "2",
    burn.cohort == "C" & year %in% (2000 + c(7, 10, 13, 16)) ~ "0",
    burn.cohort == "C" & year %in% (2000 + c(8, 11, 14, 17)) ~ "1",
    burn.cohort == "C" & year %in% (2000 + c(9, 12, 15, 18)) ~ "2",
    patch == "235-C" & year == 2016 ~ "0",
    patch == "235-C" & year %in% (2000 + c(14, 17)) ~ "1",
    patch == "235-C" & year %in% (2000 + c(15, 18)) ~ "2",
    patch == "235-N" & year %in% (2000 + c(14, 17)) ~ "0",
    patch == "235-N" & year %in% (2000 + c(15, 18)) ~ "1",
    patch == "235-N" & year == 2016 ~ "2",
    patch == "235-S" & year %in% (2000 + c(15, 18)) ~ "0",
    patch == "235-S" & year == 2016 ~ "1",
    patch == "235-S" & year %in% (2000 + c(14, 17)) ~ "2",
    site == "GIL" & year %in% (2000 + c(9, 12, 15)) ~ "0",
    site == "GIL" & year %in% (2000 + c(10, 13, 16)) ~ "1",
    site == "GIL" & year %in% (2000 + c(11, 14, 17)) ~ "2",
    site == "GIL" & year == 2018 ~ "3",
    site %in% c("KLT", "RIN") & year %in% (2000 + c(9, 12)) ~ "0",
    site %in% c("KLT", "RIN") & year %in% (2000 + c(10, 13)) ~ "1",
    site %in% c("KLT", "RIN") & year %in% (2000 + c(11, 14)) ~ "2",
    site %in% c("KLT", "RIN") & year == 2015 ~ "3",
    site == "KLT" & year == 2016 ~ "0",
    site == "KLT" & year == 2017 ~ "1",
    site == "KLT" & year == 2018 ~ "2",
    site == "RIN" & year == 2016 ~ "4",
    site == "RIN" & year == 2017 ~ "0",
    site == "RIN" & year == 2018 ~ "1",
    site == "PAW" & year %in% (2000 + c(8, 13, 17)) ~ "0",
    site == "PAW" & year %in% (2000 + c(9, 14, 18)) ~ "1",
    site == "PAW" & year %in% (2000 + c(7, 10, 15)) ~ "2",
    site == "PAW" & year %in% (2000 + c(11, 16)) ~ "3",
    site == "PAW" & year %in% (2000 + c(12)) ~ "4",
    site == "RCH" & year %in% (2000 + c(9, 12)) ~ "0",
    site == "RCH" & year %in% (2000 + c(10, 13)) ~ "1",
    site == "RCH" & year == 2011 ~ "2",
    patch == "RCH-C" & year %in% (2000 + c(15, 18)) ~ "0",
    patch == "RCH-C" & year == 2016 ~ "1",
    patch == "RCH-C" & year %in% (2000 + c(14, 17)) ~ "2",
    patch == "RCH-E" & year == 2016 ~ "0",
    patch == "RCH-E" & year == 2017 ~ "1",
    patch == "RCH-E" & year %in% (2000 + c(14, 18)) ~ "2",
    patch == "RCH-E" & year == 2015 ~ "3",
    patch == "RCH-W" & year %in% (2000 + c(14, 17)) ~ "0",
    patch == "RCH-W" & year %in% (2000 + c(15, 18)) ~ "1",
    patch == "RCH-W" & year == 2016 ~ "2",
    site == "STE" & year %in% (2000 + c(9, 12, 16)) ~ "0",
    site == "STE" & year %in% (2000 + c(10, 13, 17)) ~ "1",
    site == "STE" & year %in% (2000 + c(11, 14, 18)) ~ "2",
    site == "STE" & year == 2015 ~ "3",
    TRUE ~ "unknown"), .after = management_anti.fescue) %>% 
  dplyr::select(-burn.cohort)

vst_v07 %>% 
  filter(year >= 2014 & site == "RCH") %>% 
  select(site, patch) %>% distinct()

# Check management entries
vst_v07 %>% 
  dplyr::group_by(management_pyric.herbivory) %>% 
  dplyr::summarize(sites = paste(sort(unique(site)), collapse = "; "),
    .groups = "drop")

# Check anti-fescue management entries
vst_v07 %>% 
  dplyr::filter(year >= 2014) %>% 
  dplyr::group_by(management_anti.fescue) %>% 
  dplyr::summarize(sites = paste(sort(unique(site)), collapse = "; "),
    .groups = "drop")

# Check TSF entries
(check_tsf <- vst_v07 %>% 
  dplyr::select(patch, year, time.since.fire_years) %>% 
  dplyr::distinct() %>% 
  tidyr::pivot_wider(names_from = year, values_from = time.since.fire_years))
# tibble::view(check_tsf)

# Check structure
dplyr::glimpse(vst_v07)

##  ------------------------------------------  ##
# Export ----
##  ------------------------------------------  ##

# Make a final object
vst_v99 <- vst_v07

# Check structure
dplyr::glimpse(vst_v99)

# Export this locally
write.csv(x = vst_v99, row.names = FALSE, na = "",
  file = file.path("data", "tidy", "01_tidy-visit.csv"))

##  ------------------------------------------  ##
# Identify "Observers" in Data ----
##  ------------------------------------------  ##

# Strip names from the data and count number of transects for each
obs <- vst_v99 %>% 
  dplyr::select(transect_id, dplyr::ends_with("observer")) %>% 
  tidyr::separate_wider_delim(cols = butterfly_observer, delim = "; ",
    names = c("bf_obs1", "bf_obs2"), cols_remove = TRUE, too_few = "align_start") %>% 
  tidyr::separate_wider_delim(cols = nectar_observer, delim = "; ",
    names = c("flr_obs1", "flr_obs2"), cols_remove = TRUE, too_few = "align_start") %>% 
  tidyr::pivot_longer(cols = dplyr::contains("_obs"),
    names_to = "x", values_to = "name") %>% 
  dplyr::mutate(type = ifelse(stringr::str_detect(string = x, pattern = "flr_"),
    yes = "flower", no = "butterfly")) %>% 
  dplyr::filter(!is.na(name)) %>% 
  dplyr::group_by(name, type) %>% 
  dplyr::summarize(transect_count = length(unique(transect_id)),
    .groups = "drop") %>% 
  tidyr::pivot_wider(names_from = type, values_from = transect_count, values_fill = 0) %>% 
  dplyr::mutate(total = butterfly + flower) %>% 
  dplyr::arrange(dplyr::desc(total)) %>% 
  dplyr::mutate(global.total = sum(total, na.rm = TRUE)) %>% 
  dplyr::mutate(percent.of.total = round(x = ((total / global.total) * 100), digits = 1)) %>% 
  dplyr::select(-global.total)

# Check that out
dplyr::glimpse(obs)

# Export it
write.csv(x = obs, row.names = FALSE, na = "",
  file = file.path("data", "01_observer-credit.csv"))

# End ----
