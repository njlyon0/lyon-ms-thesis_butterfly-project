##  ------------------------------------------------------------  ##
# Butterfly Project - Wrangle Management Information
##  ------------------------------------------------------------  ##
# Purpose:
## Integrate management information into the data

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

# Read in the relevant data
mgmt_v01 <- read.csv(file.path("data", "01_tidy-visit.csv"))

# Check structure
dplyr::glimpse(mgmt_v01)

##  ------------------------------------------  ##
# Streamline Data ----
##  ------------------------------------------  ##

# Only site, patch, and year are necessary for identifying management methods
mgmt_v02 <- mgmt_v01 %>% 
  dplyr::select(year, site, patch) %>% 
  dplyr::distinct()

# Check structure
dplyr::glimpse(mgmt_v02)

##  ------------------------------------------  ##
# Get Pyric-Herbivory Management Info ----
##  ------------------------------------------  ##

# Identify fire-grazing combination management methods
mgmt_v03 <- mgmt_v02 %>% 
  dplyr::mutate(management_pyric.herbivory = dplyr::case_when(
    site %in% c("KLT", "RIN", "PAW") ~ "burn only",
    year < 2014 & site == "RCH" ~ "burn only",
    site %in% c("GIL", "LTR", "PYW", "STE") ~ "graze and burn",
    site %in% c("235", "KLN", "PYN", "PYS", "RIS") ~ "patch-burn graze",
    year >= 2014 & site == "RCH" ~ "patch-burn graze",
    site %in% c("BSH", "DUN", "RC2") ~ "none",
    TRUE ~ "unknown"))

# Check pyric-herbivory management entries
mgmt_v03 %>% 
  dplyr::group_by(management_pyric.herbivory) %>% 
  dplyr::summarize(sites = paste(sort(unique(site)), collapse = "; "),
    .groups = "drop")

# Check structure
dplyr::glimpse(mgmt_v03)

##  ------------------------------------------  ##
# Get Anti-Fescue Management Info ----
##  ------------------------------------------  ##

# Identify anti-fescue management methods (experiment started in 2014)
mgmt_v04 <- mgmt_v03 %>% 
  dplyr::mutate(management_anti.fescue = dplyr::case_when(
    year <= 2013 ~ "none",
    patch %in% c("BSH-N", "DUN-C", "GIL-S", "LTR-W", "PYW-S", "RC2-S", "STE-W") ~ "control",
    patch %in% c("BSH-C", "DUN-E", "GIL-N", "LTR-C", "PYW-C", "RC2-C", "STE-N") ~ "herbicide only",
    patch %in% c("BSH-S", "DUN-W", "GIL-C", "LTR-E", "PYW-N", "RC2-N", "STE-S") ~ "herbicide and seedmix addition",
    TRUE ~ "none"))

# Check anti-fescue management entries
mgmt_v04 %>% 
  dplyr::filter(year >= 2014) %>% 
  dplyr::group_by(management_anti.fescue) %>% 
  dplyr::summarize(sites = paste(sort(unique(patch)), collapse = "; "),
    .groups = "drop")

# Check structure
dplyr::glimpse(mgmt_v04)

##  ------------------------------------------  ##
# Get Burn History Info ----
##  ------------------------------------------  ##

# Identify years since fire for all sites/patches
mgmt_v05 <- mgmt_v04 %>% 
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
    TRUE ~ "unknown")) %>% 
  dplyr::select(-burn.cohort)

# Check TSF entries
(check_tsf <- mgmt_v05 %>% 
  dplyr::select(patch, year, time.since.fire_years) %>% 
  dplyr::distinct() %>% 
  tidyr::pivot_wider(names_from = year, values_from = time.since.fire_years))
# tibble::view(check_tsf)

# Check structure
dplyr::glimpse(mgmt_v05)

##  ------------------------------------------  ##
# Export ----
##  ------------------------------------------  ##

# Make a final object
mgmt_v99 <- mgmt_v05

# Check structure
dplyr::glimpse(mgmt_v99)

# Export this locally
write.csv(x = mgmt_v99, row.names = FALSE, na = "",
  file = file.path("data", "02_tidy-management.csv"))

# End ----
