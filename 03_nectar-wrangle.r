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
flr_v01 <- readxl::read_excel(path = bfly_file, sheet = "Floral")

# Check structure
dplyr::glimpse(flr_v01)

##  ------------------------------------------  ##
# Streamline Column Names ----
##  ------------------------------------------  ##

# Get the column names in a better format
flr_v02 <- flr_v01 %>% 
  dplyr::rename_with(.fn = tolower) %>% 
  dplyr::rename_with(.fn = ~ gsub(pattern = "\\.", replacement = "_", x = .)) %>% 
  dplyr::rename(nectar_common = nectar_common_name,
    nectar_scientific = nectar_species)

# Check new column names
names(flr_v02)

# Check structure
dplyr::glimpse(flr_v02)

##  ------------------------------------------  ##
# Check Categorical Columns ----
##  ------------------------------------------  ##

# Check categorical columns
sort(unique(flr_v02$site))
sort(unique(flr_v02$month))
sort(unique(flr_v02$round))

# Standardize and streamline those columns
flr_v03 <- flr_v02
## No such wrangling needed

# Check structure
dplyr::glimpse(flr_v03)

##  ------------------------------------------  ##
# Check Taxonomic Columns ----
##  ------------------------------------------  ##

# Check taxonomic columns
sort(unique(flr_v03$nectar_common))

# Make needed repairs
flr_v04 <- flr_v03
## No such wrangling needed

# What was gained/lost?
supportR::diff_check(old = unique(flr_v03$nectar_common), new = unique(flr_v04$nectar_common))

# Check structure
dplyr::glimpse(flr_v04)

##  ------------------------------------------  ##
# Remove Unwanted Rows/Columns
##  ------------------------------------------  ##

# Remove any rows/columns that are not likely to be useful
flr_v05 <- flr_v04 %>% 
  dplyr::filter(!nectar_common %in% c("accidental row", "no nectar plants")) %>% 
  dplyr::filter(stringr::str_detect(string = nectar_common, pattern = "unknown") != TRUE) %>% 
  dplyr::select(-month, -round, -nectar_id)

# What was lost?
message(nrow(flr_v04) - nrow(flr_v05), " rows removed")
supportR::diff_check(old = unique(flr_v04$nectar_common), new = unique(flr_v05$nectar_common))
supportR::diff_check(old = names(flr_v04), new = names(flr_v05))

# Structure check
dplyr::glimpse(flr_v05)

##  ------------------------------------------  ##
# Fix Dates ----
##  ------------------------------------------  ##

# Do needed repairs to get date in reasonable format
flr_v06 <- flr_v05 %>% 
  tidyr::separate_wider_delim(cols = date, delim = ".",
    names = c("month", "day"), cols_remove = TRUE) %>%
  dplyr::mutate(month = as.numeric(month),
    day = as.numeric(day)) %>% 
  dplyr::relocate(month, day, .after = year) %>%
  dplyr::mutate(year = year + 2000) %>% 
  dplyr::mutate(date = as.Date(paste(day, month, year, sep = "-"), format = "%d-%m-%Y"),
    .after = day)

# Check structure
dplyr::glimpse(flr_v06)

##  ------------------------------------------  ##
# Summarize Within Species ----
##  ------------------------------------------  ##

# Summarize within butterfly species
flr_v07 <- flr_v06 %>% 
  dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(flr_v06), y = c("number"))))) %>% 
  dplyr::summarize(inflorescence_count = sum(number, na.rm = TRUE),
    .groups = "drop")

# How much does this simplify the data?
message(nrow(flr_v06) - nrow(flr_v07), " rows lost")
supportR::diff_check(old = names(flr_v06), new = names(flr_v07))

# Check structure
dplyr::glimpse(flr_v07)

##  ------------------------------------------  ##
# Identify Seedmix Species ----
##  ------------------------------------------  ##

# Conditionally identify which plant species were part of a restoration seedmix applied in 2015
flr_v08 <- flr_v07 %>% 
  dplyr::mutate(in.2015.seedmix = dplyr::case_when(
    nectar_common %in% tolower(c("Lead plant", "Swamp Milkweed", "Common Milkweed", "Butterfly Milkweed", 
      "White Wild Indigo", "Prairie Coreopsis", "Tall Coreopsis", "Purple Prairie Clover", 
      "Illinois bundleflower", "Showy Tick Trefoil", "Prairie Cinquefoil", "Pale Purple Coneflower", 
      "Purple Coneflower", "Rattlesnake Master", "Tall Boneset", "Oxeye Sunflower", "Alum root", 
      "Spotted St. John's Wort", "Round-headed Bush Clover", "Cylindrical blazing star", 
      "Prairie Blazing Star", "Cardinal flower", "Great blue lobelia", "Pale Spike Lobelia", 
      "Wild Bergamot", "Primrose", "Wild Quinine", "Slender Mountain Mint", "Grey-Headed Coneflower", 
      "Black-Eyed Susan", "Sweet Black-Eyed Susan", "Prairie Petunia", "Rosinweed", "Cup plant", 
      "Blue-Eyed Grass", "Stiff goldenrod", "Sky-blue aster", "Silky aster", "Germander", 
      "Spiderwort", "Ironweed", "Culver's Root", "Golden Alexander")) ~ "in seedmix",
    TRUE ~ "not"), .after = nectar_scientific)

# Check that worked
flr_v08 %>% 
  dplyr::filter(in.2015.seedmix != "not") %>% 
  dplyr::pull(nectar_common) %>% unique() %>% sort()

# Check structure
dplyr::glimpse(flr_v08)

##  ------------------------------------------  ##
# Export ----
##  ------------------------------------------  ##

# Make a final object
flr_v99 <- flr_v08

# Check structure
dplyr::glimpse(flr_v99)

# Export this locally
write.csv(x = flr_v99, row.names = FALSE, na = "",
  file = file.path("data", "tidy", "02_tidy-nectar.csv"))

# End ----
