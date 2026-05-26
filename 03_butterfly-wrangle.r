##  ------------------------------------------------------------  ##
# Butterfly Project - Wrangle Butterfly Data
##  ------------------------------------------------------------  ##
# Purpose:
## Wrangle butterfly data (e.g., do quality control, calculate metrics)

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
bf_v01 <- readxl::read_excel(path = bfly_file, sheet = "Butterfly")

# Check structure
dplyr::glimpse(bf_v01)

##  ------------------------------------------  ##
# Streamline Column Names ----
##  ------------------------------------------  ##

# Get the column names in a better format
bf_v02 <- bf_v01 %>% 
  dplyr::rename_with(.fn = tolower) %>% 
  dplyr::rename_with(.fn = ~ gsub(pattern = "\\.", replacement = "_", x = .)) %>% 
  dplyr::rename(butterfly_common = butterfly_common_name,
    butterfly_scientific = butterfly_species,
    confidence = id_confidence,
    transect.perpendicular.distance_m = distance_m,
    within.5m.of.observer = outside_transect,
    nectar_common = nectar_common_name,
    nectar_scientific = nectar_species,
    notes = comments)

# Check new column names
names(bf_v02)

# Check structure
dplyr::glimpse(bf_v02)

##  ------------------------------------------  ##
# Check Categorical Columns ----
##  ------------------------------------------  ##

# Check categorical columns
sort(unique(bf_v02$site))
sort(unique(bf_v02$month))
sort(unique(bf_v02$round))
sort(unique(bf_v02$confidence))
sort(unique(bf_v02$activity))
sort(unique(bf_v02$within.5m.of.observer))
sort(unique(bf_v02$sex))

# Standardize and streamline those columns
bf_v03 <- bf_v02 %>% 
  dplyr::mutate(confidence = tolower(confidence)) %>% 
  dplyr::mutate(activity = tolower(activity),
    activity = gsub("flying\\/ basking", "flying/basking", x = activity))

# Re-check altered ones
sort(unique(bf_v03$confidence))
sort(unique(bf_v03$activity))

# Check structure
dplyr::glimpse(bf_v03)

##  ------------------------------------------  ##
# Check Taxonomic Columns ----
##  ------------------------------------------  ##

# Check taxonomic columns
sort(unique(bf_v03$butterfly_common))
## Note that "question mark" is a species of butterfly (_Polygonia interrogationis_), not an unknown butterfly
sort(unique(bf_v03$nectar_common))

# Make needed repairs
bf_v04 <- bf_v03 %>% 
  dplyr::mutate(butterfly_common = dplyr::case_when(
    butterfly_common %in% c("unknown cloudywing", "unknown duskywing", "unknown large skipper") ~ "unknown skipper",
    butterfly_common %in% c("unknown comma", "unknown lady") ~ "unknown brush-foot",
    butterfly_common %in% c("unknown gossamer-wing") ~ "unknown lycaenid",
    TRUE ~ butterfly_common)) %>% 
  dplyr::mutate(nectar_common = dplyr::case_when(
    nectar_common %in% c("tall swamp vervain (swamp vervain)") ~ "swamp vervain",    
    TRUE ~ nectar_common))

# What was gained/lost?
supportR::diff_check(old = unique(bf_v03$butterfly_common), new = unique(bf_v04$butterfly_common))
supportR::diff_check(old = unique(bf_v03$nectar_common), new = unique(bf_v04$nectar_common))

# Check structure
dplyr::glimpse(bf_v04)

##  ------------------------------------------  ##
# Remove Unwanted Rows/Columns
##  ------------------------------------------  ##

# Remove any rows/columns that are not likely to be useful
bf_v05 <- bf_v04 %>% 
  dplyr::filter(!butterfly_common %in% c("accidental row", "none", "unknown butterfly")) %>% 
  dplyr::filter(stringr::str_detect(string = butterfly_common, pattern = "unknown") != TRUE) %>% 
  dplyr::select(-month, -round, -butterfly_id, -confidence:-notes)

# What was lost?
message(nrow(bf_v04) - nrow(bf_v05), " rows removed")
supportR::diff_check(old = names(bf_v04), new = names(bf_v05))

# Structure check
dplyr::glimpse(bf_v05)

##  ------------------------------------------  ##
# Fix Dates ----
##  ------------------------------------------  ##

# Do needed repairs to get date in reasonable format
bf_v06 <- bf_v05 %>% 
  tidyr::separate_wider_delim(cols = date, delim = ".",
    names = c("month", "day"), cols_remove = TRUE) %>%
  dplyr::mutate(month = as.numeric(month),
    day = as.numeric(day)) %>% 
  dplyr::relocate(month, day, .after = year) %>%
  dplyr::mutate(year = year + 2000) %>% 
  dplyr::mutate(date = as.Date(paste(day, month, year, sep = "-"), format = "%d-%m-%Y"),
    .after = day)

# Check structure
dplyr::glimpse(bf_v06)

##  ------------------------------------------  ##
# Summarize Within Species ----
##  ------------------------------------------  ##

# Summarize within butterfly species
bf_v07 <- bf_v06 %>% 
  dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(bf_v06), y = c("number"))))) %>% 
  dplyr::summarize(count = sum(number, na.rm = TRUE),
    .groups = "drop")

# How much does this simplify the data?
message(nrow(bf_v06) - nrow(bf_v07), " rows lost")
supportR::diff_check(old = names(bf_v06), new = names(bf_v07))

# Check structure
dplyr::glimpse(bf_v07)

##  ------------------------------------------  ##
# Create Taxonomic Table ----
##  ------------------------------------------  ##

# Get common-to-scientific name table so that the 'actual' data can be simpler
bf_taxa <- bf_v07 %>% 
  dplyr::select(butterfly_common, butterfly_scientific) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(species = paste0(toupper(stringr::str_sub(butterfly_scientific, start = 1, end = 1)),
    stringr::str_sub(butterfly_scientific, start = 2, end = nchar(butterfly_scientific)))) %>% 
  tidyr::separate_wider_delim(cols = species, names = c("genus", "specific.epithet"),
    delim = " ", cols_remove = FALSE) %>% 
  dplyr::select(-butterfly_scientific) %>% 
  dplyr::rename(common.name = butterfly_common) %>% 
  dplyr::relocate(species, .before = genus) %>% 
  dplyr::arrange(species)

# Check structure
dplyr::glimpse(bf_taxa)

##  ------------------------------------------  ##
# Export ----
##  ------------------------------------------  ##

# Make a final object & ditch columns in other tables
 bf_v99 <- bf_v07 %>% 
  dplyr::select(-year:-whittaker, -butterfly_scientific) %>% 
  dplyr::rename(common.name = butterfly_common)

# Check structure
dplyr::glimpse(bf_v99)

# Export this locally
write.csv(x = bf_v99, row.names = FALSE, na = "",
  file = file.path("data", "03_tidy-butterfly.csv"))

# Export taxaonomic table as well
write.csv(x = bf_taxa, row.names = FALSE, na = "",
  file = file.path("data", "03_butterfly-taxa.csv"))

# End ----
