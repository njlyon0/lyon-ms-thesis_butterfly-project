##  ------------------------------------------------------------  ##
# Butterfly Project - Identify Coordinate Information ----
##  ------------------------------------------------------------  ##
# Purpose:
## Conditionally create a table of coordinates

# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, sf)

# Get set up
source("-setup.r")

# Clear environment
rm(list = ls()); gc()

##  ------------------------------------------  ##
# Load Data ----
##  ------------------------------------------  ##
# Read in the relevant data
coord_v01 <- readxl::read_excel(path = file.path("data", "raw", "GRG Site Coords.xls"))

# Check structure
dplyr::glimpse(coord_v01)

##  ------------------------------------------  ##
# Make Data Tidy ----
##  ------------------------------------------  ##

# Data are malformed and need to be put into correct rows/columns
coord_v02 <- coord_v01 %>% 
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0))) %>% 
  dplyr::select(-dplyr::all_of(c(paste0("...", 15:17)))) %>% 
  dplyr::rename(
    site = `Whittaker Botanical Plots -- Initial GPS Coordinates (15T, UTM)`,
    pasture = ...2,
    unit = ...3,
    whit1_north_easting = ...5,
    whit1_north_northing = ...6,
    whit1_south_easting = ...7,
    whit1_south_northing = ...8,
    whit2_north_easting = ...10,
    whit2_north_northing = ...11,
    whit2_south_easting = ...12,
    whit2_south_northing = ...13) %>% 
  dplyr::filter(!is.na(site) & !site %in% c("Site", "Notes:") & 
    stringr::str_detect(string = site, pattern = "of the ") != TRUE &
    stringr::str_detect(string = site, pattern = "end points") != TRUE &
    stringr::str_detect(string = site, pattern = "and 25 m") != TRUE) %>% 
  dplyr::mutate(across(.cols = dplyr::starts_with("whit"),
    .fns = as.numeric)) %>% 
  dplyr::rename(site.raw = site)

# Check structure
dplyr::glimpse(coord_v02)

##  ------------------------------------------  ##
# Extract Site/Patch Names ----
##  ------------------------------------------  ##

# Standardize site/patch names
coord_v03 <- coord_v02 %>% 
  dplyr::mutate(site = dplyr::case_when(
    site.raw == "Kell Tauke" ~ "KLT",
    site.raw == "Kellerton N" ~ "KLN",
    site.raw == "Lee Trail Rd" ~ "LTR",
    site.raw == "Richardson" ~ "RCH",
    site.raw == "Ringgold S" ~ "RIS",
    site.raw == "Pyland" ~ paste0("PY", stringr::str_sub(pasture, 1, 1)),
    TRUE ~ toupper(stringr::str_sub(site.raw, 1, 3))), .before = site.raw) %>% 
  dplyr::mutate(patch = dplyr::case_when(
    site.raw == "Pyland" ~ paste0(site, "-", stringr::str_sub(unit, 3, 3)),
    TRUE ~ paste0(site, "-", stringr::str_sub(pasture, 1, 1))),
    .after = site) %>% 
  dplyr::select(-site.raw, -pasture, -unit)

# Check output
coord_v03 %>% 
  dplyr::group_by(site) %>% 
  dplyr::summarize(patch_ct = length(unique(patch)),
    patch_id = paste(patch, collapse = " & "),
    .groups = "drop")

# Check structure
dplyr::glimpse(coord_v03)

##  ------------------------------------------  ##
# Reshape Data ----
##  ------------------------------------------  ##

# Get into long format and deal with 'multiple plots' issue
coord_v04 <- coord_v03 %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("whit")) %>% 
  tidyr::separate_wider_delim(cols = name, delim = "_",
    names = c("whit", "plot", "utm"), cols_remove = TRUE) %>% 
  dplyr::mutate(whittaker = paste0(patch, gsub("whit", "", whit)),
    .after = patch) %>% 
  dplyr::filter(plot != "south") %>% 
  dplyr::select(-whit, -plot) %>% 
  tidyr::pivot_wider(names_from = utm, values_from = value) %>% 
  dplyr::rename(longitude = easting,
    latitude = northing)

# Check structure
dplyr::glimpse(coord_v04)

##  ------------------------------------------  ##
# Translate UTM to Lat/Long ----
##  ------------------------------------------  ##

# Need to convert UTMs to Lat/Long
coord_v05 <- coord_v04 %>% 
  sf::st_as_sf(x = ., coords = c("longitude", "latitude"),
               crs = "+proj=utm +zone=15") %>% 
  sf::st_transform(x = ., crs = sf::st_crs(4326)) %>% 
  dplyr::mutate(longitude_dd = sf::st_coordinates(x = .)[,1],
                latitude_dd = sf::st_coordinates(x = .)[,2]) %>% 
  sf::st_drop_geometry(x = .)

# Check structure
dplyr::glimpse(coord_v05)

##  ------------------------------------------  ##
# Grab Missing Sites ----
##  ------------------------------------------  ##

# Read in visit data
miss_v01 <- read.csv(file.path("data", "01_tidy-visit.csv"))

# Check structure
dplyr::glimpse(miss_v01)

# Pare that down to just site/patch/whittaker and only those not already with coordinates
miss_v02 <- miss_v01 %>% 
  dplyr::select(site, patch, whittaker) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(!patch %in% coord_v05$patch)

# Re-check structure
dplyr::glimpse(miss_v02)

# Attach that to the existing coordinates
coord_v06 <- dplyr::bind_rows(coord_v05, miss_v02)

# Check for gained/lost sites
supportR::diff_check(old = coord_v06$site, new = coord_v05$site)

# Check structure
dplyr::glimpse(coord_v06)

##  ------------------------------------------  ##
# Conditionally Add Coordinates ----
##  ------------------------------------------  ##

# If known, add coordinates manually
coord_v07 <- coord_v06 %>% 
  dplyr::mutate(longitude_dd = dplyr::case_when(
    !is.na(longitude_dd) ~ longitude_dd,
    whittaker == "BSH-C1" ~ -94.06008867,
    whittaker == "DUN-C1" ~ -94.10406148,
    whittaker == "RC2-C1" ~ -94.13078464,
    TRUE ~ NA)) %>% 
  dplyr::mutate(latitude_dd = dplyr::case_when(
    !is.na(latitude_dd) ~ latitude_dd,
    whittaker == "BSH-C1" ~ 40.69094297,
    whittaker == "DUN-C1" ~ 40.50381833,
    whittaker == "RC2-C1" ~ 40.61562757,
    TRUE ~ NA))

# Check what's still unknown
coord_v07 %>% 
  dplyr::filter(is.na(longitude_dd) | is.na(latitude_dd)) %>% 
  dplyr::select(site, patch) %>% 
  dplyr::filter(stringr::str_detect(site, "SS\\.") != TRUE) %>% 
  dplyr::arrange(patch) %>% 
  dplyr::distinct()

# Check structure
dplyr::glimpse(coord_v07)

##  ------------------------------------------  ##
# Export ----
##  ------------------------------------------  ##

# Make a final object
coord_v99 <- coord_v07

# Check structure
dplyr::glimpse(coord_v99)

# Export it
write.csv(x = coord_v99, row.names = FALSE, na = "",
  file = file.path("data", "05_site-coordinates.csv"))

# End ----
