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
# Load Pre-2015 Data ----
##  ------------------------------------------  ##
# Read in the relevant data
coord_v01a <- readxl::read_excel(path = file.path("data", "raw", "GRG Site Coords.xls"))

# Check structure
dplyr::glimpse(coord_v01a)

##  ------------------------------------------  ##
# Load Post-2015 Data ----
##  ------------------------------------------  ##
# Read in the relevant data
coord_v01b <- readxl::read_excel(path = file.path("data", "raw", "GRG Site Coords 2015.xls"))

# Check structure
dplyr::glimpse(coord_v01b)

##  ------------------------------------------  ##
# Combine Versions ----
##  ------------------------------------------  ##

# Stack 'em up
coord_v01 <- dplyr::bind_rows(coord_v01a, coord_v01b) %>% 
  dplyr::distinct()

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
  dplyr::rename(site.raw = site) %>% 
  dplyr::filter(pasture != "Bee" & unit != "Bee")

# Check structure
dplyr::glimpse(coord_v02)

##  ------------------------------------------  ##
# Extract Site/Patch Names ----
##  ------------------------------------------  ##

# Standardize site/patch names
coord_v03 <- coord_v02 %>% 
  dplyr::mutate(site = dplyr::case_when(
    site.raw == "Besh" ~ "BSH",
    site.raw == "Frank North" ~ "FRN",
    site.raw == "Kellerton 235" ~ "235",
    site.raw == "Kell Tauke" ~ "KLT",
    site.raw == "Kellerton N" ~ "KLN",
    site.raw == "Lee Trail Rd" ~ "LTR",
    site.raw == "Richardson" ~ "RCH",
    site.raw == "Richardson2" ~ "RC2",
    site.raw == "Ringgold S" ~ "RIS",
    site.raw == "Pyland" ~ paste0("PY", stringr::str_sub(pasture, 1, 1)),
    TRUE ~ toupper(stringr::str_sub(site.raw, 1, 3))), .before = site.raw) %>% 
  dplyr::mutate(site = dplyr::case_when(
    site == "RCH" & unit %in% c("West", "North", "South") ~ "RCH.2007",
    site == "RCH" & unit %in% c("Y", "Center", "East") ~ "RCH.2014",
    TRUE ~ site)) %>% 
  dplyr::mutate(patch = dplyr::case_when(
    site.raw == "Pyland" ~ paste0(site, "-", stringr::str_sub(unit, 3, 3)),
    TRUE ~ paste0(site, "-", stringr::str_sub(pasture, 1, 1))),
    .after = site) %>% 
  dplyr::select(-site.raw, -pasture, -unit) %>% 
  dplyr::distinct()

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
# Ditch Missing Coordinates ----
##  ------------------------------------------  ##

# Remove any missing coordinates
coord_v05 <- coord_v04 %>% 
  dplyr::filter(!is.na(longitude) & !is.na(latitude))

# What is lost?
supportR::diff_check(old = unique(coord_v04$whittaker), new = unique(coord_v05$whittaker))
## BSH, DUN, and FRN do not have a second transect on any patch

# Check structure
dplyr::glimpse(coord_v05)

##  ------------------------------------------  ##
# Translate UTM to Lat/Long ----
##  ------------------------------------------  ##

# Need to convert UTMs to Lat/Long
coord_v06 <- coord_v05 %>% 
  sf::st_as_sf(x = ., coords = c("longitude", "latitude"),
               crs = "+proj=utm +zone=15") %>% 
  sf::st_transform(x = ., crs = sf::st_crs(4326)) %>% 
  dplyr::mutate(longitude_dd = sf::st_coordinates(x = .)[,1],
                latitude_dd = sf::st_coordinates(x = .)[,2]) %>% 
  sf::st_drop_geometry(x = .)

# Check structure
dplyr::glimpse(coord_v06)

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
  dplyr::filter(!patch %in% coord_v06$patch)

# Re-check structure
dplyr::glimpse(miss_v02)

# Attach that to the existing coordinates
coord_v07 <- dplyr::bind_rows(coord_v06, miss_v02)

# Check for gained/lost sites
supportR::diff_check(old = coord_v07$site, new = coord_v06$site)

# Check structure
dplyr::glimpse(coord_v07)

##  ------------------------------------------  ##
# Conditionally Add Coordinates ----
##  ------------------------------------------  ##

# What is missing?
coord_v07 %>% 
  dplyr::filter(is.na(longitude_dd) | is.na(latitude_dd)) %>% 
    dplyr::select(site, patch) %>% 
    dplyr::filter(stringr::str_detect(site, "SS\\.") != TRUE) %>% 
    dplyr::arrange(patch) %>% 
    dplyr::distinct()

# If known, add coordinates manually
coord_v08 <- coord_v07
  ## No missing coordinates known _a priori_

# Check what's still unknown
coord_v08 %>% 
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
