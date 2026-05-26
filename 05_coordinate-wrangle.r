##  ------------------------------------------------------------  ##
# Butterfly Project - Identify Coordinate Information ----
##  ------------------------------------------------------------  ##
# Purpose:
## Conditionally create a table of coordinates

# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse)

# Get set up
source("-setup.r")

# Clear environment
rm(list = ls()); gc()

##  ------------------------------------------  ##
# Load Data ----
##  ------------------------------------------  ##
# Read in the relevant data
coord_v01 <- read.csv(file.path("data", "01_tidy-visit.csv"))

# Check structure
dplyr::glimpse(coord_v01)

##  ------------------------------------------  ##
# Streamline Data ----
##  ------------------------------------------  ##

# Pare down to just what we need for this
coord_v02 <- coord_v01 %>% 
  dplyr::select(site) %>% 
  dplyr::distinct() %>% 
  # dplyr::filter(stringr::str_detect(string = site, pattern = "SS\\.") != TRUE) %>% 
  # dplyr::filter(!site %in% c("JER")) %>% 
  dplyr::arrange(site)

# Check structure
dplyr::glimpse(coord_v02)

##  ------------------------------------------  ##
# Add Coordinates ----
##  ------------------------------------------  ##

# Conditionally identify coordinates
coord_v03 <- coord_v02 %>% 
  dplyr::mutate(longitude_dd = dplyr::case_when(
    site == "BSH" ~ -94.06008867,
    site == "DUN" ~ -94.10406148,
    site == "GIL" ~ -94.12317923,
    site == "KLN" ~ -94.09423799,
    site == "KLT" ~ -94.10530547,
    site == "LTR" ~ -94.1389705,
    site == "PAW" ~ -94.14336075,
    site == "PYN" ~ -94.17191999,
    site == "PYS" ~ -94.17062042,
    site == "PYW" ~ -94.17587332,
    site == "RC2" ~ -94.13078464,
    site == "RIN" ~ -94.13669142,
    site == "RIS" ~ -94.13677001,
    site == "STE" ~ -94.11307227,
    TRUE ~ NA)) %>% 
  dplyr::mutate(latitude_dd = dplyr::case_when(
    site == "BSH" ~ 40.69094297,
    site == "DUN" ~ 40.50381833,
    site == "GIL" ~ 40.58431584,
    site == "KLN" ~ 40.6927233,
    site == "KLT" ~ 40.67204249,
    site == "LTR" ~ 40.57903529,
    site == "PAW" ~ 40.51903025,
    site == "PYN" ~ 40.58089478,
    site == "PYS" ~ 40.57683607,
    site == "PYW" ~ 40.57859344,
    site == "RC2" ~ 40.61562757,
    site == "RIN" ~ 40.60074146,
    site == "RIS" ~ 40.59282213,
    site == "STE" ~ 40.5897648,
    TRUE ~ NA))

# Check structure
dplyr::glimpse(coord_v03)

##  ------------------------------------------  ##
# Export ----
##  ------------------------------------------  ##

# Make a final object
coord_v99 <- coord_v03

# Check structure
dplyr::glimpse(coord_v99)

# Export it
write.csv(x = coord_v99, row.names = FALSE, na = "",
  file = file.path("data", "05_site-coordinates.csv"))

# End ----
