##  ------------------------------------------------------------  ##
# Butterfly Project - Wrangle Observerer Information
##  ------------------------------------------------------------  ##
# Purpose:
## Extract observer contributions from the visit data

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
obs_v01 <- read.csv(file.path("data", "01_tidy-visit.csv"))

# Check structure
dplyr::glimpse(obs_v01)

##  ------------------------------------------  ##
# Streamline Data ----
##  ------------------------------------------  ##

# Pare down the data to only needed columns
obs_v02 <- obs_v01 %>% 
  dplyr::select(transect_id, dplyr::ends_with("observer")) %>% 
  tidyr::separate_wider_delim(cols = butterfly_observer, delim = "; ",
    names = c("bf_obs1", "bf_obs2"), cols_remove = TRUE, too_few = "align_start") %>% 
  tidyr::separate_wider_delim(cols = nectar_observer, delim = "; ",
    names = c("flr_obs1", "flr_obs2"), cols_remove = TRUE, too_few = "align_start")

# Check structure
dplyr::glimpse(obs_v02)

##  ------------------------------------------  ##
# Count Transects Per Observer ----
##  ------------------------------------------  ##

# Summarize to actually count the number of transects per observer
obs_v03 <- obs_v02 %>% 
  tidyr::pivot_longer(cols = dplyr::contains("_obs"),
    names_to = "x", values_to = "name") %>% 
  dplyr::mutate(type = ifelse(stringr::str_detect(string = x, pattern = "flr_"),
    yes = "flower", no = "butterfly")) %>% 
  dplyr::filter(!is.na(name) & nchar(name) > 0) %>% 
  dplyr::group_by(name, type) %>% 
  dplyr::summarize(transect_count = length(unique(transect_id)),
    .groups = "drop")

# Check structure
dplyr::glimpse(obs_v03)

##  ------------------------------------------  ##
# Improve Output Format ----
##  ------------------------------------------  ##

# Do some mostly cosmetic edits to get the data looking nice for export
obs_v04 <- obs_v03 %>% 
  tidyr::pivot_wider(names_from = type, values_from = transect_count, values_fill = 0) %>% 
  dplyr::mutate(total = butterfly + flower) %>% 
  dplyr::arrange(dplyr::desc(total)) %>% 
  dplyr::mutate(global.total = sum(total, na.rm = TRUE)) %>% 
  dplyr::mutate(percent.of.total = round(x = ((total / global.total) * 100), digits = 1)) %>% 
  dplyr::select(-global.total) %>% 
  dplyr::rename(observer = name,
    nectar.transects_count = flower,
    butterfly.transects_count = butterfly,
    total.transects_count = total)

# Check that out
dplyr::glimpse(obs_v04)

##  ------------------------------------------  ##
# Export ----
##  ------------------------------------------  ##

# Make a final object
obs_v99 <- obs_v04

# Check structure
dplyr::glimpse(obs_v99)

# Export it
write.csv(x = obs_v99, row.names = FALSE, na = "",
  file = file.path("data", "06_observer-credit.csv"))

# End ----
