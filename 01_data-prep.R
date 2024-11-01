##  ------------------------------------------------------------  ##
                # Butterfly Project - Data Prep
##  ------------------------------------------------------------  ##
# Written by Nick J Lyon

# Script purpose:
## Do some generally-useful wrangling on these data
## This is pre-requisite to statistics and visualization

##  ------------------------------------------  ##      
              # Housekeeping ----
##  ------------------------------------------  ##      

# Set required libraries
# install.packages("librarian")
librarian::shelf(tidyverse)

# Clear environment & collect garbage
rm(list = ls()); gc()

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)

##  ------------------------------------------  ##      
              # Butterfly Prep ----
##  ------------------------------------------  ##      

# Read in butterfly data
bf_v1 <- read.csv(file = file.path("data", "butterfly-project_tidy-butterflies.csv"))

# Check structure
dplyr::glimpse(bf_v1)

# Summarize to relevant spatial scale (patch-level)
bf_v2 <- bf_v1 %>%
  # Remove the 'none' treatment (only applies to three sites in one year)
  dplyr::filter(adaptive.mgmt != "none") %>% 
  # Add context back in for the 'graze and burn' sights where invasive control was implemented
  dplyr::mutate(adaptive.mgmt = dplyr::case_when(
    adaptive.mgmt == "graze and burn" & herbicide.treatment != "none" ~ "graze and burn and invasive control",
    T ~ adaptive.mgmt)) %>% 
  # Summarizing step
  dplyr::group_by(year, pasture, patch, adaptive.mgmt, butterfly.common) %>% 
  dplyr::summarize(butterfly.count = sum(butterfly.count, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# Check structure
dplyr::glimpse(bf_v2)

# Calculate community metrics
bf_v3 <- bf_v2 %>% 
  ## Abundance / richness
  dplyr::group_by(year, pasture, patch) %>% 
  dplyr::mutate(
    butterfly.abundance = sum(butterfly.count, na.rm = T),
    butterfly.richness = length(unique(butterfly.common))) %>% 
  dplyr::ungroup() %>% 
  ## Diversity
  dplyr::mutate(div.temp_1 = butterfly.count / butterfly.abundance,
                div.temp_2 = log(div.temp_1),
                div.temp_3 = div.temp_1 * div.temp_2) %>% 
  dplyr::group_by(year, pasture, patch) %>% 
  dplyr::mutate(butterfly.diversity_shannon = sum(div.temp_3, na.rm = T) * -1) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-dplyr::starts_with("div.temp_"))

# Check structure
dplyr::glimpse(bf_v3)

sort(unique(bf_v3$adaptive.mgmt))

# Final tweaks & reshape to wide format
bf_v4 <- bf_v3 %>% 
  dplyr::mutate(butterfly.common = gsub(pattern = " |-|'", replacement = ".",
                                        x = butterfly.common)) %>% 
  dplyr::mutate(adaptive.mgmt.abbrev = dplyr::case_when(
    adaptive.mgmt == "burn only" ~ "BO",
    adaptive.mgmt == "graze and burn" ~ "GB",
    adaptive.mgmt == "graze and burn and invasive control" ~ "GB-IC",
    adaptive.mgmt == "invasive control" ~ "IC",
    adaptive.mgmt == "patch burn graze" ~ "PBG",
    T ~ adaptive.mgmt),
    .after = adaptive.mgmt) %>% 
  tidyr::pivot_wider(names_from = butterfly.common,
                     values_from = butterfly.count, values_fill = 0)

# Final structure check
dplyr::glimpse(bf_v4)

# Export
write.csv(x = bf_v4, row.names = F, na = '',
          file = file.path("data", "ready-butterflies.csv"))
  
##  ------------------------------------------  ##      
# Flower Prep ----
##  ------------------------------------------  ##      




# End ----
