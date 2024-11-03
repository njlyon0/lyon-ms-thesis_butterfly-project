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
          # Full Data Prep - Flower ----
##  ------------------------------------------  ##      

# Read in flower data
flr_v1 <- read.csv(file = file.path("data", "butterfly-project_tidy-flowers.csv"))

# Check structure
dplyr::glimpse(flr_v1)

# Summarize to relevant spatial scale (site-level)
flr_v2 <- flr_v1 %>%
  # Remove the 'none' treatment (only applies to three sites in one year)
  dplyr::filter(adaptive.mgmt != "none") %>% 
  # Add context back in for the 'graze and burn' sights where invasive control was implemented
  dplyr::mutate(adaptive.mgmt = dplyr::case_when(
    adaptive.mgmt == "graze and burn" & herbicide.treatment != "none" ~ "graze and burn and invasive control",
    T ~ adaptive.mgmt)) %>% 
  # Summarizing step
  dplyr::group_by(year, pasture, adaptive.mgmt, flower.common) %>% 
  dplyr::summarize(flower.count = sum(flower.count, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# Check structure
dplyr::glimpse(flr_v2)

# Calculate community metrics
flr_v3 <- flr_v2 %>% 
  ## Abundance / richness
  dplyr::group_by(year, pasture) %>% 
  dplyr::mutate(
    flower.abundance = sum(flower.count, na.rm = T),
    flower.richness = length(unique(flower.common))) %>% 
  dplyr::ungroup() %>% 
  ## Diversity
  dplyr::mutate(div.temp_1 = flower.count / flower.abundance,
                div.temp_2 = log(div.temp_1),
                div.temp_3 = div.temp_1 * div.temp_2) %>% 
  dplyr::group_by(year, pasture) %>% 
  dplyr::mutate(flower.diversity_shannon = sum(div.temp_3, na.rm = T) * -1) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-dplyr::starts_with("div.temp_"))

# Check structure
dplyr::glimpse(flr_v3)

# Final tweaks & reshape to wide format
flr_v4 <- flr_v3 %>% 
  dplyr::mutate(flower.common = gsub(pattern = " |-|'", replacement = ".",
                                     x = flower.common)) %>% 
  dplyr::mutate(adaptive.mgmt.abbrev = dplyr::case_when(
    adaptive.mgmt == "burn only" ~ "BO",
    adaptive.mgmt == "graze and burn" ~ "GB",
    adaptive.mgmt == "graze and burn and invasive control" ~ "GB-IC",
    adaptive.mgmt == "invasive control" ~ "IC",
    adaptive.mgmt == "patch burn graze" ~ "PBG",
    T ~ adaptive.mgmt),
    .after = adaptive.mgmt) %>% 
  tidyr::pivot_wider(names_from = flower.common,
                     values_from = flower.count, values_fill = 0)

# Re-check structure
dplyr::glimpse(flr_v4)

# Perform any final subsetting
flr_v5 <- flr_v4 %>% 
  ## Drop 2007
  dplyr::filter(year > 2007)

# Export
write.csv(x = flr_v5, row.names = F, na = '',
          file = file.path("data", "ready-flowers.csv"))

##  ------------------------------------------  ##      
        # Full Data Prep - Butterfly ----
##  ------------------------------------------  ##      

# Read in butterfly data
bf_v1 <- read.csv(file = file.path("data", "butterfly-project_tidy-butterflies.csv"))

# Check structure
dplyr::glimpse(bf_v1)

# Summarize to relevant spatial scale (site-level)
bf_v2 <- bf_v1 %>%
  # Remove the 'none' treatment (only applies to three sites in one year)
  dplyr::filter(adaptive.mgmt != "none") %>% 
  # Add context back in for the 'graze and burn' sights where invasive control was implemented
  dplyr::mutate(adaptive.mgmt = dplyr::case_when(
    adaptive.mgmt == "graze and burn" & herbicide.treatment != "none" ~ "graze and burn and invasive control",
    T ~ adaptive.mgmt)) %>% 
  # Summarizing step
  dplyr::group_by(year, pasture, adaptive.mgmt, butterfly.common) %>% 
  dplyr::summarize(butterfly.count = sum(butterfly.count, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# Check structure
dplyr::glimpse(bf_v2)

# Calculate community metrics
bf_v3 <- bf_v2 %>% 
  ## Abundance / richness
  dplyr::group_by(year, pasture) %>% 
  dplyr::mutate(
    butterfly.abundance = sum(butterfly.count, na.rm = T),
    butterfly.richness = length(unique(butterfly.common))) %>% 
  dplyr::ungroup() %>% 
  ## Diversity
  dplyr::mutate(div.temp_1 = butterfly.count / butterfly.abundance,
                div.temp_2 = log(div.temp_1),
                div.temp_3 = div.temp_1 * div.temp_2) %>% 
  dplyr::group_by(year, pasture) %>% 
  dplyr::mutate(butterfly.diversity_shannon = sum(div.temp_3, na.rm = T) * -1) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-dplyr::starts_with("div.temp_"))

# Check structure
dplyr::glimpse(bf_v3)

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

# Re-check structure
dplyr::glimpse(bf_v4)

# Perform any final subsetting
bf_v5 <- bf_v4 %>% 
  ## Drop 2007
  dplyr::filter(year > 2007)

# Export
write.csv(x = bf_v5, row.names = F, na = '',
          file = file.path("data", "ready-butterflies.csv"))

##  ------------------------------------------  ##      
      # Relative Abundance Prep - Flower ----
##  ------------------------------------------  ##      

# Generate an 'overall species count' version of the data
flr.spp_v1 <- flr_v5 %>% 
  tidyr::pivot_longer(cols = -year:-flower.diversity_shannon,
                      names_to = "flower.common") %>% 
  dplyr::group_by(flower.common) %>% 
  dplyr::summarize(sp.total = sum(value, na.rm = T)) %>% 
  dplyr::ungroup()

# Check structure
dplyr::glimpse(flr.spp_v1)

# Calculate relative abundance (as a %)
flr.spp_v2 <- flr.spp_v1 %>% 
  dplyr::mutate(
    overall.total = sum(sp.total, na.rm = T),
    relative.abun_perc = sp.total / overall.total * 100
  )

# Check structure
dplyr::glimpse(flr.spp_v2)

# Collapse rare species into one group
flr.spp_v3 <- flr.spp_v2 %>% 
  dplyr::mutate(flower.common = ifelse(
    test = relative.abun_perc < 5,
    yes = "Floral Spp. < 5% Total",
    no = stringr::str_to_title(gsub("\\.", " ", x = flower.common))
  )) %>% 
  dplyr::group_by(flower.common) %>% 
  dplyr::summarize(relative.abun_perc = sum(relative.abun_perc, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(dplyr::desc(relative.abun_perc))

# Check that out
flr.spp_v3

# Export
write.csv(x = flr.spp_v3, row.names = F, na = '',
          file = file.path("data", "relative-abundance_flowers.csv"))

##  ------------------------------------------  ##      
   # Relative Abundance Prep - Butterfly ----
##  ------------------------------------------  ##      

# Generate an 'overall species count' version of the data
bf.spp_v1 <- bf_v5 %>% 
  tidyr::pivot_longer(cols = -year:-butterfly.diversity_shannon,
                      names_to = "butterfly.common") %>% 
  dplyr::group_by(butterfly.common) %>% 
  dplyr::summarize(sp.total = sum(value, na.rm = T)) %>% 
  dplyr::ungroup()

# Check structure
dplyr::glimpse(bf.spp_v1)

# Calculate relative abundance (as a %)
bf.spp_v2 <- bf.spp_v1 %>% 
  dplyr::mutate(
    overall.total = sum(sp.total, na.rm = T),
    relative.abun_perc = sp.total / overall.total * 100
  )

# Check structure
dplyr::glimpse(bf.spp_v2)

# Collapse rare species into one group
bf.spp_v3 <- bf.spp_v2 %>% 
  dplyr::mutate(butterfly.common = ifelse(
    test = relative.abun_perc < 5,
    yes = "Butterfly Spp. < 5% Total",
    no = stringr::str_to_title(gsub("\\.", " ", x = butterfly.common))
    )) %>% 
  dplyr::group_by(butterfly.common) %>% 
  dplyr::summarize(relative.abun_perc = sum(relative.abun_perc, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(dplyr::desc(relative.abun_perc))

# Check that out
bf.spp_v3

# Export
write.csv(x = bf.spp_v3, row.names = F, na = '',
          file = file.path("data", "relative-abundance_butterflies.csv"))

# End ----
