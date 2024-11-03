##  ------------------------------------------------------------  ##
            # Butterfly Project - Figure Creation
##  ------------------------------------------------------------  ##
# Written by Nick J Lyon

# Script purpose:
## Create publication-quality figures
## (Based on patterns supported by statistics)

##  ------------------------------------------  ##
                # Housekeeping ----
##  ------------------------------------------  ##

# Set required libraries
# install.packages("librarian")
librarian::shelf(tidyverse, supportR)

# Create needed folder(s)
dir.create(path = file.path("figures"), showWarnings = F)

# Clear environment & collect garbage
rm(list = ls()); gc()

# Load desired custom function(s)
source(file.path("tools", "fxn_make-fig.R"))

# Read in butterfly & floral data
bf <- read.csv(file = file.path("data", "ready-butterflies.csv")) %>% 
  dplyr::rename(mgmt = adaptive.mgmt.abbrev)
flr <- read.csv(file = file.path("data", "ready-flowers.csv")) %>% 
  dplyr::rename(mgmt = adaptive.mgmt.abbrev)

# Check structure
dplyr::glimpse(bf)
dplyr::glimpse(flr)

##  ------------------------------------------  ##
                # Graph Helpers ----
##  ------------------------------------------  ##

# Color palette
mgmt.colors <- c("GB" = "#DF227C", "GB-IC" = "#725EEC", "IC" = "#588FF9", 
                 "BO" = "#FFB02F", "PBG" = "#FF6018")

# Shape 'palette'
mgmt.shapes <- c("GB" = 24, "GB-IC" = 25, "IC" = 23, "BO" = 21, "PBG" = 22)

##  ------------------------------------------  ##
# Univariate - Butterflies ----
##  ------------------------------------------  ##

# Butterfly Abundance
make_fig(df = bf, resp = "butterfly.abundance", focus = "ixn", 
           cols = mgmt.cols, shps = mgmt.shps)

# Butterfly Richness
## By management
make_fig(df = bf, resp = "butterfly.richness", focus = "mgmt", 
         cols = mgmt.cols, shps = mgmt.shps)
## By year
make_fig(df = bf, resp = "butterfly.richness", focus = "year", 
         cols = mgmt.cols, shps = mgmt.shps)

# Butterfly Diversity
make_fig(df = bf, resp = "butterfly.diversity_shannon", focus = "ixn", 
         cols = mgmt.cols, shps = mgmt.shps) +
  labs(y = "Butterfly Shannon Diversity")




# End ----
