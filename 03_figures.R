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
librarian::shelf(tidyverse, supportR, cowplot)

# Create needed folder(s)
dir.create(path = file.path("figures"), showWarnings = F)

# Clear environment & collect garbage
rm(list = ls()); gc()

# Load desired custom function(s)
source(file.path("tools", "fxn_make-fig.R"))

# Read in butterfly & floral data
bf <- read.csv(file = file.path("data", "ready-butterflies.csv")) %>% 
  dplyr::rename(mgmt = adaptive.mgmt.abbrev) %>% 
  dplyr::mutate(year = year - 2000)
flr <- read.csv(file = file.path("data", "ready-flowers.csv")) %>% 
  dplyr::rename(mgmt = adaptive.mgmt.abbrev) %>% 
  dplyr::mutate(year = year - 2000)

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
bf.abun <- make_fig(df = bf, resp = "butterfly.abundance", focus = "ixn", 
                    cols = mgmt.cols, shps = mgmt.shps) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.15, 0.8))
bf.abun

# Butterfly Richness
## By management
bf.rich1 <- make_fig(df = bf, resp = "butterfly.richness", focus = "mgmt", 
                     cols = mgmt.cols, shps = mgmt.shps) +
  labs(y = "Richness") +
  theme(legend.position = "none")
## By year
bf.rich2 <- make_fig(df = bf, resp = "butterfly.richness", focus = "year", 
                     cols = mgmt.cols, shps = mgmt.shps) +
  theme(axis.title.y = element_blank())
## Combine
bf.rich <- cowplot::plot_grid(bf.rich1, bf.rich2, nrow = 1)
bf.rich

# Butterfly Diversity
bf.dive <- make_fig(df = bf, resp = "butterfly.diversity_shannon", focus = "ixn", 
                    cols = mgmt.cols, shps = mgmt.shps) +
  labs(y = "Shannon Diversity") +
  theme(legend.position = "none")
bf.dive

# Assemble into a multi-panel figure
cowplot::plot_grid(bf.abun, bf.rich, bf.dive, labels = "AUTO", ncol = 1)
ggsave(filename = file.path("figures", "figure_butterfly.png"),
       width = 4, height = 12, units = "in")

# End ----
