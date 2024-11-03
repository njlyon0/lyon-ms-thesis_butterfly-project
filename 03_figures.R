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

# Define desired order of managment types
mgmt.order <- c("BO", "PBG", "GB", "GB-IC", "IC")

# Read in butterfly & floral data
bf <- read.csv(file = file.path("data", "ready-butterflies.csv")) %>% 
  dplyr::rename(mgmt = adaptive.mgmt.abbrev) %>% 
  dplyr::mutate(year = year - 2000,
                mgmt = factor(x = mgmt, levels = mgmt.order))
flr <- read.csv(file = file.path("data", "ready-flowers.csv")) %>% 
  dplyr::rename(mgmt = adaptive.mgmt.abbrev) %>% 
  dplyr::mutate(year = year - 2000,
                mgmt = factor(x = mgmt, levels = mgmt.order))

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
          # Univariate - Flowers ----
##  ------------------------------------------  ##

# Flower Abundance
## By year
flr.abun1 <- make_fig(df = flr, resp = "flower.abundance", focus = "year", 
                      sig = FALSE, cols = mgmt.cols, shps = mgmt.shps) +
  ylim(0, (3*10^4))
  
## By management
flr.abun2 <- make_fig(df = flr, resp = "flower.abundance", focus = "mgmt", 
                     cols = mgmt.cols, shps = mgmt.shps) +
  labs(y = "Flower Abundance") +
  ylim(0, (3*10^4)) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

## Combine
flr.abun <- cowplot::plot_grid(flr.abun1, flr.abun2, nrow = 1)
flr.abun

# Flower Richness
flr.rich <- make_fig(df = flr, resp = "flower.richness", focus = "ixn", 
                     cols = mgmt.cols, shps = mgmt.shps) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.15, 0.8))
flr.rich

# Flower Diversity
## By year
flr.dive1 <- make_fig(df = flr, resp = "flower.diversity_shannon", focus = "year", 
                      sig = FALSE, cols = mgmt.cols, shps = mgmt.shps) +
  labs(y = "Flower Diversity") +
  ylim(0, 3.2)

## By management
flr.dive2 <- make_fig(df = flr, resp = "flower.diversity_shannon", focus = "mgmt", 
                      cols = mgmt.cols, shps = mgmt.shps) +
  ylim(0, 3.2) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

## Combine
flr.dive <- cowplot::plot_grid(flr.dive1, flr.dive2, nrow = 1)
flr.dive

# Assemble into a multi-panel figure
cowplot::plot_grid(flr.abun, flr.rich, flr.dive, labels = "AUTO", ncol = 1)
ggsave(filename = file.path("figures", "figure_flower.png"),
       width = 4, height = 12, units = "in")

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
## By year
bf.rich1 <- make_fig(df = bf, resp = "butterfly.richness", focus = "year", 
                     cols = mgmt.cols, shps = mgmt.shps) +
  ylim(5, 25)

## By management
bf.rich2 <- make_fig(df = bf, resp = "butterfly.richness", focus = "mgmt", 
                     cols = mgmt.cols, shps = mgmt.shps) +
  ylim(5, 25) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

## Combine
bf.rich <- cowplot::plot_grid(bf.rich1, bf.rich2, nrow = 1)
bf.rich

# Butterfly Diversity
bf.dive <- make_fig(df = bf, resp = "butterfly.diversity_shannon", focus = "ixn", 
                    cols = mgmt.cols, shps = mgmt.shps) +
  labs(y = "Butterfly Diversity") +
  theme(legend.position = "none")
bf.dive

# Assemble into a multi-panel figure
cowplot::plot_grid(bf.abun, bf.rich, bf.dive, labels = "AUTO", ncol = 1)
ggsave(filename = file.path("figures", "figure_butterfly.png"),
       width = 4, height = 12, units = "in")

# End ----
