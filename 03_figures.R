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
librarian::shelf(tidyverse, supportR, cowplot, vegan, ape)

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
  ylim(0, (3*10^4)) +
  geom_text(label = "NS", x = 9, y = 3*10^4, size = 5)
  
## By management
flr.abun2 <- make_fig(df = flr, resp = "flower.abundance", focus = "mgmt", 
                     cols = mgmt.cols, shps = mgmt.shps) +
  labs(y = "Flower Abundance") +
  ylim(0, (3*10^4)) +
  geom_text(label = "a", x = 0.75, y = 5200, size = 4) +
  geom_text(label = "b", x = 1.75, y = 12000, size = 4) +
  geom_text(label = "ab", x = 2.75, y = 7800, size = 4) +
  geom_text(label = "ab", x = 3.75, y = 11000, size = 4) +
  geom_text(label = "ab", x = 4.75, y = 11000, size = 4) +
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
  ylim(0, 3.2) +
  geom_text(label = "NS", x = 9, y = 3.2, size = 5)

## By management
flr.dive2 <- make_fig(df = flr, resp = "flower.diversity_shannon", focus = "mgmt", 
                      cols = mgmt.cols, shps = mgmt.shps) +
  ylim(0, 3.2) +
  geom_text(label = "NS", x = 5, y = 3.2, size = 5) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

## Combine
flr.dive <- cowplot::plot_grid(flr.dive1, flr.dive2, nrow = 1)
flr.dive

# Assemble into a multi-panel figure
cowplot::plot_grid(flr.abun, flr.rich, flr.dive, labels = "AUTO", ncol = 1)
ggsave(filename = file.path("figures", "figure_flower.png"),
       width = 5, height = 12, units = "in")

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
  geom_text(label = "a", x = 0.75, y = 18.5, size = 4) +
  geom_text(label = "a", x = 1.75, y = 19, size = 4) +
  geom_text(label = "a", x = 2.75, y = 18, size = 4) +
  geom_text(label = "a", x = 3.75, y = 19, size = 4) +
  geom_text(label = "b", x = 4.75, y = 15, size = 4) +
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
       width = 5, height = 12, units = "in")

##  ------------------------------------------  ##
          # Multivariate - Flowers ----
##  ------------------------------------------  ##

# Tidy environment
rm(list = setdiff(x = ls(), y = c("bf", "flr", "mgmt.colors", "mgmt.shapes")))
gc()

# Get community alone
flr_comm <- flr %>% 
  dplyr::select(-year:-flower.diversity_shannon)

# Check that worked
supportR::diff_check(old = names(flr), new = names(flr_comm))
names(flr_comm)

# Get a distance matrix
flr_dist <- vegan::vegdist(x = as.matrix(flr_comm), method = 'kulczynski')

# Perform Principal Coordinates Analysis (PCoA)
flr_points <- ape::pcoa(D = flr_dist)

# Get an ordination
png(filename = file.path("figures", "ordination_flower.png"),
    width = 8, height = 8, units = "in", res = 520)
supportR::ordination(mod = flr_points, grps = as.character(flr$mgmt),
                     x = "topright", bg = mgmt.colors, lty = 1, alpha = 0.7)
dev.off()

##  ------------------------------------------  ##
        # Multivariate - Butterflies ----
##  ------------------------------------------  ##

# Tidy environment
rm(list = setdiff(x = ls(), y = c("bf", "flr", "mgmt.colors", "mgmt.shapes")))
gc()

# Get community alone
bf_comm <- bf %>% 
  dplyr::select(-year:-butterfly.diversity_shannon)

# Check that worked
supportR::diff_check(old = names(bf), new = names(bf_comm))
names(bf_comm)

# Get a distance matrix
bf_dist <- vegan::vegdist(x = as.matrix(bf_comm), method = 'kulczynski')

# Perform Principal Coordinates Analysis (PCoA)
bf_points <- ape::pcoa(D = bf_dist)

# Get an ordination
png(filename = file.path("figures", "ordination_butterfly.png"),
    width = 8, height = 8, units = "in", res = 520)
supportR::ordination(mod = bf_points, grps = as.character(bf$mgmt),
                     x = "topright", bg = mgmt.colors, alpha = 0.7)
dev.off()

##  ------------------------------------------  ##
            # Relative Abundance ----
##  ------------------------------------------  ##

# Tidy environment
rm(list = setdiff(x = ls(), y = c("bf", "flr", "mgmt.colors", "mgmt.shapes")))
gc()

# Make object for 'low abundance' designation
low.abun <- "Spp. < 5% Total"

# Read in relative abundance data for both taxa
flr.relabun_df <- read.csv(file = file.path("data", "relative-abundance_flowers.csv")) %>% 
  dplyr::mutate(flower.common = factor(flower.common, levels = rev(c(setdiff(flower.common, low.abun), low.abun))))
bf.relabun_df <- read.csv(file = file.path("data", "relative-abundance_butterflies.csv")) %>% 
  dplyr::mutate(butterfly.common = factor(butterfly.common,
                                       levels = rev(c(setdiff(butterfly.common, low.abun), low.abun))))

# Check them out
flr.relabun_df
bf.relabun_df

# Make vectors of colors for both taxa
flr.colors <- c("Birdsfoot Trefoil" = "#ffbf00", "White Clover" = "#edede9",
                "Slender Mountain Mint" = "#386641", "Red Clover" = "#fb6f92",
                "Daisy Fleabane" = "#fcdc5d", "Spp. < 5% Total" = "#000")
bf.colors <- c("Eastern Tailed Blue" = "#8ecae6", "Clouded Sulphur" = "#fcefb4", 
               "Orange Sulphur" = "#fcbc5d", "Pearl Crescent" = "#f85e00",
               "Cabbage White" = "#edf2f4", "Regal Fritillary" = "#a41623", 
               "Common Wood Nymph" = "#b08968", "Spp. < 5% Total" = "#000")

# Make the floral graph
flr.relabun <- ggplot(flr.relabun_df, aes(x = relative.abun_perc, y = flower.common,
                                         color = "y", fill = flower.common)) +
  geom_bar(stat = "identity") +
  labs(x = "Relative Abundance (%)", y = "Flower Species") +
  # geom_vline(xintercept = 10, linetype = 3) +
  xlim(0, 50) +
  scale_color_manual(values = "black") +
  scale_fill_manual(values = flr.colors) +
  geom_text(label = "Birdsfoot Trefoil", x = 0.5, y = 6, hjust = "left") +
  geom_text(label = "White Clover", x = 10, y = 5, hjust = "left") +
  geom_text(label = "Slender Mountain Mint", x = 8, y = 4, hjust = "left") +
  geom_text(label = "Red Clover", x = 8, y = 3, hjust = "left") +
  geom_text(label = "Daisy Fleabane", x = 7.5, y = 2, hjust = "left") +
  geom_text(label = "Spp. < 5% Total", x = 0.5, y = 1, color = "#FFF", hjust = "left") +
  guides(color = "none") +
  supportR::theme_lyon() +
  theme(legend.position = "none",
        axis.text.y = element_blank())
flr.relabun

# Do the same for butterflies
bf.relabun <- ggplot(bf.relabun_df, aes(x = relative.abun_perc, y = butterfly.common,
                       color = "y", fill = butterfly.common)) +
  geom_bar(stat = "identity") +
  labs(x = "Relative Abundance (%)", y = "Butterfly Species") +
  # geom_vline(xintercept = 10, linetype = 3) +
  xlim(0, 50) +
  scale_color_manual(values = "black") +
  scale_fill_manual(values = bf.colors) +
  geom_text(label = "Eastern Tailed Blue", x = 0.5, y = 8, hjust = "left") +
  geom_text(label = "Clouded Sulphur", x = 13, y = 7, hjust = "left") +
  geom_text(label = "Orange Sulphur", x = 12, y = 6, hjust = "left") +
  geom_text(label = "Pearl Crescent", x = 9, y = 5, hjust = "left") +
  geom_text(label = "Cabbage White", x = 7.5, y = 4, hjust = "left") +
  geom_text(label = "Regal Fritillary", x = 6, y = 3, hjust = "left") +
  geom_text(label = "Common Wood Nymph", x = 6, y = 2, hjust = "left") +
  geom_text(label = "Spp. < 5% Total", x = 0.5, y = 1, color = "#FFF", hjust = "left") +
  guides(color = "none") +
  supportR::theme_lyon() +
  theme(legend.position = "none",
        axis.text.y = element_blank())
bf.relabun

# Assemble into a multi-panel figure
cowplot::plot_grid(flr.relabun, bf.relabun, labels = "AUTO", nrow = 1)
ggsave(filename = file.path("figures", "figure_relative-abundance.png"),
       width = 8, height = 5, units = "in")

# End ----
