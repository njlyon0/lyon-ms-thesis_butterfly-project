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

# Clear environment & collect garbage
rm(list = ls()); gc()

# Create needed folder(s)
dir.create(path = file.path("figures"), showWarnings = F)

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

# Summarize abundance
bf.abun <- supportR::summary_table(data = bf, response = "butterfly.abundance",
                                   groups = c("mgmt", "year"), drop_na = T)

# Check that output
dplyr::glimpse(bf.abun)

# Make figure
ggplot(bf.abun, aes(x = year, y = mean)) +
  geom_errorbar(aes(ymax = mean + std_error, ymin = mean - std_error),
                position = position_dodge(width = 0.25), width = 0.1) +
  geom_point(aes(shape = mgmt, fill = mgmt),
             position = position_dodge(width = 0.25), size = 3) +
  geom_smooth(aes(color = mgmt),  formula = "y ~ x", method = "lm", se = F) +
  labs(x = "Year", y = "Mean Butterfly Abundance (± SE)") +
  scale_color_manual(values = mgmt.colors) +
  scale_fill_manual(values = mgmt.colors) +
  scale_shape_manual(values = mgmt.shapes) +
  supportR::theme_lyon() +
  theme(legend.position = "none")






# End ----
