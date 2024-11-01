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





##  ------------------------------------------  ##      
# Flower Prep ----
##  ------------------------------------------  ##      




# End ----
