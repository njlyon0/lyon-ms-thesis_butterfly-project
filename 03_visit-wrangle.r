##  ------------------------------------------------------------  ##
# Butterfly Project - Wrangle Nectar Resource Data
##  ------------------------------------------------------------  ##
# Purpose:
## Wrangle nectar flower data (e.g., do quality control, calculate metrics)

# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, supportR)

# Get set up
source("-setup.r")

# Clear environment
rm(list = ls()); gc()

##  ------------------------------------------  ##
# Load Data ----
##  ------------------------------------------  ##

# Identify the relevant file
bfly_file <- file.path("data", "raw", "2007-2018 GRG INVERTS MASTER DATA.xlsx")

# Check what sheets are in it
readxl::excel_sheets(path = bfly_file)

# Read in the relevant sheet
vst_v01 <- readxl::read_excel(path = bfly_file, sheet = "Sites",
  col_types = "text")

# Check structure
dplyr::glimpse(vst_v01)
