##  ------------------------------------------------------------  ##
            # Butterfly Project - Download Tidy Data
##  ------------------------------------------------------------  ##
# Written by Nick J Lyon

# Script purpose:
## Download tidy data from Google Drive folder

##  ------------------------------------------  ##      
              # Housekeeping ----
##  ------------------------------------------  ##      

# Set required libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive)

# Clear environment & collect garbage
rm(list = ls()); gc()

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)

# Identify relevant Drive folder
tidy_url <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1L9GPPA3M6LcbWeGF-aVTLlom9TxszwY7")

##  ------------------------------------------  ##      
              # Download Data ----
##  ------------------------------------------  ##      

# Identify files that are already downloaded
already_done <- dir(file.path("data"))

# List files in Drive
tidy_data <- googledrive::drive_ls(path = tidy_url)

# Identify only the needed files
needed_data <- tidy_data %>% 
  ## Pare down to only data relevant to this project
  dplyr::filter(stringr::str_detect(string = name, pattern = "butterfly-project_"))

# Is redownloading desired?
redownload <- TRUE

# # Remove already downloaded files if re-downloading isn't desired
if(redownload == F){
  needed_data <- dplyr::filter(needed_data, !name %in% already_done) 
}

# Download the needed files
purrr::walk2(.x = needed_data$id, .y = needed_data$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", .y)))

# End ----
