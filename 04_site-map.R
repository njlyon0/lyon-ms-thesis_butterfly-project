##  ------------------------------------------------------------  ##
            # Butterfly Project - Site Map Creation
##  ------------------------------------------------------------  ##
# Written by Nick J Lyon

# Script purpose:
## Create a map of the sites

##  ------------------------------------------  ##
               # Housekeeping ----
##  ------------------------------------------  ##

# Set required libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, sf, maps)

# Clear environment & collect garbage
rm(list = ls()); gc()

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)
dir.create(path = file.path("data", "map_files"), showWarnings = F)

##  ------------------------------------------  ##
            # Download Shapefiles ----
##  ------------------------------------------  ##
# Identify relevant folder
shp_url <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1B2ZamCDML1a5QUO-3E0a0Nt3feXVc-ah")

# Identify already-downloaded files
shp_done <- dir(path = file.path("data", "map_files"))

# Identify files in Drive (that are not downloaded already)
(shp_files <- googledrive::drive_ls(shp_url) %>% 
    dplyr::filter(!name %in% shp_done))

# Download them
purrr::walk2(.x = shp_files$id, .y = shp_files$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "map_files", .y)))

##  ------------------------------------------  ##      
                  # Map Prep ----
##  ------------------------------------------  ##      

# Create the needed borders file
borders <- sf::st_as_sf(maps::map(database = "state", plot = F, fill = T))

# Identify coordinates
coords_utm <- read.csv(file = file.path("data", "map_files", "site-utm-coords.csv"))

# Check structure
dplyr::glimpse(coords_utm)

# Do some wrangling and convert to latitude/longitude (from UTMs)
coords_sf <- coords_utm %>% 
  dplyr::select(-utm.zone) %>% 
  dplyr::rename(longitude = utm.x,
                latitude = utm.y) %>% 
  sf::st_as_sf(x = ., coords = c("longitude", "latitude"),
               crs = "+proj=utm +zone=15") %>% 
  sf::st_transform(x = ., crs = sf::st_crs(borders)) %>% 
  dplyr::mutate(lon = sf::st_coordinates(x = .)[,1],
                lat = sf::st_coordinates(x = .)[,2]) %>% 
  sf::st_drop_geometry(x = .)

# Check structure
dplyr::glimpse(coords_sf)

# Read in GRG boundary
grg <- sf::st_read(dsn = file.path("data", "map_files", "GRG Boundary.shp")) %>% 
  # Convert to lat/long
  sf::st_transform(x = ., crs = "+proj=longlat +datum=WGS84") %>% 
  # Pare down to one layer
  dplyr::select(ID)

# Test plot
plot(grg, axes = T)

##  ------------------------------------------  ##
# Map Creation ----
##  ------------------------------------------  ##

# Make 'zoomed out' map
borders %>% 
  ggplot() +
  geom_sf(fill = "gray95") +
  coord_sf(xlim = c(-98, -88), ylim = c(38, 44), expand = F) +
  geom_point(data = coords_sf[1,], aes(x = lon, y = lat, fill = 'x'),
             pch = 23, size = 5) +
  labs(x = "Longitude", y = "Latitude") +
  scale_x_continuous(breaks = seq(from = -98, to = -88, by = 2)) +
  scale_y_continuous(breaks = seq(from = 38, to = 44, by = 2)) +
  scale_fill_manual(values = "purple") +
  supportR::theme_lyon() + 
  theme(legend.position = "none")

# Make 'zoomed in' map
borders %>% 
  ggplot() +
  geom_sf(fill = "gray95") +
  coord_sf(xlim = c(-94.2, -94.04), ylim = c(40.45, 40.75), expand = F) +
  geom_point(data = coords_sf, aes(x = lon, y = lat, fill = pasture),
             pch = 23, size = 2) +
  labs(x = "Longitude", y = "Latitude") +
  scale_x_continuous(breaks = seq(from = -94.2, to = -94.04, by = 0.1)) +
  scale_y_continuous(breaks = seq(from = 40.45, to = 40.75, by = 0.15)) +
  supportR::theme_lyon() + 
  theme(legend.position = "none")

# End ----
