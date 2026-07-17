## Eleven Years of Butterfly and Nectar-Producing Plant Community Data from Grassland Sites Managed with Pyric Herbivory 

[![Code DOI](https://img.shields.io/badge/Code-10.5281/zenodo.20329857-orange.svg)](https://doi.org/10.5281/zenodo.20329857) [![Data DOI](https://img.shields.io/badge/EDI%20Data%20ID-edi.2404.1-blue.svg)](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.2404.1)

## Script Explanations

- `00_drive-download.r` - Download 'raw' data from code author's personal Google Drive
    - **NOTE**: This _will not_ work for you if you are not me (i.e., don't have access to the relevant folder)
- `01_visit-wrangle.r` - Get the transect visit data into EDI-ready format (includes site information, date & year, as well as various local climatic variables at time of sampling)
- `02_management-wrangle.r` - Identifies known management history for sites in the study (includes fire, grazing, and anti-fescue management)
- `03_butterfly-wrangle.r` - Get the butterfly transect data into EDI-ready format; also prepares a table of taxonomic information for each species observed at least once
- `04_nectar-wrangle.r` - Get the nectar-producing plant transect data into EDI-ready format; also prepares a table of taxonomic information for each species observed at least once
    - Additionally, identifies whether the species was included in the 2014 restoration seed-mix applied to some sites/patches
- `05_coordinate-wrangle.r` - Transforms UTM coordinates for site/transect centroids into latitude and longitude
- `06_observer-wrangle.r` - Quantifies number of transects sampled by each observer who ever did at least one transect survey in the eleven years of the study
