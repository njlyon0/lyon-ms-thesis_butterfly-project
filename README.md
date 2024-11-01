# Lyon MS Thesis - Butterfly Project

The first chapter of my M.Sc. thesis -- focused on the response of butterfly and nectar-producing flower communities to different combinations of prescribed fire and cattle grazing. This repository analyzes those data and creates publication-quality figures of the results.

## Script Explanations

- `00_data-download.R` - Downloads relevant data files from Google Drive (note that this does require prior access to the relevant folder)
    - All tidying performed in a separate GitHub repository ([njlyon0 / **lyon-ms-thesis_field-tidy**](https://github.com/njlyon0/lyon-ms-thesis_field-tidy))
- `01_data-prep.R` - Prepares data (including summarization to appropriate spatial scale) for analysis and visualization
- `02_statistics.R` - Performs statistical analysis on data. Includes univariate and multivariate tests
