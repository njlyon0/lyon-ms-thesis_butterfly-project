##  ------------------------------------------------------------  ##
           # Butterfly Project - Statistical Analysis
##  ------------------------------------------------------------  ##
# Written by Nick J Lyon

# Script purpose:
## Perform statistical analysis to assess hypotheses

##  ------------------------------------------  ##      
                # Housekeeping ----
##  ------------------------------------------  ##      

# Set required libraries
# install.packages("librarian")
librarian::shelf(tidyverse, lmerTest)

# Clear environment & collect garbage
rm(list = ls()); gc()

# Create needed folder(s)
dir.create(path = file.path("results"), showWarnings = F)

# Load desired custom function(s)
source(file.path("tools", "fxn_tabularize-mem-results.R"))

# Read in butterfly & floral data
bf <- read.csv(file = file.path("data", "ready-butterflies.csv"))
flr <- read.csv(file = file.path("data", "ready-flowers.csv"))

# Check structure
dplyr::glimpse(bf)
dplyr::glimpse(flr)

##  ------------------------------------------  ##      
                # Univariate ----
##  ------------------------------------------  ##      

# Make an empty list
results_list <- list()

# Identify the three community metrics we're interested in (for now)
metrics <- c("abundance", "richness", "diversity_shannon")

# Loop across desired response variables
for(var in c(paste0("butterfly.", metrics), paste0("flower.", metrics))){
  
  # Progress message
  message("Getting results for ", var)
  
  # Fit model
  if(stringr::str_detect(string = var, pattern = "butterfly")){
    mem <- lmerTest::lmer(bf[[var]] ~ adaptive.mgmt * year + (1|pasture), data = bf)
  } else {
    mem <- lmerTest::lmer(flr[[var]] ~ adaptive.mgmt * year + (1|pasture), data = flr)
  }
  
  # Extract the ANOVA table of results
  var_result <- tabularize_mem_results(mod = mem) %>% 
    dplyr::mutate(response = var, .before = dplyr::everything())
  
  # If the interaction is non-significant...
  if(dplyr::filter(var_result, term == "adaptive.mgmt:year")$p.value > 0.1){
    
    # Fit simpler model (no ixn term)
    if(stringr::str_detect(string = var, pattern = "butterfly")){
      mem <- lmerTest::lmer(bf[[var]] ~ adaptive.mgmt + year + (1|pasture), data = bf)
    } else {
      mem <- lmerTest::lmer(flr[[var]] ~ adaptive.mgmt + year + (1|pasture), data = flr)
    }
    
    # Re-extract ANOVA table
    var_result <- tabularize_mem_results(mod = mem) %>% 
      dplyr::mutate(response = var, .before = dplyr::everything())
  }
  
  # Add results to the results list
  results_list[[var]] <- var_result
  
}

# Unlist & wrangle the output
results_df <- results_list %>% 
  purrr::list_rbind(x = .) %>%
  dplyr::mutate(f.statistic = round(x = f.statistic, digits = 1),
                p.value = round(x = p.value, digits = 3))

# Check that out
dplyr::glimpse(results_df)
## view(results_df)

# Export the results
write.csv(x = results_df, row.names = F, na = '',
          file = file.path("results", "univariate-stats.csv"))

##  ------------------------------------------  ##      
# Multivariate ----
##  ------------------------------------------  ##      




# End ----
