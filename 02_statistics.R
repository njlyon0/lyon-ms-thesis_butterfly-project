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
librarian::shelf(tidyverse, lmerTest, RRPP)

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

# Make more lists for loop outputs
multivar.results_list <- list()
multivar.pairs_list <- list()

# Loop across two datasets
for(taxon in c("butterfly", "flower")){
  ## taxon <- "butterfly"
  
  # Progress message
  message("Getting results for ", taxon, " community composition")
  
  # Grab correct data
  if(taxon == "butterfly") { comm_df <- bf } else { comm_df <- flr }
  
  # Make community matrix with only species columns
  comm_mat <- comm_df %>% 
    dplyr::select(-year:-adaptive.mgmt.abbrev,
                  -dplyr::contains(c(".abundance", ".richness", ".diversity"))) %>% 
    as.matrix()
  
  # Get an `RRPP`-style data frame
  comm_rdf <- RRPP::rrpp.data.frame("adaptive.mgmt" = comm_df$adaptive.mgmt.abbrev,
                                    "community" = comm_mat)
  
  # Perform multivariate ANOVA (permutation-based)
  manova <- RRPP::lm.rrpp(community ~ adaptive.mgmt, data = comm_rdf)
  taxon_result <- anova(manova, effect.type = "F")
  
  # Make prettier results data object and add to list
  multivar.results_list[[taxon]] <- data.frame(
    "taxon" = paste0(taxon, " community composition"),
    "term" = "adaptive.mgmt",
    "f.statistic" = taxon_result$table$`F`[1],
    "z.score" = taxon_result$table$Z[1],
    "p.value" = taxon_result$table$`Pr(>F)`[1])
  
  # If significant, get pairwise results too
  if(multivar.results_list[[taxon]]$p.value < 0.1){
    
    # Extract pairwise results
    taxon_pairs <- summary(RRPP::pairwise(fit = manova, groups = comm_rdf$adaptive.mgmt))
    
    # And add to dedicated pairs results list
    multivar.pairs_list[[taxon]] <- data.frame(
      "taxon" = paste0(taxon, " community composition"),
      "pair" = rownames(taxon_pairs$summary.table),
      "distance" = taxon_pairs$summary.table$d,
      "z.score" = taxon_pairs$summary.table$Z,
      "p.value" = taxon_pairs$summary.table$`Pr > d`)
    
  } # Close pairwise conditional
  
} # Close loop

# Unlist & wrangle the main output
multivar.results_df <- multivar.results_list %>% 
  purrr::list_rbind(x = .) %>%
  dplyr::mutate(f.statistic = round(x = f.statistic, digits = 1),
                z.score = round(x = z.score, digits = 1),
                p.value = round(x = p.value, digits = 3))

# Check that out
multivar.results_df

# Export the results
write.csv(x = multivar.results_df, row.names = F, na = '',
          file = file.path("results", "multivariate-stats.csv"))

# Unlist & wrangle the pairwise comparison information
multivar.pairs_df <- multivar.pairs_list %>% 
  purrr::list_rbind(x = .) %>% 
  dplyr::mutate(distance = round(x = distance, digits = 2),
                z.score = round(x = z.score, digits = 1),
                p.value = round(x = p.value, digits = 3))

# Check that out
multivar.pairs_df

# Export the results
write.csv(x = multivar.pairs_df, row.names = F, na = '',
          file = file.path("results", "multivariate-pairs.csv"))

# End ----
