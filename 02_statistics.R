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
librarian::shelf(tidyverse, supportR, lmerTest, lsmeans, pbkrtest, multcompView, RRPP)

# Clear environment & collect garbage
rm(list = ls()); gc()

# Create needed folder(s)
dir.create(path = file.path("results"), showWarnings = F)

# Load desired custom function(s)
source(file.path("tools", "fxn_tabularize-mem-results.R"))

# Read in butterfly & floral data
flr <- read.csv(file = file.path("data", "ready-flowers.csv"))
bf <- read.csv(file = file.path("data", "ready-butterflies.csv"))

# Check structure
dplyr::glimpse(flr)
dplyr::glimpse(bf)

# Identify the threshold for significance
## Used to determine when:
### (A) the interaction term should be dropped
### (B) pairwise comparisons among management methods are warranted
sig_thresh <- 0.1

##  ------------------------------------------  ##      
                # Univariate ----
##  ------------------------------------------  ##      

# Make empty lists for storing outputs
results_list <- list()
pairs_list <- list()

# Identify the three community metrics we're interested in (for now)
metrics <- c("abundance", "richness", "diversity_shannon")

# Loop across desired response variables
for(var in c(paste0("flower.", metrics), paste0("butterfly.", metrics))){
## var <- "butterfly.richness"
  
  # Progress message
  message("Getting results for ", var)
  
  # Grab correct data
  if(stringr::str_detect(string = var, pattern = "butterfly")) {
    var_df <- bf } else { var_df <- flr }
  
  # Fit model
  mem <- lmerTest::lmer(var_df[[var]] ~ adaptive.mgmt * year + (1|pasture),
                        data = var_df)

  # Extract the ANOVA table of results
  var_result <- tabularize_mem_results(mod = mem) %>% 
    dplyr::mutate(response = var, .before = dplyr::everything())
  
  # If the interaction is non-significant...
  if(dplyr::filter(var_result, term == "adaptive.mgmt:year")$p.value > sig_thresh){
    
    # Fit simpler model (no ixn term)
    mem <- lmerTest::lmer(var_df[[var]] ~ adaptive.mgmt + year + (1|pasture),
                          data = var_df)
    
    # Re-extract ANOVA table
    var_result <- tabularize_mem_results(mod = mem) %>% 
      dplyr::mutate(response = var, .before = dplyr::everything())
  }
  
  # Add results to the results list
  results_list[[var]] <- var_result
  
  # If interaction was *not* significant & management *was*, get pairwise results
  if(nrow(var_result) == 2 & dplyr::filter(var_result, term == "adaptive.mgmt")$p.value < sig_thresh){
    
    # Pairwise comparisons message
    message("Pairwise comparisons among management methods necessitated!")
    
    # Do pairwise comparisons
    var_pairs <- as.data.frame(lsmeans::lsmeans(object = mem, pairwise ~ adaptive.mgmt)$contrasts)
    
    # Clean that up
    var_pairs_df <- data.frame(
      "response" = var,
      "pair" = var_pairs$contrast,
      "estimate" = var_pairs$estimate,
      "t.ratio" = var_pairs$t.ratio,
      "p.value" = var_pairs$p.value)
    
    # Add to the output list in a prettier format
    pairs_list[[var]] <- var_pairs_df
    
    } # Close conditional
  
} # Close loop

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

# Unlist & wrangle the pairwise comparison results
pairs_df <- pairs_list %>% 
  purrr::list_rbind(x = .) %>% 
  dplyr::mutate(t.ratio = round(x = t.ratio, digits = 1),
                p.value = round(x = p.value, digits = 3)) %>% 
  dplyr::mutate(pair = gsub("graze and burn and invasive control", "GB-IC", pair),
                pair = gsub("graze and burn", "GB", pair),
                pair = gsub("invasive control", "IC", pair),
                pair = gsub("burn only", "BO", pair),
                pair = gsub("patch burn graze", "PBG", pair),
                pair = gsub(" - ", ":", pair))

# Check that out
dplyr::glimpse(pairs_df)
## view(pairs_df)

# Export the results
write.csv(x = pairs_df, row.names = F, na = '',
          file = file.path("results", "univariate-pairs.csv"))

##  ------------------------------------------  ##      
              # Multivariate ----
##  ------------------------------------------  ##      

# Make more lists for loop outputs
multivar.results_list <- list()
multivar.pairs_list <- list()

# Loop across two datasets
for(taxon in c("flower", "butterfly")){
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
  if(multivar.results_list[[taxon]]$p.value < sig_thresh){
    
    # Pairwise comparisons message
    message("Pairwise comparisons among management methods necessitated!")
    
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
