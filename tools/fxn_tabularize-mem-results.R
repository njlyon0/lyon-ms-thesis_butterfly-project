#' @title Tabularize Statistical Results
#' 
#' @description Accepts a model object created by `lmerTest::lmer` and returns a streamlined ANOVA table from that model as a dataframe. Meant as a convenience for exporting quick-and-easy statistical results using this relatively common package and function.
#' 
#' @param mod (mixed-effects model) object of class 'lmerModLmerTest' and 'lmerTest' from which to generate and extract an ANOVA table
#' 
#' @return (dataframe) streamlined ANOVA table
#' 
tabularize_mem_results <- function(mod = NULL){
  
  # Error for missing model
  if(is.null(mod) == TRUE)
    stop("'mod' must be provided")
  
  # Error for wrong model type
  if(all(class(mod) %in% c("lmerModLmerTest", "lmerTest")) != TRUE)
    stop("'mod' must be an object created by `lmerTest::lmer`")
  
  # Fit ANOVA
  mod_aov <- anova(mod)
  
  # Extract relevant information
  mod_results <- data.frame("term" = rownames(mod_aov),
                            "deg.freedom" = mod_aov$NumDF,
                            "f.statistic" = mod_aov$`F value`,
                            "p.value" = mod_aov$`Pr(>F)`)
  
  # Return that
  return(mod_results) }
