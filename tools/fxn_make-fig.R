#' @title Create a Figure
#' 
#' @description Creates a figure from a set of tightly-defined options. For ease of writing, many assumptions about the structure and content of the data are made without appropriate error checks so it is unlikely that this function will be broadly useful beyond this context.
#' 
#' @param mod (mixed-effects model) object of class 'lmerModLmerTest' and 'lmerTest' from which to generate and extract an ANOVA table
#' 
#' @param df (dataframe / tibble) data object to summarize to create the figure
#' @param resp (character) column name of response variable
#' @param focus (character) name of type of figure to make. Must be one of "ixn" (interaction), "mgmt" (management), or "year".
#' @param cols (character) vector of custom colors. Must have a 'names' attribute that corresponds to the "mgmt" column
#' @param shps (character) vector of shape integers (see `?pch`). Must have a 'names' attribute that corresponds to the "mgmt" column
#' 
#' @return (ggplot) ggplot2-style figure
#' 
make_fig <- function(df, resp, focus, cols, shps){
  
  # Error for bad 'focus' entires
  if(focus %in% c("ixn", "mgmt", "year") != TRUE)
    stop("'focus' must be one of 'ixn', 'mgmt', or 'year'")
  
  # Generate nice label for plot
  y.lab <- stringr::str_to_title(gsub(pattern = "\\.", replacement = " ", x = resp))
  
  # Interaction figure ----
  if(focus == "ixn"){
    
    # Summarize
    table <- supportR::summary_table(data = df, response = resp, groups = c("mgmt", "year"), drop_na = T)
    
    # Graph
    q <- ggplot(table, aes(x = year, y = mean)) +
      geom_errorbar(aes(ymax = mean + std_error, ymin = mean - std_error),
                    position = position_dodge(width = 0.25), width = 0.1) +
      geom_point(aes(shape = mgmt, fill = mgmt),
                 position = position_dodge(width = 0.25), size = 3) +
      geom_smooth(aes(color = mgmt),  formula = "y ~ x", method = "lm", se = F) +
      labs(x = "Year", y = y.lab) +
      scale_color_manual(values = mgmt.colors) +
      scale_fill_manual(values = mgmt.colors) +
      scale_shape_manual(values = mgmt.shapes) +
      supportR::theme_lyon()
    
  }
  
  # Management figure ----
  if(focus == "mgmt"){
    
    # Graph (no summarizing)
    q <- ggplot(df, aes(x = mgmt, y = .data[[resp]])) +
      geom_boxplot(aes(fill = mgmt), outlier.shape = 21) +
      labs(x = "Management", y = y.lab) +
      scale_fill_manual(values = mgmt.colors) +
      supportR::theme_lyon()
      
  }
  
  # Year figure ----
  if(focus == "year"){
    
  # Summarize
  table <- supportR::summary_table(data = df, response = resp, groups = "year", drop_na = T)
  
  # Graph
  q <- ggplot(table, aes(x = year, y = mean)) +
    geom_errorbar(aes(ymax = mean + std_error, ymin = mean - std_error), width = 0.1) +
    geom_point(shape = 21, fill = "gray25", size = 3) +
    geom_smooth(color = "black", formula = "y ~ x", method = "lm", se = F) +
    labs(x = "Year", y = y.lab) +
    supportR::theme_lyon()
  
  }
  
  # Return generated figure
  return(q)
  
}