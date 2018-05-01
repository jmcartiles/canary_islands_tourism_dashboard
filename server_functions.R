

# load necessary functions

# dygraphs data with units in euros
custom_dygraph <- function(df, euros, is.variation = FALSE) {
  
  # if (is.variation == TRUE) {
  #   
  #   nums <- unlist(lapply(df, is.numeric))
  #   min.value <- min(df[,nums])
  #   max.value <- max(df[,nums])
  #   axis.range <- c(min.value, max.value)
  #   
  # } else {
  #   
  #   axis.range <- NULL
  #   
  # }
  
  xts(df, as.Date(df$fecha, format = "%Y-%m-%d %H:%M:%OS")) %>%
    dygraph() %>%
    dyAxis(
      name = "y",
      # valueRange = axis.range,
      valueFormatter = 'function(d){return d}',
      axisLabelFormatter = ifelse(is.variation == TRUE, 'function(d){return (d) + " %"}',
                                  ifelse(is.variation == FALSE & euros == TRUE, 'function(d){return Math.round(d/1e6) + " mill.(\u20ac)"}',
                                  'function(d){return Math.round(d/1e6) + " mill."}'))
    ) %>%
    dyOptions(fillGraph = TRUE, fillAlpha = 0.1) %>%
    dyRangeSelector()
  
}

# button download data in CSV
button_download_csv <- function(df) {
  
  downloadHandler(
    filename = function(){"thename.csv"}, 
    content = function(fname){
      write.csv(df, fname)
    }
  )
  
}