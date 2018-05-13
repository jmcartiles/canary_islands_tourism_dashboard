

# load necessary functions

# dygraphs data with units in euros
custom_dygraph <- function(df, euros, is.variation = FALSE, is.mill = TRUE) {
  
  
  if(is.variation) {
    prueba <- c(min(df[,-1], na.rm = TRUE) -10,max(df[,-1], na.rm = TRUE) +10)
  } else{
    prueba <- NULL
  }
  
  xts(df, as.Date(df$fecha, format = "%Y-%m-%d %H:%M:%OS")) %>%
    dygraph() %>%
    dyAxis(
      name = "y",
      valueRange = prueba,
      valueFormatter = 'function(d){return d}',
      axisLabelFormatter = ifelse(is.variation == TRUE, 'function(d){return (d) + " %"}',
                                  ifelse(is.variation == FALSE & euros == TRUE & is.mill == TRUE,
                                         'function(d){return Math.round(d/1e6) + " mill.(\u20ac)"}',
                                         'function(d){return d}'))
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