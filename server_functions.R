

# load necessary functions

# dygraphs data with units in euros
custom_dygraph <- function(df, euros) {
  
  xts(df, as.Date(df$fecha, format = "%Y-%m-%d %H:%M:%OS")) %>%
    dygraph() %>%
    dyAxis(
      name = "y",
      valueFormatter = 'function(d){return d}',
      axisLabelFormatter = ifelse(euros == TRUE,
                                  'function(d){return Math.round(d/1e6) + " mill.(\u20ac)"}',
                                  'function(d){return Math.round(d/1e6) + " mill."}')
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