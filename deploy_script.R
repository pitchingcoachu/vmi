# deploy_script.R
# VMI Baseball App Deployment Script
# Deploys the Shiny app to shinyapps.io

library(rsconnect)

# Deploy to shinyapps.io
deploy_app <- function() {
  tryCatch({
    # Install required packages if not already installed
    required_packages <- c("shiny", "tidyverse", "DT", "gridExtra", "ggplot2", 
                          "patchwork", "hexbin", "ggiraph", "httr2", "MASS", 
                          "curl", "readr", "lubridate", "stringr", "akima", 
                          "plotly", "RCurl", "jsonlite")
    
    for (pkg in required_packages) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        cat("Installing package:", pkg, "\n")
        install.packages(pkg)
      }
    }
    
    # Deploy the app
    deployApp(
      appDir = ".",
      appName = "vmi-baseball-analytics",  # You can change this name
      forceUpdate = TRUE,
      launch.browser = FALSE
    )
    cat("App deployed successfully!\n")
    return(TRUE)
  }, error = function(e) {
    cat("Deployment failed:", e$message, "\n")
    return(FALSE)
  })
}

# Run deployment
if (!interactive()) {
  success <- deploy_app()
  if (!success) quit(status = 1)
}
