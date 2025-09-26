# deploy_script.R
# VMI Baseball App Deployment Script
# Deploys the Shiny app to shinyapps.io

library(rsconnect)

# Deploy to shinyapps.io
deploy_app <- function() {
  tryCatch({
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
