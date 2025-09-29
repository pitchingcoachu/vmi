#!/usr/bin/env Rscript
# install_packages.R
# Script to install all required packages for VMI Baseball Analytics

# Set options for package installation
options(repos = c(CRAN = "https://cloud.r-project.org/"))
options(timeout = 300)
options(install.packages.compile.from.source = "never")

cat("VMI Baseball Analytics - Package Installation\n")
cat("=============================================\n")

# Function to install packages with error handling
install_package_safe <- function(pkg) {
  tryCatch({
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat("Installing", pkg, "...\n")
      install.packages(pkg, dependencies = TRUE, quiet = FALSE)
      cat("✓ Successfully installed:", pkg, "\n")
    } else {
      cat("✓ Already installed:", pkg, "\n")
    }
  }, error = function(e) {
    cat("✗ Failed to install", pkg, ":", e$message, "\n")
    return(FALSE)
  })
}

# Core packages (install first)
core_packages <- c(
  "shiny",
  "rsconnect"
)

cat("\nInstalling core packages...\n")
for (pkg in core_packages) {
  install_package_safe(pkg)
}

# Tidyverse components (install step by step)
tidyverse_packages <- c(
  "ggplot2",
  "dplyr", 
  "readr",
  "tibble",
  "tidyr",
  "stringr",
  "forcats",
  "purrr",
  "lubridate"
)

cat("\nInstalling tidyverse components...\n")
for (pkg in tidyverse_packages) {
  install_package_safe(pkg)
}

# Now install full tidyverse
cat("\nInstalling tidyverse meta-package...\n")
install_package_safe("tidyverse")

# Other required packages
other_packages <- c(
  "DT",
  "gridExtra",
  "patchwork", 
  "hexbin",
  "ggiraph",
  "httr2",
  "MASS",
  "curl",
  "akima",
  "plotly",
  "RCurl",
  "jsonlite"
)

cat("\nInstalling additional packages...\n")
for (pkg in other_packages) {
  install_package_safe(pkg)
}

# Verify installation by loading critical packages
cat("\nVerifying package installation...\n")
critical_packages <- c("shiny", "tidyverse", "DT", "ggplot2", "dplyr")

all_good <- TRUE
for (pkg in critical_packages) {
  tryCatch({
    library(pkg, character.only = TRUE)
    cat("✓ Successfully loaded:", pkg, "\n")
  }, error = function(e) {
    cat("✗ Failed to load:", pkg, "\n")
    all_good <<- FALSE
  })
}

if (all_good) {
  cat("\n🎉 All packages installed and verified successfully!\n")
  cat("Ready for deployment to shinyapps.io\n")
} else {
  cat("\n❌ Some packages failed to install or load.\n")
  cat("Please check the error messages above.\n")
  quit(status = 1)
}
