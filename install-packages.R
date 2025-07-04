

# function to be run for installing all necessary packages
package_load <- function(cran_packages = character(), github_packages = character()) {
  
  # CRAN packages 
  if (length(cran_packages) > 0) {
    missing <- cran_packages[!cran_packages %in% rownames(installed.packages())]
    if (length(missing) > 0) {
      install.packages(missing)
    }
  }
  
  # GitHub packages 
  if (length(github_packages) > 0) {
    if (!"devtools" %in% rownames(installed.packages())) {
      install.packages("devtools")
    }
    
    github_names <- sapply(github_packages, function(x) basename(x))
    missing_github <- github_packages[!github_names %in% rownames(installed.packages())]
    
    if (length(missing_github) > 0) {
      for (pkg in missing_github) {
        devtools::install_github(pkg, quiet = TRUE)
      }
    }
  }
  
  # loading
  all_packages <- c(cran_packages, sapply(github_packages, function(x) basename(x)))
  for (pkg in all_packages) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE))
  }
  
  cat("All packages loaded.")
}
