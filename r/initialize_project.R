# initialize_project.R

# Load renv library
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}
library(renv)

# Set the working directory to the project directory
path <- normalizePath(file.path(getwd(), ".."))
setwd(path)

# Initialize the project
initializeProject <- function(path) {
  setwd(path)
  # Ensure renv is active in the project
  if (!file.exists(file.path(path, "renv"))) {
    renv::init()
  }
  # Restore the renv environment
  renv::restore()
}

# Call the initializeProject function
initializeProject(path)

# Additional step: Restore the Conda environment
system("conda env update --file environment.yml --prune")