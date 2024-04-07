install.packages("reticulate")

library(reticulate)

use_python("C:/Users/cscar/anaconda3/envs/fmt/python.exe", required = TRUE)
py_config()

conda_list()


# create conda environment
conda_create("fmt_r")

# Install PDAL in the environment
conda_install("fmt_r", "pdal", channel = "conda-forge")

# Use the environment
use_condaenv("fmt_r", required = TRUE)

pdal <- import("pdal")

py_config()