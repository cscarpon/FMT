# Sys.setenv(RETICULATE_PYTHON = "C:/Users/cscar/anaconda3/envs/fmt_env/python.exe")

# Load the reticulate package
library(reticulate)

# Use the conda environment
use_condaenv("fmt_env")

# Verify the Python configuration
py_config()

# Use a Python package
np <- import("numpy")
pdal <- import("pdal")

# Source the Python script
icp_module <- paste0(getwd(), "/py/icp_pdal.py")

source_python(icp_module)

#Create instance of the ICP class
icp_aligner <- pdal_icp("C:/Users/cscar/FMT/data/TTP_2015.laz", "C:/Users/cscar/FMT/data/TTP_2019_32617.laz")

# Call the align method
aligned_file_path <- icp_aligner$align()

