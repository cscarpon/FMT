source("global.R")
source("server.R")
source("r/functions.R")
source("r/spatial_container.R")
source("r/meta_obj.R")

set_lidr_threads(10)

writeLAS(las19_noiseless_clean, "F:/Thesis/Sunnybrook/DataDir/Sunnybrook_19_clean.laz")

writeLAS(las15_noiseless_clean, "F:/Thesis/Sunnybrook/DataDir/Sunnybrook_15_clean.laz")

#test

#Methods

# to_dtm()        save_las()
# to_chm()        save_dtm()
# save_mask()     save_chm()
# save_pc()

#Functions

# process_raster(source, target, mask_layer)
# CHM_diff_classify(earlier, later)
# raster_stats(raster)
# mask_pc()

dir <- "./data/"
# dir <- "G:/Thesis/Sunnybrook/DataDir/"
mo_dir <- mo$new(dir)
print(mo_dir$metadata)


# pc_14_raw <- spatial_container$new(mo_dir$metadata$file_path[3])
# pc_14_raw$set_crs(32617)
# mask_14 <- mask_pc(pc_14_raw$LPC)
# pc_14_raw$mask <- mask_14
# pc_14_raw$to_dtm()
# pc_14_raw$DTM
# writeRaster(pc_14_raw$DTM, file.path("./saves/dtm_raw15.tif"), gdal = c("COMPRESS=LZW"), overwrite = TRUE)

pc_14 <- spatial_container$new(mo_dir$metadata$file_path[2])
pc_14$set_crs(32617)

decimate_15 <- decimate_points(pc_14$LPC, random(1))
writeLAS(decimate_15, file.path("./data/SB_15_decimate.laz"))

# path_19 <- "data/TTP_2019_decimate.laz"
pc_19 <- spatial_container$new(mo_dir$metadata$file_path[4])
pc_19$set_crs(32617)


writeLAS(pc_14$LPC, "G:/Thesis/Sunnybrook/DataDir/pc_14.laz")
writeLAS(pc_19$LPC, "G:/Thesis/Sunnybrook/DataDir/pc_19.laz")

conda_create("EMT_conda", python = "3.9.13", packages = c("pdal", "numpy", "scipy"))

#Activate conda environment
reticulate::use_condaenv("EMT_conda", required = TRUE)
reticulate::import("pdal")

renv::use_python("C:/Users/cscar/anaconda3/envs/EMT_conda/python.exe")

# Source the Python script
icp_module <- paste0(getwd(), "/py/icp_pdal.py")

reticulate::source_python(icp_module)

# Create instance of the ICP class
icp_aligner <- pdal_icp(pc_14$filepath, pc_19$filepath)

# Call the align method
aligned_file_path <- icp_aligner$align()

# Process the source point cloud
pc_14A <- spatial_container$new(as.character(aligned_file_path))
pc_14$set_crs(32617)

#Plotting the mask

plot(pc_14$mask)

#Generating the DTM and CHM

pc_14$mask <- mask_pc(pc_14$LPC)
pc_19$mask <- mask_pc(pc_19$LPC)


pc_14$mask <- st_read("C:\\Users\\cscar\\FMT\\data\\SB_Mask15.shp")
pc_19$mask <- st_read("C:\\Users\\cscar\\FMT\\data\\SB_Mask19.shp")


st_write(pc_14$mask,"C:\\Users\\cscar\\FMT\\data\\SB_Mask15.shp")
st_write(pc_19$mask,"C:\\Users\\cscar\\FMT\\data\\SB_Mask19.shp")

pc_14$mask <- mask_source
pc_19$mask <- mask_target

pc_14$to_dtm(1)

plot(pc_14$DTM)

pc_19$to_dtm(1)

pc_14$to_chm(1)
plot(pc_14$CHM)
plot(pc_14$CHM_raw)


pc_19$to_chm(1)

plot(pc_19$CHM)


#This function aligns the two rasters and returns aligned raster objects.
aligned_chm <- process_raster(source = pc_14$CHM_raw, target = pc_19$CHM_raw, source_mask = pc_14$mask, target_mask = pc_19$mask, method = "bilinear")

source <- pc_14$CHM_raw
target <- pc_19$CHM_raw
source_mask <- pc_14$mask
target_mask <- pc_19$mask
method <- "bilinear"

aligned <- FALSE
aligned <- terra::compareGeom(source, target, stopOnError = FALSE)

if (!aligned) {
  # Assuming you have predefined masks for source and target
  
  # source_mask <- transform_polygon_crs(source_mask, target_mask, crs)
  
  union_sf <- sf::st_union(source_mask, target_mask)
  union_vect <- terra::vect(union_sf)
  
  # Crop the source to match the target raster's extent.
  
  # source <- transform_raster_crs(source, target, crs)
  source <- terra::crop(source, terra::ext(union_vect))
  target <- terra::crop(target, terra::ext(union_vect))
  
  source <- terra::resample(source, target, method = method)
}

# Apply masks to each of the raster layers which will be used for the difference.
source <- terra::mask(source, union_vect)
target <- terra::mask(target, union_vect)

return(list(source = source, target = target, mask = union_vect))

footprints <- sf::st_read("C:/Users/cscar/FMT/data/SB_Buildings.shp")

# Create a new raster with the same extent as SB_Change and 1m resolution
template_raster <- terra::rast(extent = terra::ext(source), resolution = 1)


# Step 3: Rasterize the building polygons onto the new 1m raster grid
buildings_raster <- terra::rasterize(terra::vect(footprints), template_raster, background = NA)

terra::crs(buildings_raster) <- terra::crs(source)

# Step 4: Set the overlapping raster cells in SB_Change to 0 where buildings exist
source[!is.na(buildings_raster)] <- 0
target[!is.na(buildings_raster)] <- 0



classified_diff <- diff_classify(source, target)
diff_class <- terra::mask(classified_diff, union)
plot(diff_class)


# Save the processed raster in the rv list so it can be accessed elsewhere
rv$classified_diff <- diff_class



st_write(pc_14$mask, paste0(getwd(), "/Data/mask_2015.shp"))
st_write(pc_19$mask, paste0(getwd(), "/Data/mask_2019.shp"))

################################################

################################################

source_chm <- aligned_chm[[1]]
target_chm <- aligned_chm[[2]]
chm_mask <- aligned_chm[[3]]

# Function to generate CHM and classify the differences

diff_class <- diff_classify(source_chm, target_chm)

plot_stats(diff_class)

plot(diff_class)


diff_numbers <- function(raster){
  # Get the values of the raster
  raster_values <- terra::values(raster)
  
  # Remove NA values if necessary
  raster_values <- raster_values[!is.na(raster_values)]
  
  # Get the count of each class
  class_counts <- table(raster_values)
  
  # Calculate the total number of cells
  total_cells <- sum(class_counts)
  
  # Calculate the percentage of each class
  class_percentages <- (class_counts / total_cells) * 100
  
  return(list(class_counts = class_counts, class_percentages = class_percentages))
}

# Get the values of the raster
raster_values <- terra::values(diff_class)

# Remove NA values if necessary
raster_values <- raster_values[!is.na(raster_values)]

# Get the count of each class
class_counts <- table(raster_values)

str(class_counts)

# Calculate the total number of cells
total_cells <- sum(class_counts)

# Calculate the percentage of each class
class_percentages <- (class_counts / total_cells) * 100

class_counts[[1]]
class_counts[[2]]
class_counts[[3]]
class_counts[[4]]
class_counts[[5]]

return(list(class_counts = class_counts, class_percentages = class_percentages))



# Display the results
print(class_percentages)

# Display the outputs/. 

displayMap(pc_14$DTM, pc_14$CHM, diff_class, chm_mask)


pc_14$save_sc(file.path("./saves/pc_14.RData"))
pc_19$save_sc(file.path("./saves/pc_19.RData"))

sc_test <- load(file.path("./saves/pc_14.RData"))

buildings_vect <- vect("C:/Users/cscar/FMT/data/SB_Buildings.shp")
buildings_vect <- project(buildings_vect, "EPSG:32617")
plot(buildings_vect)

plot(buildings_vect)

diff_masked <- terra::mask(diff_class, buildings_vect,inverse = TRUE, updatevalue = 3)
plot(diff_masked)
writeRaster(diff_masked, "C:/Users/cscar/FMT/data/diff_masked.tif", gdal = c("COMPRESS=LZW"))


pc_14$CHM <- chm_masked
plot(chm_masked)

chm_masked <- terra::mask(pc_14$CHM_raw, buildings_vect,inverse = TRUE, updatevalue = 0)
pc_14$CHM_raw <- chm_masked
plot(chm_masked)


chm_masked <- terra::mask(pc_19$CHM, buildings_vect,inverse = TRUE, updatevalue = 0)
pc_19$CHM <- chm_masked

chm_masked <- terra::mask(pc_19$CHM_raw, buildings_vect,inverse = TRUE, updatevalue = 0)
pc_19$CHM_raw <- chm_masked
plot(chm_masked)

pc_14_copy <- pc_14
pc_19_copy <- pc_19

pc_14$save_sc(file.path("./saves/pc_14.rds"))
pc_19$save_sc(file.path("./saves/pc_19.rds"))


pc_19 <- load(file.path("./data/pc_19.rds"))


saveRDS(pc_19_copy, file = "C:/Users/cscar/FMT/data/pc_19.rds")
pc_19 <- readRDS("C:/Users/cscar/FMT/data/pc_19.rds")
pc_19
