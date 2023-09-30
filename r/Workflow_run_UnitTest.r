source("fmt/global.R")
source("r/functions.R")
source("r/pc_obj.R")

reticulate::py_config()

#Methods

# to_dtm()        save_las()
# to_chm()        save_dtm()
# save_mask()     save_chm()
# save_pc()

#Functions
  
process_raster(source, target, mask_layer)
CHM_diff_classify(earlier, later)
raster_stats(raster)
mask_pc()

reticulate::py_config()


# conda_list()
use_condaenv("point_clouds", required = TRUE)
py_install("brotli")
# miniconda_update(path = miniconda_path())
# py_install("open3d", envname = "point_clouds")



path_14 <- "data/TTP_2014_decimate.laz"
pc_14 <- pc_obj$new(path_14)

pc_14$set_crs(32617)

path_19 <- "data/TTP_2019_decimate.laz"
pc_19 <- pc_obj$new(path_19)
pc_19$set_crs(32617)


# # import SciPy (it will be automatically discovered in "r-reticulate")
# open3D <- import("open3D")
# numpy <- import("numpy")

# source_python("py/ICP_Object.py")

plot(pc_14$mask)

pc_14$to_dtm(5)
pc_19$to_dtm(5)

pc_14$to_chm(5)
pc_19$to_chm(5)

sf_14 <- pc_14$mask
sf_19 <- pc_19$mask

DTM_14 <- pc_14$DTM
DTM_19 <- pc_19$DTM

rast_14 <- pc_14$CHM
rast_19 <- pc_19$CHM


#This function aligns the two rasters and returns aligned raster objects.
aligned_chm <- process_raster(pc_14$CHM, pc_19$CHM, source_mask = pc_14$mask, target_mask = pc_19$mask, crs = 4326, method = "bilinear")

source_chm <- aligned_chm[[1]]
target_chm <- aligned_chm[[2]]
chm_mask <- aligned_chm[[3]]

print(source_chm)
print(target_chm)

plot(source_chm)
plot(chm_diff_test)

unique(values(chm_diff_test))


# Function to generate CHM and classify the differences

chm_diff_test <- CHM_diff_classify(source_chm, target_chm)

displayMap(DTM_14, rast_14, chm_diff_test, chm_mask)


plot(chm_diff_test)

plot(chm_diff_test)

sf_14 <- pc_14$mask
sf_19 <- pc_19$mask

DTM_14 <- pc_14$DTM
DTM_19 <- pc_19$DTM

rast_14 <- pc_14$CHM
rast_19 <- pc_19$CHM

plot(sf_14)
plot(DTM_14_test)

# Plot the outputs
displayMap(DTM_14, rast_14, chm_diff_test, chm_mask)


rStats <- raster_stats(chm_diff_test)

plot_stats(chm_diff_test)



#Calculate statistics for a raster and return a data frame. Currently only does area of each class.
raster_stats <- function(raster) {
  rast_freq <- terra::freq(raster)
  rast_freq$area <- rast_freq$count * 0.5 #Square metres
  return(rast_freq)
}



       mask <- if(!is_empty_sfc(rv$pc1$mask)) sf::st_transform(rv$pc1$mask, 4326) else NULL
          pal <- if(!is.null(mask)) "rgba(173, 216, 230, 0.4)" else NULL

          m <- leaflet() %>%
                  addTiles()
              if (!is.null(mask) && any(class(mask) %in% c("sf", "sfc"))) {
              m <- addPolygons(m, data = mask, color = "red", group = "Mask")
            }
            m <- addLayersControl(
              m,
              overlayGroups = c("Mask"),
              options = layersControlOptions(collapsed = FALSE)
            )
            return(m)