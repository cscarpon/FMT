source("app/global.R")
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

C:\Users\cscar\OneDrive\Documents\Data\TTP\Sample

path_14 <- "C:/Users/cscar/OneDrive/Documents/Data/TTP/Sample/TTP_2014_decimate.laz"
pc_14 <- pc_obj$new(path_14)

pc_14$set_crs(32617)

path_19 <- "C:/Users/cscar/OneDrive/Documents/Data/TTP/Sample/TTP_2019_decimate.laz"
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

processed_chm <- process_raster(selected_las()$CHM, selected_las2()$CHM, selected_las()$mask, selected_las2()$mask, crs = rv$crs, method = "bilinear")

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

displayMap(DTM_14, rast_14, chm_diff_test, chm_mask)

# Mask and project DTM
dtm_m <- terra::mask(dtm, chm_mask)
dtm_m <- terra::project(dtm_m, "EPSG:4326")
} else {
dtm_m <- NULL
}

  # Mask and project source_chm
  if (!is_empty(chm)) {
    chm_m <- terra::mask(chm, mask)
    chm_m <- terra::project(chm_m, "EPSG:4326")
  } else {
    chm_m <- NULL
  }

  # Project chm_diff
  diff <- if(!is_empty(chm_diff)) terra::project(chm_diff, "EPSG:4326") else NULL
  diff <- terra::clamp(diff)
  diff_round <- terra::round(diff)

  # Transform chm_mask
  mask <- if(!is_empty_sfc(mask)) terra::project(chm_diff, "EPSG:4326") else NULL


  m <- leaflet() %>%
        addTiles()

  
        m <- addPolygons(m, data = dtm_m, color = "red", group = "Mask")
      

        if (!is_empty(dtm)) {
          m <- addRasterImage(m, dtm, group = "DTM", maxBytes = Inf)
        }

        if (!is_empty(chm)) {
          m <- addRasterImage(m, chm, group = "CHM", maxBytes = Inf)
        }

        if (!is_empty(diff_round)) {
              colors <- c("darkorange", "orange", "white", "lightgreen", "darkgreen")
              hex_values <- apply(col2rgb(colors), 2, function(col) rgb(col[1], col[2], col[3], maxColorValue = 255))
              
              pal <- colorNumeric(hex_values, terra::values(diff_clamp), na.color = "transparent")
              labels <- c("< -10", "-10 to -2.5", "-2.5 to 2.5", "2.5 to 10", "> 10")

              m <- addRasterImage(m, diff_round, colors = pal, group = "Diff", maxBytes = Inf)
              m <- addLegend(m,
                            colors = colors,
                            labels = labels,
                            position = "bottomright",
                            title = "Change in Tree Height (m)")
          }

        m <- addLayersControl(m,
                              overlayGroups = c("Mask", "DTM", "CHM", "Diff"),
                              options = layersControlOptions(collapsed = FALSE))

  return(m)
}


mask_raster <- function(raster) {

    raster <- terra::rast(raster)
    mask <- vect(mask)
    crs_input <- paste0("EPSG:", 4326)

    if (is.na(terra::crs(raster))) {
    terra::crs(raster) <- crs_input
    }

      if (is.na(terra::crs(mask))) {
    terra::crs(mask) <- crs_input
    }

    # Check if the CRS of both rasters are the same
    if (terra::crs(raster) != terra::crs(mask)) {
    # Transform the CRS of the source raster to match the target raster
    mask <- terra::project(mask, terra::crs(raster))
    }

    raster <- terra::crop(raster, mask)
    pts <- terra::as.points(raster, values=TRUE)
    polygons_sf <- sf::st_as_sf(pts)
    poly_buff <- sf::st_buffer(polygons_sf, 25)
    poly_union <- sf::st_union(poly_buff)
    poly_union <- sf::st_buffer(poly_union, -24)
    return(poly_union)
}
