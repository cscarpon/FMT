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

path_19 <- "C:/Users/cscar/OneDrive/Documents/Data/TTP/Sample/TTP_2019_decimate.laz"
pc_19 <- pc_obj$new(path_19)


# import SciPy (it will be automatically discovered in "r-reticulate")
open3D <- import("open3D")
numpy <- import("numpy")

source_python("py/ICP_Object.py")

plot(pc_14$mask)

pc_14$to_dtm(5)
pc_19$to_dtm(5)

plot(pc_14$DTM)


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



# Function to generate CHM and classify the differences
aligned_chm
chm_diff_test <- CHM_diff_classify(aligned_chm[[1]], aligned_chm[[2]])

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

providerTileOptions()

if (!is_empty(DTM_14)) {
    DTM_14_lf <- terra::mask(DTM_14, terra::vect(sf_14))
    dtm <- leaflet::projectRasterForLeaflet(DTM_14_lf, "EPSG:4326")
} else {
    dtm <- NULL
}

if (!is_empty(source_chm)) {
    source_chm_lf <- terra::mask(source_chm, terra::vect(sf_14))
    chm <- terra::project(source_chm_lf, "EPSG:4326")
} else {
    chm <- NULL
}


diff <- if(!is_empty(chm_diff_test)) terra::project(chm_diff_test, "EPSG:4326") else NULL
mask <- if(!is_empty_sfc(chm_mask)) sf::st_transform(sf::st_as_sf(chm_mask), 4326) else NULL
pal <- if(!is.null(mask)) "rgba(173, 216, 230, 0.4)" else NULL


m <- leaflet() %>%
    addTiles() %>%
    addProviderTiles(providers$OpenTopoMap)

    if (!is.null(mask) && any(class(mask) %in% c("sf", "sfc"))) {
        m <- addPolygons(m, data = mask, color = "red", group = "Mask")
    }

    if (!is_empty(dtm)) {
        m <- addRasterImage(m, dtm, group = "DTM", maxBytes = Inf)
    }

    if (!is_empty(chm)) {
        m <- addRasterImage(m, chm, group = "CHM", maxBytes = Inf)
    }


    if (!is_empty(diff_round)) {
        colors <- c("darkorange", "orange", "white", "lightgreen", "darkgreen")
        rgb_values <- col2rgb(colors)

        # If you want to get them in the common #RRGGBB format:
        hex_values <- apply(rgb_values, 2, function(col) rgb(col[1], col[2], col[3], maxColorValue = 255))

        pal <- colorNumeric(hex_values, values(diff_clamp),na.color = "transparent")
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

m

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
