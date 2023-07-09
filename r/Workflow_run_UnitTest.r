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



path_14 <- "C:/Users/User/Forest_Analyser_5000/data/TTP/LAS/Clipped/TTP_2014_decimate.laz"
pc_14 <- pc_obj$new(path_14)

path_19 <- "C:/Users/User/Forest_Analyser_5000/data/TTP/LAS/Clipped/TTP_2019_decimate.laz"
pc_19 <- pc_obj$new(path_19)


# import SciPy (it will be automatically discovered in "r-reticulate")
open3D <- import("open3D")
numpy <- import("numpy")

source_python("py/ICP_Object.py")

plot(pc_14$mask)

pc_14$to_dtm(1)
pc_19$to_dtm(1)

plot(pc_14$DTM)


pc_14$to_chm(1)
pc_19$to_chm(1)

plot(pc_14$CHM)

#This function aligns the two rasters and returns aligned raster objects.
chm_14_aligned <- process_raster(pc_14$CHM, pc_19$CHM, pc_19$mask, method = "bilinear")

chm_classified <- CHM_diff_classify(chm_14_aligned, pc_19$CHM)

plot(chm_classified)

print(paste("Executing task: Plot Leaflet. Please wait as the rasters are converted to its web format"))
dtm <- if(!is_empty(pc_14$DTM)) terra::project(pc_19$DTM, "EPSG:4326") else NULL
chm <- if(!is_empty(pc_14$CHM)) terra::project(pc_19$CHM, "EPSG:4326") else NULL
diff <- if(!is_empty(chm_classified)) terra::project(chm_classified, "EPSG:4326") else NULL
mask <- if(!is_empty_sfc(pc_19$mask)) sf::st_transform(pc_19$mask, 4326) else NULL
pal <- if(!is.null(mask)) "rgba(173, 216, 230, 0.4)" else NULL

plot(dtm)
plot(chm)
plot(diff)
plot(diff_raster)


m <- leaflet() %>%
    addProviderTiles(providers$OpenStreetMap)

if (!is_empty(dtm)) {
    m <- addRasterImage(m, raster::raster(dtm), group = "DTM", maxBytes = Inf)
}

if (!is_empty(chm)) {
    m <- addRasterImage(m, raster::raster(chm), group = "CHM", maxBytes = Inf)
}

diff_raster <- raster::ratify(diff_raster)

# Then create a Raster Attribute Table (RAT)
rat <- levels(diff_raster)[[1]]
rat$colors <- pal(rat$ID)

# Assign RAT to your raster
levels(diff_raster) <- rat

# Now add your raster image to the map
m <- addRasterImage(m, diff_raster, group = "CHM_Difference", maxBytes = Inf)

m



# Color palette
bins <- c(1, 2, 3, 4, 5, NA)  # Specify your bins, in this case, 1 to 5 and NA
palette <- c("red", "orange", "white", "lightgreen", "darkgreen", "white")  # Color for each bin, NA mapped to black
labels <- c("Loss - Greater than 10m", "Loss - from 2.5m to 10m", "No change", "Gain - from 2.5m to 10m", "Gain - Greater than 10m", "Out of range")  # Labels for each bin
pal <- colorBin(palette = palette, na.color = "white", bins = bins)  # Create color palette function with colorBin

diff_raster <- raster::raster(diff)
terra::values(diff) <- round(terra::values(diff))

terra::values(diff)[is.na(terra::values(diff))] <- NA  # Make sure NAs in diff are really NAs

diff_raster <- raster::as.factor(diff_raster)

plot(test)

unique(terra::values(diff_raster))


diff_raster <- diff  # assuming 'diff' is your SpatRaster

r <- rast(system.file("ex/elev.tif", package="terra"))
plet(r, main="Hi\nthere")

# Make sure NAs in diff are really NAs
values(diff_raster)[is.na(values(diff_raster))] <- NA

# Assuming you have created a color palette function 'pal' as before
m <- terra::plet(diff, col = pal, alpha = 0.8, main = "CHM_Difference", tiles = "OpenStreetMap", legend = "bottomright")

# Print the interactive map
m


m <- addRasterImage(m, diff, group = "CHM_Difference", maxBytes = Inf)

m

m <- addRasterImage(m, diff_raster, method = "ngb", group = "CHM_Difference", maxBytes = Inf)

m

    if (!is.null(mask) && any(class(mask) %in% c("sf", "sfc"))) {
        m <- addPolygons(m, data = mask, color = "red", group = "Mask")
    }

    m <- addLayersControl(
    m,
    overlayGroups = c("DTM", "CHM", "CHM_Difference", "Mask"),
    options = layersControlOptions(collapsed = FALSE)
    )

    m <- addLegend(
    m,
    pal = pal,
    values = ~bins,
    position = "bottomright",
    title = "Change in Height",
    labels = labels
    )
    
    print(paste("Completed task: Plot Leaflet"))
