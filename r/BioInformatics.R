source("global.R")
source("r/functions.R")
source("r/spatial_container.R")
source("r/meta_obj.R")

dir <- "./data/"
mo_dir <- mo$new(dir)
print(mo_dir$metadata)

pc_14 <- spatial_container$new(mo_dir$metadata$file_path[1])

pc_14$set_crs(32617)

# path_19 <- "data/TTP_2019_decimate.laz"
pc_19 <- spatial_container$new(mo_dir$metadata$file_path[2])
pc_19$set_crs(32617)

# las_21 <- lidR::readLAS("G:/Thesis/GoogleDrive/2021-DronePtCloud.las")
# las_21_decimate <- decimate_points(las_21, random(10))
# 
# writeLAS(las_21_decimate, "data/2021.laz")

las_21 <- lidR::readLAS("data/2021.laz")

# Classify ground

mycsf <- csf(sloop_smooth = TRUE, class_threshold = 2, cloth_resolution = 1, time_step = 1)
las <- classify_ground(las_21, mycsf)

pc_ground <- lidR::filter_poi(las, Classification == 2)


mask_21 <- mask_pc(pc_ground)

#Activate conda environment
reticulate::use_condaenv("EMT_conda", required = TRUE)

# Source the Python script
icp_module <- paste0(getwd(), "/py/icp_pdal.py")

reticulate::source_python(icp_module)

# Create instance of the ICP class
icp_aligner <- pdal_icp(pc_19$filepath, "data/2021.laz")

# Call the align method
icp_aligner$align()

remove(icp_aligner)


# WMS base URL
wms_url <- "https://gis.toronto.ca/arcgis/services/basemap/cot_ortho_2015_color_8cm/MapServer/WMSServer?"

# Layer name you want to scrape (you can find this from the GetCapabilities document)
layer_name <- "0"  # This is usually the default layer index



# Original bounding box
xmin <- -79.3449481800000029
ymin <- 43.613016450000003 
xmax <- -79.3205116126065519
ymax <- 43.6450682060423816


# Example: breaking the area into smaller tiles
tile_width <- (xmax - xmin) / 4
tile_height <- (ymax - ymin) / 4

for (i in 0:3) {
  for (j in 0:3) {
    tile_xmin <- xmin + i * tile_width
    tile_xmax <- tile_xmin + tile_width
    tile_ymin <- ymin + j * tile_height
    tile_ymax <- tile_ymin + tile_height
    
    bbox_tile <- paste(tile_xmin, tile_ymin, tile_xmax, tile_ymax, sep = ",")
    wms_url_tile <- paste0(
      wms_base_url,
      "service=WMS&version=1.1.1&request=GetMap&layers=0",
      "&bbox=", bbox_tile,
      "&width=2048&height=2048",  # Adjust resolution as needed
      "&srs=EPSG:4326",
      "&format=image/png"
    )
    
    # Download each tile
    ortho_raster_tile <- terra::rast(wms_url_tile)
    
    # Save each tile
    tile_filename <- paste0("D:/Data/City of Toronto/TTP/orthoimagery2015_tile_", i, "_", j, ".tif")
    writeRaster(ortho_raster_tile, tile_filename, format = "GTiff", overwrite = TRUE)
  }
}

bbox <- paste(xmin, ymin, xmax, ymax, sep = ",")

# WMS base URL
wms_base_url <- "https://gis.toronto.ca/arcgis/services/basemap/cot_ortho_2015_color_8cm/MapServer/WMSServer?"

# Construct the full WMS request URL
wms_url <- paste0(
  wms_base_url,
  "service=WMS&version=1.1.1&request=GetMap&layers=0",
  "&bbox=", bbox,
  "&width=256&height=256",  # Adjust width and height for the desired resolution
  "&srs=EPSG:4326",  # Coordinate Reference System
  "&format=image/png"  # Output format
)


gdallocationinfo "WMS:https://gis.toronto.ca/arcgis/services/basemap/cot_ortho_2015_color_8cm/MapServer/WMSServer?SERVICE=WMS&VERSION=1.1.1&
                REQUEST=GetMap&LAYERS=cot_ortho_2015_color_8cm&SRS=EPSG:3857&
                BBOX=-8820138.85,5391939.76,-8786437.17,5397014.38&
                FORMAT=image/png&TILED=true&TILESIZE=256&
                MINRESOLUTION=0.25" -geoloc -8803288.01 5394477.07 -xml -b 1

gdalwarp -t_srs "EPSG:3857" "WMS:http://your_wms_server_url?SERVICE=WMS&VERSION=1.1.1&
                REQUEST=GetMap&LAYERS=your_layer_name&SRS=EPSG:3857&
                BBOX=-8820138.85,5391939.76,-8786437.17,5397014.38&
                FORMAT=image/png&TILED=true&TILESIZE=256&
                MINRESOLUTION=0.25" "D:/Data/City of Toronto/TTP/orthoimagery2015_warped.tif"

gdal_translate -tr 1 1 -co COMPRESS=LZW "WMS:https://gis.toronto.ca:443/arcgis/services/basemap/cot_ortho_2015_color_8cm/MapServer/WmsServer?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap&LAYERS=0&CRS=EPSG:3857&BBOX=-8869931.930700,5395816.883800,-8842000.000000,5425000.000000&FORMAT=image/tiff" "D:/Data/City of Toronto/TTP/orthoimagery2015_tile1_resampled.tif"

gdal_translate -tr 1 1 -co COMPRESS=LZW -oo TIMEOUT=300 "WMS:https://gis.toronto.ca:443/arcgis/services/basemap/cot_ortho_2015_color_8cm/MapServer/WmsServer?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap&LAYERS=0&CRS=EPSG:3857&BBOX=-8869931.930700,5395816.883800,-8842000.000000,5425000.000000&FORMAT=image/tiff" "D:/Data/City of Toronto/TTP/orthoimagery2015_tile1_resampled_timeout.tif"

gdal_translate -tr 1 1 -co COMPRESS=LZW -oo TIMEOUT=300 "WMS:https://gis.toronto.ca:443/arcgis/services/basemap/cot_ortho_2015_color_8cm/MapServer/WmsServer?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap&LAYERS=0&CRS=EPSG:3857&BBOX=-8869931.930700,5410408.4419,-8855965.96535,5425000.000000&FORMAT=image/tiff" "D:/Data/City of Toronto/TTP/orthoimagery2015_tile1.tif"
# Download orthoimagery using the constructed WMS request URL
ortho_raster <- terra::rast(wms_url)

# Plot the orthoimagery
plot(ortho_raster)

ortho_raster_aggregated <- aggregate(ortho_raster, fact = 100, fun = mean)

# Check the new resolution
print(ortho_raster_aggregated)



tiles <- terra::makeTiles(ortho_raster, 40, filename = "D:/Data/City of Toronto/TTP/tile2015_.tif")



?makeTiles

# Define the number of rows and columns per chunk
nrows_chunk <- nrow(ortho_raster) / 10
ncols_chunk <- ncol(ortho_raster) / 10

# Create an empty raster to store the aggregated result
ortho_raster_aggregated <- terra::rast(nrows = nrows_chunk, ncols = ncols_chunk, 
                                ext = terra::ext(ortho_raster), crs = terra::crs(ortho_raster))

# Loop over chunks
for (row in seq(1, nrow(ortho_raster), by = nrows_chunk)) {
  for (col in seq(1, ncol(ortho_raster), by = ncols_chunk)) {
    # Define the extent of the chunk
    chunk_ext <- terra::ext(ortho_raster, row, row + nrows_chunk - 1, col, col + ncols_chunk - 1)
    
    # Extract the chunk
    chunk <- terra::crop(ortho_raster, chunk_ext)
    
    # Aggregate the chunk
    chunk_aggregated <- terra::aggregate(chunk, fact = 10, fun = mean)
    
    # Merge the aggregated chunk back into the full raster
    ortho_raster_aggregated <- terra::mosaic(ortho_raster_aggregated, chunk_aggregated, fun = mean)
  }
}


# Check the size of the ortho_raster object in memory
print(object.size(ortho_raster), units = "MB")

str(ortho_raster)

print(ortho_raster)

# Save the raster to a file (GeoTIFF format)
writeRaster(ortho_raster, "D:/Data/City of Toronto/TTP/orthoimagery2015.tif", options = c("COMPRESS=LZW"), overwrite = TRUE)
