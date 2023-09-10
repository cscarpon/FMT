# Function to for preprocessing of a raster. It aligns the source to the target, resamples and then masks the output so they have the same extent as the target
process_raster <- function(source, target, source_mask, target_mask, crs = rv$crs, method = "bilinear") {
  aligned <- FALSE
  tryCatch({
    aligned <- terra::compareGeom(source, target, stopiffalse = FALSE, tolerance = 0.1)
  }, error = function(e) {
    print(paste("compareGeom error:", e$message, ", reprocessing rasters"))
    aligned <- FALSE
  })
  if (!aligned) {
    # Assuming you have predefined masks for source and target
    
    source_mask <- transform_polygon_crs(source_mask, target_mask, crs)

    union <- sf::st_union(source_mask, target_mask)
    union <- terra::vect(union)

    # Crop the source to match the target raster's extent.

    source <- transform_raster_crs(source, target, crs)
    source <- terra::crop(source, terra::ext(union))
    target <- terra::crop(target, terra::ext(union))

    source <- terra::resample(source, target, method = method)
  }
  # Apply masks to each of the raster layers which will be used for the difference.
  source <- terra::mask(source, union)
  target <- terra::mask(target, union)

  return(list(source = source, target = target, mask = union))
}

# Function to generate CHM and classify the differences
CHM_diff_classify <- function(earlier, later) {
    # Compute the difference
    diff <- later - earlier
    # Create a raster for the magnitude of change
    # Classify the differences
    m <- c(-Inf, -10, 1,
            -10, -2.5, 2,
            -2.5, 2.5, 3,
            2.5, 10, 4,
            10, Inf, 5)
    # rclmat <- matrix(m, ncol = 3, byrow = TRUE)
    # Create a matrix with the ranges for reclassification
    rclmat <- matrix(m, ncol = 3, byrow = TRUE)
    diff_class <- terra::classify(diff, rclmat, include.lowest = TRUE)
    # Return the classified difference
    # Write the output
    return(diff_class)
}
#Calculate statistics for a raster and return a data frame. Currently only does area of each class.
raster_stats <- function(raster) {
  rast_freq <- terra::freq(rast)
  rast_freq$area <- rast_freq$count * 0.5 #Square metres
  return(rast_freq)
}

plot_stats <- function(rast_stats) {
  class_labels <- c("Large Loss: > 10m loss",
                    "Loss: 2.5m to 10m loss",
                    "Minimal change: -2.5m to 2.5m",
                    "Growth: 2.5m to 10m growth",
                    "Large Growth: > 10m growth")

  # Plot the area column with different colors for each bar and a legend
  ggplot(rast_stats, aes(x = value, y = area, fill = factor(value))) +
    geom_bar(stat = "identity") +
    labs(x = "Loss and Gain", y = "Area (m^2)", fill = "Class") +
    ggtitle("names of things") +
    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"),
                      labels = class_labels, drop = FALSE) +
    theme_minimal() +
    scale_y_continuous(labels = comma)
}
#Check to ensure SpatRaster is not empty - used for Leaflet testing
is_empty <- function(raster) {
    all(is.na(terra::values(raster)))
}
#Check to ensure sfc is not empty - used for Leaflet testing
is_empty_sfc <- function(sfc) {
    length(sfc) == 0
}

# Compare two rasters to ensure that they have the same crs.
transform_raster_crs <- function(source_raster, target_raster, crs) {
  crs_input <- paste0("EPSG:", 4326)

  # If source raster lacks a CRS, assign it the reference CRS
  if (is.na(terra::crs(source_raster))) {
    terra::crs(source_raster) <- crs_input
  }

  # Check if the CRS of both rasters are the same
  if (terra::crs(source_raster) != terra::crs(target_raster)) {
    # Transform the CRS of the source raster to match the target raster
    source_raster <- terra::project(source_raster, target_raster)
  }

  return(source_raster)
}

transform_polygon_crs <- function(source_polygon, target_polygon, crs) {
  # If source polygon lacks a CRS, assign it the reference CRS
  crs_int <- as.integer(crs)
  if (is.na(sf::st_crs(source_polygon))) {
    sf::st_crs(source_polygon) <- sf::st_crs(crs_int)
  }

  # Check if the CRS of both polygons are the same
  if (sf::st_crs(source_polygon) != sf::st_crs(target_polygon)) {
    # Transform the CRS of the source polygon to match the target polygon
    source_polygon <- sf::st_transform(source_polygon, sf::st_crs(target_polygon))
  }
  
  return(source_polygon)
}

#Mask pc is called on initially. Every point cloud object will have a mask layer.

mask_pc <- function(pc) {
    decimate <- lidR::decimate_points(pc, random(1))

    # Check if there is a ground classification
    if (!"2" %in% unique(pc$Classification)) {

      # Classify ground
      pc_decimated <- lidR::classify_ground(pc, csf())
      pc_ground <- lidR::filter_poi(pc_decimated, Classification == 2)
    } else {
      pc_ground <- lidR::filter_poi(pc, Classification == 2)
    }
    coords <- sf::st_as_sf(pc_ground@data[,c("X", "Y")], coords = c("X", "Y"), crs = lidR::projection(pc))

    # Extract the geometry from the sf object
    geom <- sf::st_geometry(coords)

    # Define the raster extent
    r <- raster::raster(extent(coords), resolution = 1) # Adjust resolution as needed

    # Convert your sf object to SpatialPointsDataFrame
    pts_sp <- as(coords, "Spatial")

    # Rasterize
    r <- raster::rasterize(pts_sp, r)

    out_shp <- raster::rasterToPoints(r, spatial = TRUE)

    polygons_sf <- sf::st_as_sf(out_shp)
    poly_buff <- sf::st_buffer(polygons_sf, 25)
    poly_union <- sf::st_union(poly_buff)
    poly_union <- sf::st_buffer(poly_union, -24)
    sf::st_crs(poly_union) <- lidR::projection(pc)
    return(poly_union)
}

displayMap <- function(dtm, chm, chm_diff, mask) {

  # Mask and project DTM_14
  if (!is_empty(dtm)) {
    dtm <- rast(dtm)
    dtm_m <- terra::mask(dtm, terra::vect(mask))
    dtm_m <- terra::project(dtm_m, "EPSG:4326")
  } else {
    dtm_m <- NULL
  }

  # Mask and project source_chm
  if (!is_empty(chm)) {
    chm_m <- terra::mask(chm, terra::vect(mask))
    chm_m <- terra::project(chm_m, "EPSG:4326")
  } else {
    chm_m <- NULL
  }

  # Project chm_diff
  diff <- if(!is_empty(chm_diff)) terra::project(chm_diff, "EPSG:4326") else NULL

  diff_round <- terra::round(terra::clamp(diff))

  # Transform chm_mask
  mask <- if(!is_empty_sfc(mask)) sf::st_transform(sf::st_as_sf(mask), 4326) else NULL


  m <- leaflet() %>%
    addTiles()

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
    hex_values <- apply(col2rgb(colors), 2, function(col) rgb(col[1], col[2], col[3], maxColorValue = 255))
    
    pal <- colorNumeric(hex_values, values(diff_clamp), na.color = "transparent")
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