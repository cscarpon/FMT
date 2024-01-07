#Extract information from a file path to determine what shapefiles you have
extract_info <- function(file_path) {
  # Normalize the file path
  path_normal <- normalizePath(file_path, mustWork = FALSE)
  
  # Check if the path exists
  if (!file.exists(path_normal)) {
    stop("Path does not exist: ", path_normal)
  }
  
  # Define the pattern for file extensions
  exts <- "\\.(laz|las|xyz|csv|shp|tif|tiff|json)$"
  
  # List files matching the pattern
  data_list <- list.files(path_normal, pattern = exts, full.names = TRUE)
  
  # Initialize an empty data frame
  meta_df <- data.frame(
    id = numeric(),
    file_path = character(),
    size_mb = numeric(),
    ext = character(),
    creation_date = as.POSIXct(character())
  )
  
  # Loop over the files and extract information
  for (i in seq_along(data_list)) {
    
    object_id <- i
    
    # Extract file path
    current_file_path <- data_list[i]
    
    current_file_path <- normalizePath(current_file_path, mustWork = FALSE)
    
    
    # Extract file extension
    ext <- tools::file_ext(current_file_path)
    
    # Get file size
    size <- format(round(file.info(current_file_path)$size / (1024^2),2), nsmall = 2)
    
    # Get last modification date
    date <- file.info(current_file_path)$mtime
    formatted_date <- format(date, "%Y-%m-%d")
    
    # Append the information to the data frame
    meta_df <- rbind(meta_df, data.frame(id = object_id,
                                         file_path = current_file_path,
                                         size_mb = size,
                                         ext = ext,
                                         creation_date = formatted_date,
                                         stringsAsFactors = FALSE))
  }
  meta_df <- meta_df %>%
    dplyr::arrange(ext) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::select(id, dplyr::everything())
  
  return(meta_df)
}


# Function to for preprocessing of a raster. It aligns the source to the target, resamples and then masks the output so they have the same extent as the target
process_raster <- function(source, target, source_mask, target_mask,  method = "bilinear") {
  
  aligned <- FALSE
  aligned <- terra::compareGeom(pc_14$CHM, pc_19$CHM)
    
  if (!aligned) {
    # Assuming you have predefined masks for source and target
    
    # source_mask <- transform_polygon_crs(source_mask, target_mask, crs)

    union <- sf::st_union(source_mask, target_mask)
    union <- terra::vect(union)

    # Crop the source to match the target raster's extent.

    # source <- transform_raster_crs(source, target, crs)
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

#Calculate statistics for a raster and return a data frame to plot in ggplot2

plot_stats <- function(difference_raster) {
  rast_freq <- terra::freq(difference_raster)
  rast_freq$area <- rast_freq$count * 0.5 #Square metres

  # Handling NaN
  rast_freq$value[is.nan(rast_freq$value)] <- "NaN"
  
  # Define class labels
  class_labels <- c("Large Loss: > 10m loss",
                    "Loss: 2.5m to 10m loss",
                    "Minimal change: -2.5m to 2.5m",
                    "Growth: 2.5m to 10m growth",
                    "Large Growth: > 10m growth",
                    "NaN")
  
  # Define class values
  class_values <- c("1", "2", "3", "4", "5", "NaN")
  
  # Convert value to a factor
  rast_freq$value <- factor(rast_freq$value, levels = class_values, labels = class_labels)

  # Plot the area column with different colors for each bar and a legend
  ggplot(rast_freq, aes(x = value, y = area, fill = factor(value))) +
    geom_bar(stat = "identity") +
    labs(x = "Loss and Gain", y = "Area (m^2)", fill = "Class") +
    scale_x_discrete(labels=c('Large Loss', 'Loss', 'Minimal Change', 'Growth', 'Large Growth')) +
    ggtitle("Raster Statistics for Change Detection") +
    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"),
                      labels = class_labels, drop = FALSE) +
    theme_minimal() +
    scale_y_continuous(labels = comma)
}

# Show DF

show_df <- function(df) {
  print(df)
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

  m <- leaflet::leaflet() %>%
    leaflet::addTiles()

  m

  # Transform chm_mask
  mask <- terra::project(mask, "EPSG:4326")

  # Mask and project DTM_14
  if (!is_empty(dtm)) {
    dtm_m <- terra::project(dtm, "EPSG:4326")
    dtm_m <- terra::mask(dtm_m, mask)
  } else {
    dtm_m <- NULL
  }

  # Mask and project source_chm
  if (!is_empty(chm)) {
    chm_m <- terra::project(chm, "EPSG:4326")
    chm_m <- terra::mask(chm_m, mask)
  } else {
    chm_m <- NULL
  }

  # Project chm_diff
  diff <- if (!is_empty(chm_diff)) terra::project(chm_diff, "EPSG:4326") else NULL
  diff <- terra::clamp(diff, 1, 5)
  diff_round <- round(diff)

  m <- leaflet::addPolygons(m, data = mask, color = "red", group = "Mask")
  if (!is_empty(dtm)) {
    m <- leaflet::addRasterImage(m, dtm_m, group = "DTM", maxBytes = Inf)
  }

  if (!is_empty(chm)) {
    m <- leaflet::addRasterImage(m, chm_m, group = "CHM", maxBytes = Inf)
  }

  if (!is_empty(diff_round)) {
    colors <- c("darkorange", "orange", "white", "lightgreen", "darkgreen")
    hex_values <- apply(col2rgb(colors), 2, function(col) rgb(col[1], col[2], col[3], maxColorValue = 255))
    pal <- leaflet::colorNumeric(hex_values,
                                 terra::values(diff_round),
                                 na.color = "transparent")
    labels <- c("< -10", "-10 to -2.5", "-2.5 to 2.5", "2.5 to 10", "> 10")

    m <- leaflet::addRasterImage(m,
                                 diff_round,
                                 colors = pal,
                                 group = "Diff",
                                 maxBytes = Inf)
    m <- leaflet::addLegend(m,
                            colors = colors,
                            labels = labels,
                            position = "bottomright",
                            title = "Change in Tree Height (m)")
  }

  m <- leaflet::addLayersControl(m,
                                 overlayGroups = c("Mask", "DTM", "CHM", "Diff"),
                                 options = leaflet::layersControlOptions(collapsed = FALSE))

  return(m)
}

initial_map <- function(mask) {
  mask <- if (!is_empty_sfc(mask)) sf::st_transform(mask, 4326) else NULL

  m <- leaflet::leaflet() %>%
    leaflet::addTiles()

  if (!is.null(mask) && any(class(mask) %in% c("sf", "sfc"))) {
    m <- leaflet::addPolygons(m, data = mask, color = "red", group = "Mask")
  }
  m <- leaflet::addLayersControl(
    m,
    overlayGroups = c("Mask"),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )
  return(m)
}



extract_las_info <- function(las, slice_size = 1) {
  # Extract information
  extent <- las@header@PHB[24:28] # Extracting extent
  area <- (las@header@PHB[["Number of point records"]] / 100)
  epsg <- sf::st_crs(las)$epsg
  min_z <- las@header@PHB[["Min Z"]]
  max_z <- las@header@PHB[["Max Z"]]
  # Combine extent into a single string
  extent_str <- paste(extent[1], extent[2], extent[3], extent[4], sep = ", ")

  # Calculate the density
  # Creating the index to cut the data into slices
  s <- seq(min_z, max_z, slice_size) # Number of layers

  # Initial layout for density grid
  layout <- grid_density(las, res = 4)
  layout <- rast(nrows = nrow(layout),
                 ncols = ncol(layout),
                 extent = extent(layout))
  
  #Create a raster with all values set to 0
  values(layout) <- 0

  # Prepare to store densities
  density_rasters <- vector("list", length(s))
  for (i in seq_along(s)) {
    subset <- filter_poi(las, Z >= s[i] & Z < (s[i] + slice_size))
    if (!is.empty(subset))
      density_rasters[[i]] <- grid_density(subset, res = layout)
    else
      density_rasters[[i]] <- layout
  }

  # Ensure all rasters have the same extent
  for (i in seq_along(density_rasters)) {
    density_rasters[[i]] <- extend(density_rasters[[i]], ext(layout))
  }

  # Convert list of rasters to a SpatRaster
  density_rasters_full <- rast(density_rasters)
  names(density_rasters_full) <- paste0("Layer ", s)

  # Calculate the average density
  avg_density <- mean(values(density_rasters_full), na.rm = TRUE)

  # Creating a data frame
  las_info_df <- data.frame(extent = extent_str,
                            area_m = area,
                            density_per_unit = avg_density,
                            epsg = epsg,
                            min_z = min_z,
                            max_z = max_z,
                            stringsAsFactors = FALSE)
  return(las_info_df)

}

las_density <- function(las, slice_size = 1) {
  # Determine Z range from LAS header
  min_z <- las@header@PHB[["Min Z"]]
  max_z <- las@header@PHB[["Max Z"]]
  s <- seq(min_z, max_z, slice_size) # Number of layers

  # Initial layout for density grid
  layout <- grid_density(las, res = 4)
  layout <- rast(nrows = nrow(layout), ncols = ncol(layout), extent = extent(layout))
  values(layout) <- 0

  # Prepare to store densities
  density_rasters <- vector("list", length(s))
  for (i in seq_along(s)) {
    subset <- filter_poi(las, Z >= s[i] & Z < (s[i] + slice_size))
    if (!is.empty(subset))
      density_rasters[[i]] <- grid_density(subset, res = layout)
    else
      density_rasters[[i]] <- layout
  }

  # Ensure all rasters have the same extent
  for (i in seq_along(density_rasters)) {
    density_rasters[[i]] <- extend(density_rasters[[i]], ext(layout))
  }

  # Convert list of rasters to a SpatRaster
  density_rasters_full <- rast(density_rasters)
  names(density_rasters_full) <- paste0("Layer ", s)

  # Calculate the average density
  avg_density <- mean(values(density_rasters_full), na.rm = TRUE)

  return(list(
    average_density = avg_density,
    density_per_unit = avg_density * prod(res(density_rasters_full))
  ))
}