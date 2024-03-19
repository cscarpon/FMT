meta_df <- data.frame(
  id = numeric(),
  file_path = character(),
  size_mb = numeric(),
  ext = character(),
  creation_date = as.POSIXct(character())
)


dummy_spat <- terra::rast(extent = terra::ext(0, 1, 0, 1), res = 1, vals = NA)

path_14 <- "C:/Users/cscar/FMT/data/TTP_2014_decimate.laz"
path_14 <- "data/TTP_2014_decimate.laz"
pc_14 <- spatial_container$new(path_14)


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