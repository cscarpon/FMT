# Function to for preprocessing of a raster. It aligns the source to the target, resamples and then masks the output so they have the same extent as the target
process_raster <- function(source, target, mask_layer, method = "bilinear") {
  aligned <- FALSE
  tryCatch({
    aligned <- terra::compareGeom(source, target, stopiffalse = FALSE, tolerance = 0.1)
  }, error = function(e) {
    print(paste("compareGeom error:", e$message))
    aligned <- FALSE
  })

  if (!aligned) {
    source <- terra::resample(source, target, method = method)
    # Crop the source to match the target raster's extent.
    source <- terra::crop(source, terra::ext(target))
  }
  source <- terra::mask(source, mask_layer)
  return(source)
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
    rclmat <- matrix(m, ncol = 3, byrow = TRUE)
    diff_class <- terra::classify(diff, rclmat, include.lowest = TRUE)
    # Return the classified difference
    # Write the output
    return(diff_class)
}

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

is_empty <- function(raster) {
    all(is.na(terra::values(raster)))
}

is_empty_sfc <- function(sfc) {
    length(sfc) == 0
}