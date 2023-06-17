library(lidR)
library(terra)
library(sf)
library(reticulate)
#install.packages("reticulate")

path <- system.file("python", package = <package>)
reticulate::import_from_path(<module>, path = path, delay_load = TRUE)
import open3d as o3d
import numpy as np

# create a new environment
conda_create("point_clouds", python = "3.9")
# indicate that we want to use a specific virtualenv
#use_virtualenv("point_clouds")

# install open3d in the new environment
conda_install("point_clouds", "open3d", channel = "open3d-admin")

system.file()

icp <- import_from_path("icp_alignment", path = "C:/Users/User/R_Scripts/TTP/" )
o3d <- import("open3d")

reticulate::py_last_error()
import open3d as o3d
import numpy as np



rast <- rast("C:/Users/User/Documents/Python_Scripts/TTP/Results/chm_14.tif")



ttp_boundary <- st_read("C:/Users/User/Documents/Python_Scripts/TTP/Clean/TTP.shp")

PC_obj <- setRefClass(
  "las_obj",
  fields = list(
    data = "data.frame",
    LPC = "LAS",
    mask = "sf"
  ),
  methods = list(
    initialize = function(file_path = character(0)) {
        lidR::set_lidr_threads(6)
        ext <- tools::file_ext(file_path)
        if (ext == "xyz") {
            .self$data <- read.table(file_path)
            names(.self$data) <- c("X", "Y", "Z")
            .self$LPC <- LAS(.self$data)
            ground <- classify_ground(.self$LPC, algorithm = pmf(ws = 5, th = 3))
            .self$LPC@data$Classification <- ground@data$Classification
            .self$LPC@data$ReturnNumber <- 1
            .self$LPC@data$NumberOfReturns <- 1
        } else if (ext == "las" || ext == "laz") {
            .self$LPC <- lidR::readLAS(file_path)
        if (is.empty(.self$LPC)) return(NULL)
            .self$data <- data.frame(X = .self$LPC@data$X,
                                    Y = .self$LPC@data$Y,
                                    Z = .self$LPC@data$Z,
                                    Intensity = .self$LPC@data$Intensity,
                                    ReturnNumber = .self$LPC@data$ReturnNumber,
                                    NumberOfReturns = .self$LPC@data$NumberOfReturns,
                                    Classification = .self$LPC@data$Classification
                                    )
      }
    },
    set_crs = function(crs) {
      st_crs(.self$LPC) <- crs
      .self$LPC <- st_transform(.self$LPC, crs)
      .self$data <- data.frame(X = .self$LPC@data$X,
                                    Y = .self$LPC@data$Y,
                                    Z = .self$LPC@data$Z,
                                    ReturnNumber = .self$LPC@data$ReturnNumber,
                                    NumberOfReturns = .self$LPC@data$NumberOfReturns,
                                    Classification = .self$LPC@data$Classification
                                    )

    },
    get_mask = function() {
      coords <- st_as_sf(.self$LPC@data[,c("X", "Y")], coords = c("X", "Y"), crs = lidR::projection(.self$LPC))
      coords_sample <- st_sample(coords, size = (nrow(coords)*.01), type = "regular")
      buffer <- sf::st_buffer(coords_sample, 5)
      union <- sf::st_union(buffer)
      masked <- sf::st_buffer(union, -5)
      .self$mask <- masked
    },
    get_data = function() {
      return(.self$data)
    },
    get_meta = function() {
      return(.self$metadata)
    },
    get_lpc = function() {
      return(.self$LPC)
    },
    to_xyz = function(path) {
      write.table(.self$data[,c("X", "Y", "Z")], path, row.names=FALSE, col.names=FALSE, quote=FALSE, sep=" ")
    },
    to_chm = function(res = 0.5) {
      fill.na <- function(x, i=5) { if (is.na(x)[i]) { return(mean(x, na.rm = TRUE)) } else {return(x[i]) }}
      w <- matrix(1, 3, 3)
      dtm <- rasterize_terrain(.self$LPC, 0.5, tin())
      nlas <- normalize_height(.self$LPC, dtm)
      chm <- lidR::rasterize_canopy(nlas, res, p2r(0.2, na.fill = tin()))
      filled <- terra::focal(chm, w, fun = fill.na)
      clamp <- terra::clamp(filled, lower = 0)

      # Return the CHM
      return(clamp)
    }
  )
)

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

create_xyz <- function(self, path) {
  df <- data.frame(X = self$data$X, Y = self$data$Y, Z = self$data$Z)
  write.table(df,
              file = path,
              row.names = FALSE,
              col.names = FALSE,
              quote = FALSE,
              sep = " ")
  #return(df)
}

resample_pc <- function(self, density, update = TRUE) {
  self$LPC <- decimate_points(self$LPC, random(density))
  if (update == TRUE) {

  }

}

# Function to generate CHM and classify the differences
# Earlier is a raster that is earlier in time (2014)
# Later is the most recent raster (2019)
CHM_diff_classify <- function(earlier, later) {
    # Compute the difference
    diff <- later - earlier
    # Classify the differences
    m <- c(-Inf, -10, 1,
            -10, -2.5, 2,
            -2.5, 2.5, 3,
            2.5, 10, 4,
            10, Inf, 5)
    rclmat <- matrix(m, ncol = 3, byrow = TRUE)
    diff_class <- terra::classify(diff, rclmat, include.lowest = TRUE)
    diff <- list(diff_class)
    # Write the output
    return(diff)
}

# Andrew Pipeline

las2014 <- PC_obj$new("C:/Users/User/Downloads/GSR 6October2022-003.las")
las2019 <- PC_obj$new("C:/Users/User/Downloads/GSR 5May2023-005.las")

las2014$set_crs(32617)
las2019$set_crs(32617)


# Read LAS or XYZ file and convert it into our LAS object
las2014 <- PC_obj$new("C:/Users/User/Documents/Python_Scripts/TTP/LAS/Clipped/TTP_2014_transformed.xyz")
las2019 <- PC_obj$new(C:/Users/User/Downloads/GSR 6October2022-003.las)
las2021 <- PC_obj$new("C:/Users/User/Documents/Python_Scripts/TTP/LAS/Clipped/TTP_2021_transformed.xyz")

las2014$set_crs(32617)
las2021$set_crs(32617)

las2014$data

create_xyz(las2014, "C:/Users/User/Downloads/GSR 6October2022-003.xyz")
create_xyz(las2019, "C:/Users/User/Downloads/GSR 5May2023-005.xyz")



#str(las2019)

#las2019$get_mask()




chm14 <- las2014$to_chm(0.5)
chm19 <- las2019$to_chm(0.5)
chm21 <- las2021$to_chm(0.5)





chm14_processed <- process_raster(chm14, chm14, ttp_boundary)
chm19_processed <- process_raster(chm19, chm14, ttp_boundary)
chm21_processed <- process_raster(chm21, chm14, ttp_boundary)

terra::writeRaster(chm14_processed, "C:/Users/User/Documents/Python_Scripts/TTP/Results/chm_14.tif", overwrite = TRUE)
terra::writeRaster(chm19_processed, "C:/Users/User/Documents/Python_Scripts/TTP/Results/chm_19.tif", overwrite = TRUE)
terra::writeRaster(chm21_processed, "C:/Users/User/Documents/Python_Scripts/TTP/Results/chm_21.tif", overwrite = TRUE)

s <- c(chm14_processed, chm19_processed, chm21_processed)
time <- c(2014, 2019, 2021)

trend <- terra::app(s, fun = function(x) {
                            coef(lm(x ~ time)) 
                            })
slope <- trend[[1]]  # Slope of the trend line
intercept <- trend[[2]]  # Intercept of the trend line

# Plot the results
terra::plot(slope, main="Slope of trend line")
terra::plot(intercept, main="Intercept of trend line")


plot(chm14_processed)
plot(chm19_processed)


diff_14_19 <- CHM_diff_classify(chm14_processed, chm19_processed)
diff_14_19 <- diff_14_19[[1]]
plot(diff_14_19)


diff_19_21 <- CHM_diff_classify(chm19_processed, chm21_processed)


diff_14_21 <- CHM_diff_classify(chm14_processed, chm21_processed)





terra::writeRaster(diff_14_19, "C:/Users/User/Documents/Python_Scripts/TTP/Results/diff_14_19.tif")
terra::writeRaster(diff_19_21[[1]], "C:/Users/User/Documents/Python_Scripts/TTP/Results/diff_19_21.tif")
terra::writeRaster(diff_14_21[[1]], "C:/Users/User/Documents/Python_Scripts/TTP/Results/diff_14_21.tif")

plot(diff_14_19[[1]])
plot(diff_19_21[[1]])
plot(diff_14_21[[1]])




# method to recombine transformed coordinates with original data
recombine_las <- function(xyz_obj, transformed_pcd) {
  transformed_df <- data.frame(X = transformed_pcd$points[,1], Y = transformed_pcd$points[,2], Z = transformed_pcd$points[,3])
  additional_attr <- xyz_obj@data[ , !(names(xyz_obj@data) %in% c('X','Y','Z'))]
  combined_df <- cbind(transformed_df, additional_attr)
  las <- LAS(combined_df)
  return(las)
}
writeRaster()