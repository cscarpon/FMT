pc_obj <- setRefClass(
  "las_obj",
  fields = list(
    data = "data.frame",
    LPC = "LAS",
    CHM = "SpatRaster",
    DTM = "SpatRaster",
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
            ground <- lidR::classify_ground(.self$LPC, algorithm = pmf(ws = 5, th = 3))
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
    to_dtm = function(resolution = 0.5) {
      dtm <- lidR::rasterize_terrain(.self$LPC, resolution, tin())
      dtm <- terra::rast(dtm)
      .self$DTM <- dtm
      return(dtm)
    },
    to_chm = function(res = 0.5) {
      fill_na <- function(x, i=5) { if (is.na(x)[i]) { return(mean(x, na.rm = TRUE)) } else {return(x[i])}}
      w <- matrix(1, 3, 3)
      dtm <- .self$DTM
      nlas <- lidR::normalize_height(.self$LPC, dtm)
      chm <- lidR::rasterize_canopy(nlas, res, p2r(0.2, na.fill = tin()))
      filled <- terra::focal(chm, w, fun = fill_na)
      clamp <- terra::clamp(filled, lower = 0)
      .self$CHM <- clamp
      # Return the CHM
      return(clamp)
    },
    save_las = function(path) {
      lidR::writeLAS(.self$LPC, path)
    },
    save_dtm = function(path) {
      terra::writeRaster(.self$DTM, path, overwrite = TRUE)
    },
    save_chm = function(path) {
      terra::writeRaster(.self$CHM, path, overwrite = TRUE)
    }
  )
)