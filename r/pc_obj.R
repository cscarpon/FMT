pc_obj <- setRefClass(
  "point_cloud_obj",
  fields = list(
    data = "data.frame",
    LPC = "LAS",
    CHM = "SpatRaster",
    DTM = "SpatRaster",
    mask = "sfc",
    filepath =  "character",
    filename = "character"
  ),
  methods = list(
    initialize = function(file_path = character(0)) {
        lidR::set_lidr_threads(6)
        .self$filepath <- file_path
        .self$filename <- basename(file_path)
        dummy_spat <- terra::rast(extent = terra::ext(0, 1, 0, 1), res=1, vals=NA)
        .self$DTM <- dummy_spat
        .self$CHM <- dummy_spat
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
      mask <- mask_pc(.self$LPC)
      .self$mask <- mask
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
      mask <- terra::mask(dtm, terra::vect(.self$mask))
      .self$DTM <- mask
      print("Raster Info After assignment:")
      print(.self$DTM)
    },
    to_chm = function(resolution = 0.5)  {
      fill_na <- function(x, i=5) { if (is.na(x)[i]) { return(mean(x, na.rm = TRUE)) } else {return(x[i])}}
      w <- matrix(1, 3, 3)
      dtm <- .self$DTM
      nlas <- lidR::normalize_height(.self$LPC, dtm)
      chm <- lidR::rasterize_canopy(nlas, resolution, p2r(0.2, na.fill = tin()))
      filled <- terra::focal(chm, w, fun = fill_na)
      clamp <- terra::clamp(filled, lower = 0)
      mask <- terra::mask(clamp, terra::vect(.self$mask))
      .self$CHM <- mask
    },
    save_mask = function(path) {
      sf::st_write(.self$mask, path)
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


    # get_mask = function() {
    #   coords <- st_as_sf(.self$LPC@data[,c("X", "Y")], coords = c("X", "Y"), crs = lidR::projection(.self$LPC))
    #   mask <- concaveman(coords, concavity = 2, length_threshold = 0)
    #   mask <- st_as_sfc(mask)
    #   mask <- st_simplify(mask, preserveTopology = TRUE, dTolerance = 0.1)
    #   .self$mask <- mask
    # },

# las2014 <- pc_obj$new("C:/Users/User/Documents/Python_Scripts/TTP/LAS/Clipped/TTP_2014.laz")
# las2014$to_dtm()
# plot(las2014$DTM)
