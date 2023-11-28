spc <- setRefClass(
    "spatial_container",
    fields = list(
        xyz = "data.frame",
        LPC = "list",
        CHM = "list",
        DTM = "list",
        mask = "list",
        shapes = "list",
        rasters = "list",
        filepath = "character",
        filename = "character",
        metadata = "data.frame"
    ),
    methods = list(
        initialize = function(file_path = character(0)) {
            file_info <- extract_info(file_path)
            .self$metadata <- file_info
        }
    )
)        

process_files <- function(.self, selection_input) {
    processed_files <- list()   
        for file_id in selection_input {
           data_path <- .self$metadata$filepath[file_id]
           data_ext <- .self$metadata$ext[file_id]

           if (data_ext == "xyz") {
                .self$xyz <- read.table(data_path)
                names(.self$xyz) <- c("X", "Y", "Z")
                las <- LAS(.self$xyz)
                ground <- lidR::classify_ground(las, algorithm = pmf(ws = 5, th = 3))
                las@data$Classification <- ground@data$Classification
                las@data$ReturnNumber <- 1
                las@data$NumberOfReturns <- 1
                append(.self$LPC, las)
            } else if (ext == "las" || ext == "laz") {
                las <- lidR::readLAS(data_path)
                append(.self$LPC, las)
            } else if (data_ext == "tif") {
                raster <- terra::rast(data_path)
                append(.self$raster, raster)


        }
      },



library(terra)
install_packages("terra")
raster_test <- rast("C:/Users/cscar/OneDrive/Documents/Old_2017_2023/FMT/data/TTP/Results/chm_14.tif")



# Compare two rasters to ensure that they have the same crs.
transform_raster_crs <- function(source_raster, target_raster, user_crs) {
  
  crs_input <- paste0("EPSG:", 4326)

  # If source raster lacks a CRS, assign it the reference CRS
  if (is.na(terra::crs(source_raster))) {
    terra::crs(source_raster) <- user_crs
  }

  # Check if the CRS of both rasters are the same
  if (terra::crs(source_raster) != terra::crs(target_raster)) {
    # Transform the CRS of the source raster to match the target raster
    source_raster <- terra::project(source_raster, target_raster)
  }

  return(source_raster)
}
numbers_list <- c(1,2,3,4,5)

test_list <- unlist()

sc <- new("spatial_container",filepath = "C:/Users/cscar/OneDrive/Documents/Data/TTP/Sample")

x <- MyObject$new(); ## make a new RC object from the generator
x; ## how the RC object prints itself
## Reference class object of class "MyObject"
## Field "name":
## character(0)
## Field "age":
## numeric(0)
is(x,'refClass'); ## it's an RC object
## [1] TRUE
isS4(x); ## it's also an S4 object; the RC OOP system is built on top of S4
## [1] TRUE
slotNames(x); ## only one S4 slot
## [1] ".xData"
x@.xData; ## it's an environment
## <environment: 0x602c0e3b0>
environment(x$getPrinter); ## the RC object environment is set as the closure of its methods
## <environment: 0x602c0e3b0>
ls(x@.xData,all.names=T); ## list its names; require all.names=T to get dot-prefixed names
## [1] ".->age"       ".->name"      ".refClassDef" ".self"        "age"          "field"
## [7] "getClass"     "name"         "show"
x@.xData$.self; ## .self pseudo-field points back to the self object
## Reference class object of class "MyObject"
## Field "name":
## character(0)
## Field "age":
## numeric(0)

lidR::set_lidr_threads(6)
            .self$filepath <- file_path
            .self$filename <- basename(file_path)
            dummy_spat <- terra::rast(extent = terra::ext(0, 1, 0, 1), res=1, vals=NA)
            .self$DTM <- dummy_spat
            .self$CHM <- dummy_spat
            ext <- tools::file_ext(file_path)
            if (ext == "xyz") {
                .self$xyz <- read.table(file_path)
                names(.self$xyz) <- c("X", "Y", "Z")
                .self$LPC <<- LAS(.self$xyz)
                ground <- lidR::classify_ground(.self$LPC, algorithm = pmf(ws = 5, th = 3))
                .self$LPC@data$Classification <- ground@data$Classification
                .self$LPC@data$ReturnNumber <- 1
                .self$LPC@data$NumberOfReturns <- 1
            } else if (ext == "las" || ext == "laz") {
                .self$LPC <- lidR::readLAS(file_path)
                if (is.empty(.self$LPC)) return(NULL)
                .self$xyz <- data.frame(X = .self$LPC@data$X,
                                        Y = .self$LPC@data$Y,
                                        Z = .self$LPC@data$Z,
                                        Intensity = .self$LPC@data$Intensity,
                                        ReturnNumber = .self$LPC@data$ReturnNumber,
                                        NumberOfReturns = .self$LPC@data$NumberOfReturns,
                                        Classification = .self$LPC@data$Classification
                                        )
            }
            .self$mask <<- mask_pc(.self$LPC)
        },
        set_crs = function(crs) {
            crs <- as.integer(crs)
            current_crs <- st_crs(.self$LPC)
            # If there's an existing CRS
            if (!is.null(current_crs)) {
                cat("The current CRS for LPC is:", current_crs$WKT, "\n")  # Reporting the existing CRS in WKT format
                if (current_crs$ep