pc_obj <- setRefClass(
  "point_cloud_container",
    fields = list(
      xyz = "data.frame",
      LPC = "LAS",
      CHM = "SpatRaster",
      DTM = "SpatRaster",
      hull = "sfc",
      filepath =  "character",
      filename = "character"
    ),
      methods = list(
        initialize = function(file_path = character(0)) {
          lidR::set_lidr_threads(6)
          .self$filepath <- file_path
          .self$filename <- basename(file_path)
          
          #Determine the extentsion of the filepath
          ext <- tools::file_ext(file_path)
          
          #create a dummy raster
          dummy_spat <- terra::rast(extent = terra::ext(0, 1, 0, 1), res=1, vals=NA)
          .self$DTM <- dummy_spat
          .self$CHM <- dummy_spat
          
          if (ext == "xyz") {
            
            #read the x,y,z file
            xyz_table <- read.table(file_path )
            
            #add the names to the columns
            names(xyz_table) <- c("X", "Y", "Z")
            
            #add the xyz object to the xyz list
            .self$xyz <<-  xyz_table
            
            #create a last from the xyz object
            las_xyz <- lidR::LAS(xyz_table)
            
            #classify the ground points
            ground <- lidR::classify_ground(las_xyz, algorithm = pmf(ws = 5, th = 3))
            las_xyz@data$Classification <- ground@data$Classification
            las_xyz@data$ReturnNumber <- 1
            las_xyz@data$NumberOfReturns <- 1
            
            #append the new las file to the list
            .self$LPC <<- las_xyz
            
            #Append the mask 
            .self$mask <<- mask_pc(las_xyz)
            
          } else if (ext == "las" || ext == "laz") {
            
            #Read the las files
            las <- lidR::readLAS(file_path)
            
            #append the new las file to the list
            .self$LPC <<-  las
            
            #Append the mask 
            .self$mask <<- mask_pc(las)
          }
        },
          
        get_data = function() {
          return(.self$xyz)
        },
        
        get_lpc = function() {
          return(.self$LPC)
        },
        
        to_xyz = function(path) {
          write.table(.self$xyz[,c("X", "Y", "Z")], path, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = " ")
        },
        
        to_dtm = function(resolution = 1) {
          dtm <- lidR::rasterize_terrain(.self$LPC, resolution, tin())
          .self$DTM  <<- dtm
        },
        
        to_chm = function( resolution = 1) {
          fill_na <- function(x, i=5) { if (is.na(x)[i]) { return(mean(x, na.rm = TRUE)) } else {return(x[i])}}
          w <- matrix(1, 3, 3)
          nlas <- lidR::normalize_height(.self$LPC,.self$DTM)
          chm <- lidR::rasterize_canopy(nlas, resolution, p2r(0.2, na.fill = tin()))
          filled <- terra::focal(chm, w, fun = fill_na)
          clamp <- terra::clamp(filled, lower = 0)
          .self$CHM <<- clamp
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
        },
        
        save_pc = function(path) {
          save(.self, file = path, overwrite = TRUE)
        }
    )
)

# # Save the S4 object to a .RData file
# save(my_s4, file = "my_s4.RData")