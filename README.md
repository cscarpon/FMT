# Forest Monitoring Tool (FMT)
A Lidar Tool designed to help foresters, scientists, citizen scientists, and anyone who is curious enough to explore.

The tool combines most pre-processing, processing, and post-processing procedures for any point cloud, raster, and shapefile data in the click of a few buttons.

![image](https://github.com/cscarpon/FMT/assets/39811242/7beb7c32-2752-4aed-a78e-7c05fbf20a48)

Tools and Function: The following tools and data pipelines allow for the easy performance of complicated tasks. Users can also plot and interact with their data. (exploratory data analysis). 


Tools:

Pre-processing (Pipelines in the form of Tools):

Point Clouds
- Can provide transformations on both .las and .xyz (Most programs cannot use xyz files and can only use .las, ironically, ICP alignment can only use xyz files.)
- Transforming point clouds by CRS (.xyz, .las)
- Aligning point clouds with ICP Alignment
- Creating tight hulls,
- Ground classification
- normalization of point clouds (removing the ground heights, so that only trees are exposed).
- Validating point clouds

Rasters
raster resampling and alignment,
clipping,
mask generation,
raster validation for alignments

.shp,
Transforming,
Clipping,
Validating data,
unioning

Other inputs:
.json, .geojson, whatever I want really

Processing:

Point cloud differencing (I think I figured out how to do this with the Python Package that does the alignment),
Raster Differencing,
Canopy Height models,
Classification of rasters,
raster classification statistics

Post Processing:

Saving .laz, .xyz, .las
Mapping
