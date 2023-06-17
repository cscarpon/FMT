import open3d as o3d
import numpy as np

# Read .las files
pcd1 = o3d.io.read_point_cloud("C:/Users/User/Documents/Python_Scripts/TTP/LAS/Clipped/TTP_2019.xyz")
pcd2 = o3d.io.read_point_cloud("C:/Users/User/Documents/Python_Scripts/TTP/LAS/Clipped/TTP_2014.xyz")
pcd3 = o3d.io.read_point_cloud("C:/Users/User/Documents/Python_Scripts/TTP/LAS/Clipped/TTP_2021.xyz")

# Apply voxel downsampling for efficiency
pcd1 = pcd1.voxel_down_sample(voxel_size=0.02)
pcd2 = pcd2.voxel_down_sample(voxel_size=0.02)
pcd3 = pcd3.voxel_down_sample(voxel_size=0.02)

# Estimate normals
pcd1.estimate_normals(search_param=o3d.geometry.KDTreeSearchParamHybrid(radius=0.05, max_nn=30))
pcd2.estimate_normals(search_param=o3d.geometry.KDTreeSearchParamHybrid(radius=0.05, max_nn=30))
pcd3.estimate_normals(search_param=o3d.geometry.KDTreeSearchParamHybrid(radius=0.05, max_nn=30))

# Apply Point-to-Plane ICP registration
reg_p2p = o3d.pipelines.registration.registration_icp(
    pcd1, pcd3, 0.01, np.eye(4),
    o3d.pipelines.registration.TransformationEstimationPointToPlane())

# Apply Point-to-Plane ICP registration
reg_p2p2 = o3d.pipelines.registration.registration_icp(
    pcd1, pcd2, 0.01, np.eye(4),
    o3d.pipelines.registration.TransformationEstimationPointToPlane())

print("Transformation is:")
print(reg_p2p.transformation)


# Apply transformation on the point cloud
transformed_pcd3 = pcd3.transform(reg_p2p.transformation)
transformed_pcd2 = pcd2.transform(reg_p2p2.transformation)

# Write point cloud to file
o3d.io.write_point_cloud("C:/Users/User/Documents/Python_Scripts/TTP/LAS/Clipped/TTP_2014_ICP.xyz", transformed_pcd2)



import open3d as o3d
import numpy as np
import os

# Define the base directory
base_dir = "C:/Users/User/Documents/Python_Scripts/TTP/LAS/Clipped/"

# Define the paths to your point cloud files and the output directory
pcd_filenames = ["TTP_2019.xyz", "TTP_2014.xyz", "TTP_2021.xyz"]
pcd_paths = [os.path.join(base_dir, filename) for filename in pcd_filenames]

# Create output filenames based on the input filenames
output_filenames = [os.path.splitext(filename)[0] + "_ICP.xyz" for filename in pcd_filenames]
output_paths = [os.path.join(base_dir, filename) for filename in output_filenames]

# Read reference point cloud (first file)
pcd1 = o3d.io.read_point_cloud(pcd_paths[0])

# Preprocess reference point cloud
voxel_size = 0.02
pcd1 = pcd1.voxel_down_sample(voxel_size=voxel_size)
pcd1.estimate_normals(search_param=o3d.geometry.KDTreeSearchParamHybrid(radius=0.05, max_nn=30))

# Iterate over the rest of the point cloud files
for i in range(1, len(pcd_paths)):
    # Read point cloud
    pcd = o3d.io.read_point_cloud(pcd_paths[i])
    
    # Preprocess point cloud
    pcd = pcd.voxel_down_sample(voxel_size=voxel_size)
    pcd.estimate_normals(search_param=o3d.geometry.KDTreeSearchParamHybrid(radius=0.05, max_nn=30))

    # Apply Point-to-Plane ICP registration
    reg_p2p = o3d.pipelines.registration.registration_icp(
        pcd, pcd1, 0.01, np.eye(4),
        o3d.pipelines.registration.TransformationEstimationPointToPlane())

    print("Transformation for ", pcd_filenames[i], " is:")
    print(reg_p2p.transformation)

    # Apply transformation on the point cloud
    transformed_pcd = pcd.transform(reg_p2p.transformation)

    # Write point cloud to file
    o3d.io.write_point_cloud(output_paths[i], transformed_pcd)
    
    
    
import open3d as o3d
import numpy as np
import os

class PointCloudAligner:
    def __init__(self, base_dir, voxel_size=0.02, radius=0.05, max_nn=30):
        # Initialize the class with the base directory and parameters for processing
        self.base_dir = base_dir
        self.voxel_size = voxel_size
        self.radius = radius
        self.max_nn = max_nn
        self.reference_pcd = None

    def preprocess_point_cloud(self, pcd):
        # Function to preprocess the point cloud data by voxel downsampling and estimating normals
        pcd = pcd.voxel_down_sample(voxel_size=self.voxel_size)
        pcd.estimate_normals(search_param=o3d.geometry.KDTreeSearchParamHybrid(radius=self.radius, max_nn=self.max_nn))
        return pcd

    def align_point_clouds(self, pcd_filenames):
        # Function to align multiple point clouds to the first one in the list

        # Construct the full paths to the point cloud files
        pcd_paths = [os.path.join(self.base_dir, filename) for filename in pcd_filenames]

        # Generate output filenames based on the input filenames
        output_filenames = [os.path.splitext(filename)[0] + "_ICP.xyz" for filename in pcd_filenames]

        # Construct the full paths to the output files
        output_paths = [os.path.join(self.base_dir, filename) for filename in output_filenames]

        # Read reference point cloud (first file in the list)
        self.reference_pcd = o3d.io.read_point_cloud(pcd_paths[0])

        # Preprocess reference point cloud
        self.reference_pcd = self.preprocess_point_cloud(self.reference_pcd)

        # Iterate over the rest of the point cloud files
        for i in range(1, len(pcd_paths)):
            # Read and preprocess point cloud
            pcd = o3d.io.read_point_cloud(pcd_paths[i])
            pcd = self.preprocess_point_cloud(pcd)

            # Apply Point-to-Plane ICP registration to align the point cloud to the reference
            reg_p2p = o3d.pipelines.registration.registration_icp(
                pcd, self.reference_pcd, self.voxel_size, np.eye(4),
                o3d.pipelines.registration.TransformationEstimationPointToPlane())

            print(f"Transformation for {pcd_filenames[i]} is:")
            print(reg_p2p.transformation)

            # Apply the calculated transformation to the point cloud
            transformed_pcd = pcd.transform(reg_p2p.transformation)

            # Write the aligned point cloud to an output file
            o3d.io.write_point_cloud(output_paths[i], transformed_pcd)

# Usage
base_dir = "C:/Users/User/Documents/Python_Scripts/TTP/LAS/Clipped/"
pcd_filenames = ["TTP_2019.xyz", "TTP_2014.xyz", "TTP_2021.xyz"]

# Create an instance of the PointCloudAligner class
aligner = PointCloudAligner(base_dir)

# Align the point clouds and write the output files
aligner.align_point_clouds(pcd_filenames)