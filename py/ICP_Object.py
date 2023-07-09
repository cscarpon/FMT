# import open3d as o3d
# import numpy as np

class PointCloud:
    def __init__(self, path):
        self.path = path
        self.pcd = o3d.io.read_point_cloud(path)
        self.pcd = self.pcd.voxel_down_sample(voxel_size=0.02)
        self.pcd.estimate_normals(search_param=o3d.geometry.KDTreeSearchParamHybrid(radius=0.05, max_nn=30))

    def align(self, target):
        reg_p2p = o3d.pipelines.registration.registration_icp(
            self.pcd, target.pcd, 0.01, np.eye(4),
            o3d.pipelines.registration.TransformationEstimationPointToPlane())

        return reg_p2p.transformation

    def apply_transformation(self, transformation_matrix):
        self.pcd.transform(transformation_matrix)

    def save(self, filename):
        o3d.io.write_point_cloud(filename, self.pcd)



# pcd = o3d.io.read_point_cloud("C:/Users/User/Downloads/GSR6October2022-003.xyz") 



# # Instantiate the PointCloud objects for each .xyz file
# pcd1 = PointCloud("C:/Users/User/Downloads/GSR6October2022-003.xyz")
# pcd2 = PointCloud("C:/Users/User/Downloads/GSR5May2023-005.xyz")
# pcd3 = PointCloud("C:/Users/User/Documents/Python_Scripts/TTP/LAS/Clipped/TTP_2021.xyz")

# # Align pcd2 and pcd3 to pcd1
# transformation_pcd2 = pcd2.align(pcd1)
# transformation_pcd3 = pcd3.align(pcd1)

# # Apply the transformation
# pcd2.apply_transformation(transformation_pcd2)
# pcd3.apply_transformation(transformation_pcd3)

# # Save the transformed point clouds
# pcd2.save("C:/Users/User/Documents/Python_Scripts/TTP/LAS/Clipped/TTP_2014_transformed.xyz")
# pcd3.save("C:/Users/User/Documents/Python_Scripts/TTP/LAS/Clipped/TTP_2021_transformed.xyz")
