import numpy as np
import open3d as o3d


def icp_alignment(pc_path, output_filenames):
    pcd = o3d.io.read_point_cloud(path)
    pcd = pcd.voxel_down_sample(voxel_size=0.02)
    pcd.estimate_normals(search_param=o3d.geometry.KDTreeSearchParamHybrid(radius=0.05, max_nn=30))


## Creating the transformation matrix of the point cloud that will be applied for the transformation
def align(source, target):
    reg_p2p = o3d.pipelines.registration.registration_icp(
    source.pcd, target.pcd, 0.01, np.eye(4),
    o3d.pipelines.registration.TransformationEstimationPointToPlane())
    return reg_p2p.transformation

## Applying the transformation matrix to the point cloud from align()
def apply_transformation(source, transformation_matrix):
    self.pcd.transform(transformation_matrix)

def save(self, filename):
    o3d.io.write_point_cloud(filename, self.pcd)
