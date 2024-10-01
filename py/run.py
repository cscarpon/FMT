# Run the alignment
source_path = "G:/Thesis/Sunnybrook/DataDir/pc_14.laz"
target_path = "G:/Thesis/Sunnybrook/DataDir/pc_19.laz"
icp_aligner = pdal_icp(source_path, target_path)
aligned_file = icp_aligner.align()

if aligned_file:
    print(f"Aligned file created at: {aligned_file}")
else:
    print("Alignment failed.")