import pdal
import os
import json

class pdal_icp:
    def __init__(self, source_path, target_path):
        self.source = source_path
        self.target = target_path
        self.json = None
        self.arrays = None
        self.metadata = None
        self.log = None

    def align(self):
        aligned_path = self._generate_aligned_path(self.source)
        
        # Define the pipeline configuration as a dictionary
        pipeline_dict = {
            "pipeline": [
                {
                    "type": "readers.las",
                    "filename": self.source
                },
                {
                    "type": "readers.las",
                    "filename": self.target
                },
                {
                    "type": "filters.icp"
                },
                {
                    "type": "writers.las",
                    "filename": aligned_path
                }
            ]
        }

        # Convert the dictionary to a JSON string
        pipeline_json = json.dumps(pipeline_dict, indent=4)
        self.json = pipeline_json

        # Create and execute the PDAL pipeline
        try:
            pipeline = pdal.Pipeline(pipeline_json)
            pipeline.execute()

            # Retrieve the transformed point cloud
            self.arrays = pipeline.arrays
            self.metadata = pipeline.metadata
            self.log = pipeline.log
            
            print("ICP alignment completed successfully.")
            return aligned_path
        
        except RuntimeError as e:
            print(f"An error occurred while executing the PDAL pipeline: {e}")
            return None

    def _generate_aligned_path(self, source_path):
        base, ext = os.path.splitext(source_path)
        return f"{base}_aligned{ext}"
    
# if __name__ == "__main__":
#     source_path = "G:/Thesis/Sunnybrook/DataDir/pc_14.laz"
#     target_path = "G:/Thesis/Sunnybrook/DataDir/pc_19.laz"
    
#     icp_aligner = pdal_icp(source_path, target_path)
#     aligned_file = icp_aligner.align()
    
#     if aligned_file:
#         print(f"Aligned file created at: {aligned_file}")
#     else:
#         print("Alignment failed.")



# # Run the alignment
# source_path = "G:/Thesis/Sunnybrook/DataDir/pc_14.laz"
# target_path = "G:/Thesis/Sunnybrook/DataDir/pc_19.laz"
# icp_aligner = pdal_icp(source_path, target_path)
# aligned_file = icp_aligner.align()
# 
# if aligned_file:
#     print(f"Aligned file created at: {aligned_file}")
# else:
#     print("Alignment failed.")
