import pdal
import json

class PDAL_ICP:
    def __init__(self, target_path, source_path, output_path):
        self.fixed_path = target_path
        self.moving_path = source_path
        self.output_path = output_path
        self.metadata_path = "icp_metadata.json"

    def run_icp_alignment(self):
        # Define the PDAL pipeline JSON configuration
        pipeline_json = {
            "pipeline": [
                self.fixed_path,
                self.moving_path,
                {
                    "type": "filters.icp",
                    "max_iter": 1000,  # Maximum number of iterations
                    "min_diff": 1e-6,  # Minimum difference for convergence
                    "centroid": "auto",  # Calculate centroid automatically
                    "transform": "auto"  # Initial transformation (if needed)
                },
                self.output_path
            ]
        }

        # Execute the pipeline with metadata output
        pipeline = pdal.Pipeline(json.dumps(pipeline_json))
        count = pipeline.execute()
        metadata = json.loads(pipeline.metadata)

        # Save metadata to a file (optional)
        with open(self.metadata_path, 'w') as f:
            json.dump(metadata, f, indent=4)

        print(f"ICP Alignment completed with {count} points processed.")

    def get_transformation_matrix(self):
        # Load the metadata from the file
        with open(self.metadata_path, 'r') as f:
            metadata = json.load(f)

        # Extract the transformation matrix from the metadata
        icp_metadata = metadata["metadata"]["filters.icp"]
        transformation_matrix = icp_metadata["transform"]
        print("Transformation Matrix:", transformation_matrix)

        return transformation_matrix
