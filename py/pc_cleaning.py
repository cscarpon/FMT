import pdal
import os
import json

class PDALClassifier:
    def __init__(self, input_path, output_path):
        self.input_path = input_path
        self.output_path = output_path
        self.pipeline = None

    def classify(self):
        # Define the PDAL pipeline
        pipeline_dict = {
            "pipeline": [
                {
                    "type": "readers.las",
                    "filename": self.input_path
                },
                {
                    # Noise removal using statistical outlier detection
                    "type": "filters.outlier",
                    "method": "statistical",
                    "mean_k": 8,
                    "multiplier": 2.5
                },
                {
                    "type": "writers.las",
                    "filename": self.output_path
                }
            ]
        }

        # Convert pipeline to JSON
        pipeline_json = json.dumps(pipeline_dict, indent=4)

        # Create and execute PDAL pipeline
        self.pipeline = pdal.Pipeline(pipeline_json)

        try:
            self.pipeline.execute()
            print(f"Classification and noise removal completed. Output saved to {self.output_path}")
        except RuntimeError as e:
            print(f"An error occurred while executing the PDAL pipeline: {e}")


# if __name__ == "__main__":
#     # Input and output paths for the point cloud
#     input_las = "path_to_your_input_file.laz"  # Replace with your input file path
#     output_las = "path_to_your_output_file_classified.laz"  # Replace with your desired output path

#     # Create classifier instance and run classification
#     classifier = PDALClassifier(input_las, output_las)
#     classifier.classify()


input_las = "G:/EMC/Projects/Aecon/Data/Cleaned/Original/Lidar/Laz/Group1/Group1_1.laz"  # Replace with your input file path
output_las = "G:/EMC/Projects/Aecon/Data/Cleaned/Original/Lidar/Laz/Group1/Group1_1_Classified.laz"  # Replace with your desired output path

# Create classifier instance and run classification
classifier = PDALClassifier(input_las, output_las)
classifier.classify()