import os
import pandas as pd

# Path to the main folder containing subfolders
main_folder_path = '/Users/lenahuttner/Desktop/Dissertation/Exp4_data/data_tier_shadow'
# List to store DataFrames from each subfolder
dfs = []

# Iterate through each subfolder
for subfolder_name in os.listdir(main_folder_path):
    subfolder_path = os.path.join(main_folder_path, subfolder_name)

    # Check if the item in the main folder is a subfolder
    if os.path.isdir(subfolder_path):
        # Construct the path to the 'production_df.csv' file in the subfolder
        csv_file_path = os.path.join(subfolder_path, 'df_production.csv')

        # Check if the 'production_df.csv' file exists in the subfolder
        if os.path.exists(csv_file_path):
            # Read the CSV file into a DataFrame and append it to the list
            df = pd.read_csv(csv_file_path)
            dfs.append(df)
             print(f"Added {csv_file_path} to the list.")
        else:
             print(f"No 'production_df.csv' file found in {subfolder_path}.")
     else:
         print(f"{subfolder_name} is not a subfolder.")

# Check if there are DataFrames in the list before concatenating
if dfs:
    # Concatenate all DataFrames in the list along the rows (axis=0)
    result_df = pd.concat(dfs, axis=0, ignore_index=True)

    # Path to save the final merged CSV file
    output_csv_path = '/Users/lenahuttner/Desktop/Dissertation/Exp4_data/data_tier_shadow/tier_shadow.csv'

    # Save the merged DataFrame to a new CSV file
    result_df.to_csv(output_csv_path, index=False)

    print(f"Merged CSV file saved at: {output_csv_path}")
else:
    print("No CSV files found in any subfolder.")
