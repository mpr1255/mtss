


















# Write the updated DataFrame to the output CSV file
df.to_csv(OUTPUT_CSV, index=False)
print(f"CSV file with 'file_location' column created: {OUTPUT_CSV}")

#%%
# Function to get a random number of rows from the DataFrame
def get_random_rows(num_rows):
    return df.sample(n=num_rows)

# Function to filter rows based on a list of DOIs from a file
def filter_rows_by_dois(doi_file):
    with open(doi_file, "r") as file:
        dois = [doi.strip() for doi in file.readlines()]
    
    return df[df["doi"].isin(dois)]

# Example usage of the functions
random_rows = get_random_rows(5)
print("Random rows:")
pd.set_option('display.max_colwidth', None)  # Display full column width
print(random_rows[["doi", "file_location"]])  # Display only the "doi" and "file_location" columns

#%%


