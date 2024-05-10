#%%
import os
import pandas as pd
import random
import proj_secrets
import sys

import asyncio
import aiosqlite
import anthropic
import backoff
import json
import logging
from tqdm import tqdm
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, confusion_matrix

here = os.path.dirname(sys.prefix)
os.chdir(here)

# Directory containing the txt files
TXT_FILES_DIR = proj_secrets.TXT_DIR

# Input CSV file path
INPUT_CSV = "./output/109k_papers_all_coded_for_pub1.csv"
OUTPUT_CSV = "./output/data-final.csv"

#%%

# Function to convert the txt file name back to a DOI
def txt_to_doi(txt_file):
    return txt_file.replace("_", "/").replace(".txt", "")

# Read the input CSV file using pandas
df = pd.read_csv(INPUT_CSV)

# Add the 'file_location' column to the DataFrame
def add_file_location(doi):
    txt_file = doi.replace("/", "_") + ".txt"
    txt_file_path = os.path.join(TXT_FILES_DIR, txt_file)
    
    if os.path.exists(txt_file_path):
        return os.path.abspath(txt_file_path)
    else:
        return ""

df["file_location"] = df["doi"].apply(add_file_location)


#%%

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

@backoff.on_exception(backoff.expo, Exception, max_tries=5)
async def process_row(semaphore, client, row, truncate, truncate_start, pbar, save_to_df=False):
    async with semaphore:
        try:
            doi = row["doi"]
            file_location = row["file_location"]

            if file_location:
                with open(file_location, "r") as file:
                    content = file.read()
                    words = content.split()
                    start_index = int(len(words) * truncate_start)
                    truncated_content = " ".join(words[start_index:start_index+truncate])

                prompt = f"Given the following text:\n\n{truncated_content}\n\nEvaluate whether this academic paper uses statistical inference. Indicators of this may include reference to regression analysis, statistical prediction, p-values, statistical significance, correlation analysis, sensitivity analysis, ANOVA, causal inference, and so on. Return ONLY a 1 or 0."

                message = await client.messages.create(
                    model="claude-3-haiku-20240307",
                    max_tokens=1000,
                    temperature=0,
                    messages=[
                        {
                            "role": "user",
                            "content": prompt
                        }
                    ]
                )

                model_response = message.content[0].text
                quant = 1 if "1" in model_response else 0

                if save_to_df:
                    row["stat_bool_llm"] = quant
                else:
                    print(f"DOI: {doi}, Quant: {quant}")
                    print(f"filename: {file_location}")

            else:
                logger.warning(f"File not found for DOI: {doi}")

        except Exception as e:
            logger.error(f"An error occurred: {e}")
            await asyncio.sleep(10)  # Wait for 10 seconds before retrying
            raise  # Re-raise the exception to trigger backoff

        pbar.update(1)
        return quant

async def process_rows(df, truncate, truncate_start, save_to_df=False):
    semaphore = asyncio.Semaphore(2)  # Limit the number of concurrent tasks to 10
    client = anthropic.AsyncAnthropic(api_key=os.environ["ANTHROPIC_API_KEY"])

    with tqdm(total=len(df), desc="Processing rows") as pbar:
        tasks = []
        for _, row in df.iterrows():
            task = asyncio.create_task(process_row(semaphore, client, row, truncate, truncate_start, pbar, save_to_df))
            tasks.append(task)

        responses = await asyncio.gather(*tasks)
        return responses

#%%

# Read the input TSV file
INPUT_TSV = "output/round2_3_handcoded_clean.csv"
df = pd.read_csv(INPUT_TSV, sep="\t")
df.rename(columns={'file_in': 'doi'}, inplace=True)

# Filter rows where stat_bool is 1
stat_bool_1_rows = df[df["stat_bool"] == 1]

# Filter rows where stat_bool is 0 or NA
stat_bool_0_na_rows = df[(df["stat_bool"] == 0) | (df["stat_bool"].isna())]

# Function to resolve the file location
def resolve_file_location(file_in):
    txt_file_path = os.path.join(proj_secrets.TXT_DIR, file_in + ".txt")
    if os.path.exists(txt_file_path):
        return os.path.abspath(txt_file_path)
    else:
        return ""


# Add the 'file_location' column to the DataFrame
stat_bool_1_rows["file_location"] = stat_bool_1_rows["doi"].apply(resolve_file_location)
stat_bool_0_na_rows["file_location"] = stat_bool_0_na_rows["doi"].apply(resolve_file_location)

# Sample 70 files from stat_bool_1_rows
selected_stat_bool_1_rows = stat_bool_1_rows[stat_bool_1_rows["file_location"] != ""].sample(n=70)

# Sample 30 files from stat_bool_0_na_rows
selected_stat_bool_0_na_rows = stat_bool_0_na_rows[stat_bool_0_na_rows["file_location"] != ""].sample(n=30)

# Combine the selected rows
selected_rows = pd.concat([selected_stat_bool_1_rows, selected_stat_bool_0_na_rows])

#%%

# Get the current event loop
loop = asyncio.get_event_loop()

# Schedule the execution of process_rows
task = loop.create_task(process_rows(selected_rows, truncate=5000, truncate_start=0.3))

task_test = loop.create_task(process_rows(selected_rows.loc[[1119]], truncate=4000, truncate_start=0.3))
task_test.result()

# # Wait for the task to complete
# loop.run_until_complete(task)

# Get the responses from the completed task
responses = task.result()

#%%
# Convert the responses to a list of integers
predicted_labels = [int(response) for response in responses]

# Get the true labels from the 'stat_bool' column
true_labels = selected_rows["stat_bool"].fillna(0).astype(int).tolist()

# Create a new DataFrame with the original values, predicted values, and additional columns
result_df = selected_rows.copy()
result_df["predicted_label"] = predicted_labels
result_df["true_label"] = true_labels
result_df["correct"] = result_df["predicted_label"] == result_df["true_label"]
result_df[result_df["correct"] == False][["doi", "file_location", "stat_bool", "predicted_label", "true_label", "correct"]].to_csv("./temp/llm-differences.csv", index=False)

#%%
# Calculate accuracy, precision, recall, and F1 score
accuracy = accuracy_score(true_labels, predicted_labels)
precision = precision_score(true_labels, predicted_labels)
recall = recall_score(true_labels, predicted_labels)
f1 = f1_score(true_labels, predicted_labels)

# Print the evaluation metrics
print("Accuracy:", accuracy)
print("Precision:", precision)
print("Recall:", recall)
print("F1 Score:", f1)

# Print the confusion matrix
cm = confusion_matrix(true_labels, predicted_labels)
print("Confusion Matrix:")
print(cm)

# Save the result DataFrame to a CSV file
# result_df.to_csv("result_analysis.csv", index=False)