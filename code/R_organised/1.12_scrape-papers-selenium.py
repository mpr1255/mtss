#%%
import os
import glob
import sqlite3
import pandas as pd
from selenium import webdriver
import concurrent.futures
import random

def fetch_dois_from_db(db_path):
    with sqlite3.connect(db_path) as conn:
        return pd.read_sql_query("SELECT doi FROM crossref", conn)

def get_existing_dois(data_folder):
    files = glob.glob(os.path.join(data_folder, "*.txt"))
    return [os.path.basename(f).replace('_', '/')[:-4] for f in files]

def download_page(doi):
    url = f"https://doi.org/{doi}"
    options = webdriver.ChromeOptions()
    options.add_argument("--headless")
    driver = webdriver.Chrome(options=options)
    driver.get(url)
    page_source = driver.page_source
    driver.quit()
    return doi, page_source

# Set up paths
database_path = "./data/data.sqlite"
data_folder = "./data/txt"

# Fetch DOIs from the database and existing files
dois_from_db = fetch_dois_from_db(database_path)
existing_dois = get_existing_dois(data_folder)

# Perform anti-join to find missing DOIs
missing_dois = dois_from_db[~dois_from_db['doi'].isin(existing_dois)]

# Randomly shuffle the missing DOIs
random.shuffle(missing_dois['doi'])

# Download pages in parallel using 10 workers
with concurrent.futures.ProcessPoolExecutor(max_workers=10) as executor:
    for doi, page_source in executor.map(download_page, missing_dois['doi']):
        with open(os.path.join(data_folder, doi.replace('/', '_') + ".html"), 'w', encoding='utf-8') as f:
            f.write(page_source)




# Practice stuff
# # Define the URL and the path for the output file
# from selenium import webdriver
# import time
# import os
# import sys
# import rpy2.robjects as robjects




# # Define the URL and the path for the output file
# url = "https://doi.org/10.1086/721745"
# output_path = "./temp/output7.html"  # Replace with your desired path

# # Set the current working directory to the directory of the Python interpreter
# here = os.path.dirname(sys.prefix)
# os.chdir(here)

# # Setup the Chrome WebDriver
# options = webdriver.ChromeOptions()
# options.add_argument("--headless")  # Run in headless mode
# options.add_argument("user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")
# driver = webdriver.Chrome(options=options)

# # go to page
# driver.get(url)

# # Get the page source
# page_source = driver.page_source

# # Close the browser
# driver.quit()

# # Check the size of the page source
# size_in_kb = sys.getsizeof(page_source) / 1024  # Size in kilobytes
# if size_in_kb < 20:
#     with open("scraping_log.txt", "a") as log_file:
#         log_file.write(f"Warning: The size of the page at {url} is smaller than expected ({size_in_kb:.2f} KB).\n")

# # Save the page source to a file
# with open("rendered_page.html", "w", encoding="utf-8") as f:
#     f.write(page_source)
