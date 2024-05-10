#%%
import asyncio
import nest_asyncio
from pyppeteer import launch
import os
import sys
here = os.path.dirname(sys.prefix)
os.chdir(here)

# Apply the patch for asyncio
nest_asyncio.apply()

async def save_webpage(url, output_path):
    # Define a user agent
    user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"

    # Launch the browser
    browser = await launch()
    page = await browser.newPage()

    # Set the user agent
    await page.setUserAgent(user_agent)

    # Navigate to the URL
    response = await page.goto(url, {'waitUntil': 'networkidle0'})

    # Check if the page redirected
    if response.status in [301, 302]:
        redirect_url = response.headers['location']
        await page.goto(redirect_url, {'waitUntil': 'networkidle0'})

    # Get page content and save it to a file
    content = await page.content()
    with open(output_path, 'w') as file:
        file.write(content)

    # Close the browser
    await browser.close()

# URL and output file path
url = 'https://doi.org/10.1086/721745'
output_path = './temp/output5.html'  # Update this path

# Run the asynchronous function
asyncio.get_event_loop().run_until_complete(save_webpage(url, output_path))
