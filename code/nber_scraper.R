library(httr)
library(rvest)
library(tidyverse)

# Base URL
base <- "https://www.nber.org/system/files/working_papers/"

# Polite
ua <- user_agent("Myles Owens (myles.owens@stanford.edu) Please contact if you have any concerns")

# Pick 10 random paper IDs
set.seed(555)
ids <- sample(0001:32000, 5)

# Construct URLs like w20846/w20846.pdf
pdf_urls <- paste0(base, "w", ids, "/w", ids, ".pdf")

print(pdf_urls)

# Create folder to store downloads
dir.create("nber_pdfs", showWarnings = FALSE)

# Try downloading each PDF
for (url in pdf_urls) {
  file <- file.path("nber_pdfs", basename(url))
  message("Downloading: ", url)
  
  # Respectful download with custom User-Agent
  res <- GET(url, ua)
  
  if (status_code(res) == 200) {
    writeBin(content(res, "raw"), file)
    message("✅ Saved to: ", file)
  } else {
    message("⚠️ Skipped (status ", status_code(res), "): ", url)
  }
  
  # Be polite: add a short random delay (1–3 seconds)
  pause <- runif(1, 1, 3)
  message("Sleeping for ", round(pause, 2), " seconds...")
  Sys.sleep(pause)
}
