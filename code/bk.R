library(tidyverse)
library(stringdist) 

# --- Timer Setup ---
script_start_time <- Sys.time()

cat("\n[1/2] Loading data and calculating BK distances (Optimized)...\n")
p1_start <- Sys.time()

# 1. Load & Process Data
df <- read_csv("WOS2022.csv", show_col_types = FALSE) %>%
  mutate(
    normalized_ref = str_to_lower(reference) %>% str_remove_all("[^a-z0-9]"),
    
    # --- OPTIMIZATION CHANGE ---
    # stringdist::stringdist is much faster than utils::adist for large vectors
    # method = "lv" specifies Levenshtein distance
    bk_lev_len = as.integer(stringdist(normalized_ref, normalized_ref[1], method = "lv"))
  )

p1_end <- Sys.time()
cat(sprintf("Processing complete. Duration: %s\n", round(p1_end - p1_start, 2)))

# 2. Save Data
# (Optional: Comment this out if you just want to test speed and not write to disk)
write_csv(df, "processed_references.csv")

# Total Script Runtime
cat("\n--- Total Runtime ---\n")
print(Sys.time() - script_start_time)