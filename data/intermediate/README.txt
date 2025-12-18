================================================================================
INTERMEDIATE DATA
================================================================================

PURPOSE
-------
This folder stores temporary and intermediate processing files that are
generated during the analysis pipeline but not needed for final results.

TYPICAL CONTENTS
----------------
- Batch processing checkpoints
- Partial results during long-running jobs
- Cached distance matrices
- Temporary merge files

EXAMPLES
--------
batch_001_matches.rds     - Fuzzy match results for papers 1-1000
batch_002_matches.rds     - Fuzzy match results for papers 1001-2000
wos_distance_matrix.rds   - Precomputed WOS internal distances
nber_extracted_raw.rds    - Raw citation extraction before cleaning

CLEANUP POLICY
--------------
Files in this folder are SAFE TO DELETE after:
  1. Final processed data is generated
  2. Analysis is complete
  3. Results are validated

To clean up intermediate files:
  R: unlink("data/intermediate/*", recursive = TRUE)
  Bash: rm -rf data/intermediate/*

CHECKPOINTING
-------------
For long-running jobs, use checkpointing:

  # Save checkpoint every N iterations
  if (i %% checkpoint_interval == 0) {
    saveRDS(results_so_far,
            sprintf("data/intermediate/checkpoint_%05d.rds", i))
  }

  # Resume from checkpoint
  existing <- list.files("data/intermediate", pattern = "checkpoint")
  if (length(existing) > 0) {
    last_checkpoint <- max(existing)
    results <- readRDS(file.path("data/intermediate", last_checkpoint))
    start_from <- extract_checkpoint_number(last_checkpoint) + 1
  }

NOTES
-----
- Do NOT commit intermediate files to git
- Use .rds format for R objects (faster than CSV)
- Include timestamps in filenames for debugging
- Delete old checkpoints after successful completion

CONTACT
-------
For questions, contact: myles.owens@stanford.edu
================================================================================
