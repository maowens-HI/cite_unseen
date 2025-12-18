================================================================================
PROCESSED DATA
================================================================================

PURPOSE
-------
This folder contains cleaned, normalized, and analysis-ready data derived
from raw WOS and NBER sources.

EXPECTED FILES
--------------
wos_normalized.parquet
  - WOS corpus with normalized citation strings
  - Columns: original_reference, normalized_reference, doi, pub_year

nber_citations.parquet
  - Extracted citations from NBER papers
  - Columns: paper_id, citation_num, original_citation, normalized_citation,
             paper_date, pre_treatment

match_results.parquet
  - Fuzzy matching results
  - Columns: paper_id, citation_num, best_wos_match, match_score,
             potential_hallucination

FILE FORMAT
-----------
Prefer Parquet format for:
  - Efficient storage (compressed)
  - Fast read/write operations
  - Column-level access (memory efficient)
  - Cross-platform compatibility

Alternative: CSV for smaller files or maximum compatibility

PROCESSING LOG
--------------
Document processing steps here:

Date        | Step                      | Script               | Notes
------------|---------------------------|----------------------|-------------
YYYY-MM-DD  | WOS normalization         | 02_text_norm...R     | n records
YYYY-MM-DD  | NBER citation extraction  | 03_citation_ext...R  | n papers
YYYY-MM-DD  | Fuzzy matching            | 04_fuzzy_match...R   | n comparisons

DATA VALIDATION
---------------
Before using processed data, verify:
  [ ] Row counts match expected totals
  [ ] No unexpected NA values
  [ ] Normalized strings contain only lowercase letters
  [ ] Match scores are in range [0, 1]
  [ ] Paper dates are properly formatted

VERSIONING
----------
For major reprocessing, use version suffixes:
  - wos_normalized_v1.parquet
  - wos_normalized_v2.parquet

Or date suffixes:
  - match_results_20241201.parquet

CONTACT
-------
For data questions, contact: myles.owens@stanford.edu
================================================================================
