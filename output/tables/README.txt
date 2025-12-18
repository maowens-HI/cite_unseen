================================================================================
TABLES
================================================================================

PURPOSE
-------
Statistical tables, regression results, and summary statistics.

EXPECTED OUTPUTS
----------------
tab_01_summary_statistics.csv
  - Descriptive statistics for key variables
  - Pre/post treatment breakdown
  - WOS corpus summary

tab_02_match_score_summary.csv
  - Distribution of match scores
  - Percentiles (25th, 50th, 75th, 90th, 99th)
  - Hallucination rate by threshold

tab_03_regression_main.tex
  - Main causal estimation results
  - DiD or RDD coefficients
  - Standard errors, confidence intervals

tab_04_regression_robustness.tex
  - Sensitivity to threshold choice
  - Alternative specifications
  - Placebo tests

tab_05_paper_level_results.csv
  - Per-paper hallucination counts
  - For manual validation sampling

FORMATS
-------
CSV:  For data tables, easy import into other tools
TEX:  For LaTeX/publication-ready tables
HTML: For reports and presentations

LaTeX tables should use:
  - booktabs package (toprule, midrule, bottomrule)
  - siunitx for number alignment
  - threeparttable for notes

REGENERATION
------------
# Generate all tables
source("src/06_causal_estimation.R")

# Export specific table
export_table_01(summary_stats, format = "csv")
export_table_03(regression_results, format = "tex")

CONTACT
-------
For table questions, contact: myles.owens@stanford.edu
================================================================================
