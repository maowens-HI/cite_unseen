================================================================================
REPORTS
================================================================================

PURPOSE
-------
Analysis summaries, validation reports, and documentation of findings.

EXPECTED OUTPUTS
----------------
report_preliminary_results.md
  - Initial findings summary
  - Key statistics and visualizations
  - Next steps and open questions

report_validation.md
  - Manual spot-check results
  - False positive/negative analysis
  - Threshold calibration findings

report_sensitivity_analysis.md
  - How results change with parameters
  - Robustness to specification choices
  - Confidence in main findings

report_final.html
  - Complete analysis narrative
  - All figures and tables embedded
  - Methodology discussion

REPORT TEMPLATES
----------------
Use R Markdown for reproducible reports:

  ---
  title: "Cite Unseen: Preliminary Results"
  author: "Myles Owens"
  date: "`r Sys.Date()`"
  output: html_document
  ---

  ```{r setup, include=FALSE}
  source("src/00_config.R")
  ```

VERSIONING
----------
Include dates in report filenames:
  report_20241201_preliminary.md
  report_20241215_final.md

Or use git tags for major versions:
  git tag -a v1.0-report -m "First draft report"

CONTACT
-------
For report questions, contact: myles.owens@stanford.edu
================================================================================
