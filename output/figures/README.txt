================================================================================
FIGURES
================================================================================

PURPOSE
-------
Publication-quality figures and diagnostic plots.

EXPECTED OUTPUTS
----------------
fig_01_match_score_distribution.png
  - Histogram/density of fuzzy match scores
  - Pre vs post treatment comparison

fig_02_time_series_scores.png
  - Average match scores over time
  - Vertical line at treatment date

fig_03_did_visualization.png
  - Difference-in-differences visual
  - Parallel trends assumption check

fig_04_score_by_paper_characteristics.png
  - Match scores by author count, paper length, etc.

fig_05_sensitivity_thresholds.png
  - How results change with different thresholds

FIGURE SPECIFICATIONS
---------------------
For publication:
  - Format: PDF (vector) or PNG (300+ DPI)
  - Dimensions: Width 6-8 inches, Height 4-6 inches
  - Font: 10-12pt, sans-serif preferred
  - Colors: Colorblind-friendly palette

For presentations:
  - Format: PNG (150+ DPI)
  - Dimensions: 16:9 aspect ratio
  - Font: 14-18pt minimum
  - High contrast colors

REGENERATION
------------
# Generate all figures
source("src/05_score_analysis.R")

# Generate specific figure
create_figure_01(match_results, output_path = "output/figures/")

CONTACT
-------
For figure questions, contact: myles.owens@stanford.edu
================================================================================
