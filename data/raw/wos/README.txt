================================================================================
WEB OF SCIENCE (WOS) RAW DATA
================================================================================

PURPOSE
-------
This folder contains raw CSV exports from Web of Science, serving as the
ground truth corpus of verified academic citations.

EXPECTED FILES
--------------
- WOS2020.csv, WOS2021.csv, WOS2022.csv, WOS2023.csv, etc.
- Each file contains citations from that publication year

REQUIRED COLUMNS
----------------
At minimum, WOS exports should contain:
  - reference:    Full citation string
  - doi:          Digital Object Identifier (if available)
  - pub_year:     Publication year
  - authors:      Author names
  - title:        Article/book title
  - source:       Journal/publisher name

DATA ACQUISITION
----------------
1. Access Web of Science at webofscience.com
2. Search for relevant records (e.g., economics journals)
3. Export as CSV with "Full Record" option
4. Place files in this directory with consistent naming

FILE NAMING CONVENTION
----------------------
  WOS{YEAR}.csv         - Single year export
  WOS{START}-{END}.csv  - Multi-year export range
  WOS_economics.csv     - Subject-specific export

NOTES
-----
- Do NOT commit large data files to git (see .gitignore)
- Store original exports unchanged; processing happens in /data/processed/
- Document any export filters or constraints in this README
- Expected file size: 100MB - 1GB per year depending on scope

CONTACT
-------
For data access questions, contact: myles.owens@stanford.edu
================================================================================
