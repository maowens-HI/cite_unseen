================================================================================
NBER WORKING PAPERS RAW DATA
================================================================================

PURPOSE
-------
This folder contains raw NBER working paper files (text or PDF format) from
which bibliographic citations will be extracted and analyzed.

EXPECTED FILES
--------------
- w#####.txt   - Plain text version of working paper
- w#####.pdf   - PDF version of working paper (if text unavailable)

Example: w31280.txt, w31281.txt, w31500.pdf

PAPER ID STRUCTURE
------------------
NBER working papers use sequential IDs:
  - w0001 - w10000: Older papers (pre-2000s, variable quality)
  - w10001 - w25000: 2000s-2010s papers
  - w25001 - w33000+: 2018-present (most relevant for LLM study)

KEY DATE FOR ANALYSIS
---------------------
  Treatment Date: November 30, 2022 (ChatGPT 3.0 release)

  Pre-treatment: Papers w##### with publication date < 2022-11-30
  Post-treatment: Papers w##### with publication date >= 2022-11-30

DATA ACQUISITION
----------------
Papers can be downloaded from: https://www.nber.org/papers

For batch downloading, see: /code/nber_scraper.R
- Uses polite scraping with delays
- Includes user-agent identification
- Respects rate limits

FILE PROCESSING
---------------
PDF files will be converted to text using pdftools::pdf_text()
Text extraction focuses on:
  1. Bibliography/References section
  2. Works Cited section
  3. Footnotes with citations

NOTES
-----
- Do NOT commit paper files to git (copyright considerations)
- Store paper metadata separately if needed
- Expected storage: ~50MB per 1000 papers (text only)
- For full PDF corpus: ~5GB per 1000 papers

SCRAPING ETHICS
---------------
- Always use polite User-Agent headers
- Include 1-3 second delays between requests
- Respect robots.txt and rate limits
- Contact NBER if doing large-scale downloads

CONTACT
-------
For data access questions, contact: myles.owens@stanford.edu
================================================================================
