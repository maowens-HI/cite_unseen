# Quick Guide to the `src/` Folder

A simple explanation of each file for research assistants.

---

## The Big Picture

This project checks if academic papers contain **fake citations** (references to papers that don't exist). Think of it like a spell-checker, but for bibliographies.

**The pipeline flows like this:**
```
Load Data → Clean Text → Extract Citations → Compare Against Database → Analyze Results
```

---

## Pipeline Files (Run in Order)

### `00_config.R` - The Settings File
**What it does:** Stores all the project settings in one place.

**Analogy:** Like the settings app on your phone - controls how everything else behaves.

**Key settings:**
- `treatment_date`: November 30, 2022 (when ChatGPT launched)
- `hallucination_threshold`: 0.30 (how different a citation can be before we suspect it's fake)
- File paths for where data lives

---

### `01_data_ingestion.R` - The Data Loader
**What it does:** Reads raw data files from your computer into R.

**Analogy:** Like opening files from your Downloads folder into Excel.

**Key functions:**
- `load_wos_corpus()` - Loads the Web of Science database (the "real" citations)
- `load_nber_papers()` - Loads the NBER papers we're checking

---

### `02_text_normalization.R` - The Text Cleaner
**What it does:** Simplifies citation text so we can compare them fairly.

**Analogy:** Like removing formatting from text before comparing two documents.

**Example:**
```
Before: "Smith, J. (2020). Machine Learning. Journal, 34(2)."
After:  "smithj2020machinelearningjournal342"
```

**Key function:** `normalize_citation()` - strips punctuation, spaces, and capitalization

---

### `03_citation_extraction.R` - The Citation Finder
**What it does:** Finds the bibliography section in each paper and pulls out individual citations.

**Analogy:** Like highlighting just the "References" section at the end of a paper.

**Key function:** `extract_citations()` - finds and parses the bibliography

---

### `04_fuzzy_matching.R` - The Comparison Engine
**What it does:** Compares each NBER citation against the entire Web of Science database to find the closest match.

**Analogy:** Like autocorrect suggesting the word you probably meant to type.

**Key concept - Match Distance:**
- `0.00` = Exact match (citation is definitely real)
- `0.10` = Very similar (probably real, minor typos)
- `0.30+` = Very different (possibly fake/hallucinated)

**Key function:** `fuzzy_match()` - compares citations and returns distances

---

### `05_score_analysis.R` - The Results Summarizer
**What it does:** Creates statistics and charts showing how many potential fake citations were found.

**Analogy:** Like generating a report card with grades and graphs.

**Key outputs:**
- Average match scores
- Count of potential hallucinations
- Charts comparing before vs. after ChatGPT

---

### `06_causal_estimation.R` - The Statistical Test
**What it does:** Runs statistical tests to determine if fake citations actually increased after ChatGPT launched.

**Analogy:** Like a clinical trial determining if a medicine works - controls for other factors.

**Key functions:**
- `run_did()` - Difference-in-differences test
- `run_rdd()` - Regression discontinuity test

---

## Support Files

### `utils.R` - Helper Tools
Common utilities used throughout: reading files safely, cleaning text, extracting DOIs.

### `data_models.R` - Data Templates
Defines what columns each dataset should have. Like a blank spreadsheet with headers.

### `wos_parser.R` - Web of Science Reader
Parses the specific format of Web of Science export files.

### `nber_parser.R` - NBER Paper Reader
Extracts citations from NBER paper text files.

---

## Quick Reference: File Dependencies

```
00_config.R          ← Everything depends on this
    ↓
data_models.R        ← Defines data structures
utils.R              ← Helper functions
wos_parser.R         ← Parses WOS files
nber_parser.R        ← Parses NBER files
    ↓
01_data_ingestion.R  ← Uses all the above
    ↓
02_text_normalization.R
    ↓
03_citation_extraction.R
    ↓
04_fuzzy_matching.R
    ↓
05_score_analysis.R
    ↓
06_causal_estimation.R
```

---

## Glossary

| Term | Simple Definition |
|------|-------------------|
| **Hallucination** | A fake citation that sounds real but doesn't exist |
| **Fuzzy matching** | Comparing text that's similar but not identical |
| **Levenshtein distance** | Count of edits needed to turn one text into another |
| **Normalization** | Simplifying text to make comparisons fair |
| **DiD** | Difference-in-differences - a statistical method |
| **RDD** | Regression discontinuity - another statistical method |
| **WOS** | Web of Science - database of real academic papers |
| **NBER** | National Bureau of Economic Research - economics papers |

---

*Questions? Contact: myles.owens@stanford.edu*
