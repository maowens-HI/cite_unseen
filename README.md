# Cite Unseen

Detect LLM-hallucinated references in NBER working papers using the ChatGPT release (Nov 30, 2022) as a natural experiment.

## Quick Start

```bash
# 1. Install dependencies
Rscript -e "install.packages(c('tidyverse', 'stringdist'))"

# 2. Add your data
#    - WOS references: data/wos/*.txt (tab-separated: WOS_ID <tab> reference)
#    - NBER papers: data/nber/w#####.txt

# 3. Run analysis
Rscript cite_unseen.R
```

## Project Structure

```
cite_unseen/
├── cite_unseen.R      # Main script (all functionality)
├── data/
│   ├── wos/           # Web of Science reference files
│   └── nber/          # NBER paper text files (w#####.txt)
└── output/            # Results (match_results.csv, plots)
```

## How It Works

1. **Load WOS corpus** - Ground truth of verified academic citations
2. **Parse NBER papers** - Extract bibliography citations
3. **Normalize text** - Lowercase, alphanumeric only for comparison
4. **Fuzzy match** - Find best WOS match using Levenshtein distance
5. **Classify** - Citations with distance > 0.30 flagged as potential hallucinations

## Configuration

Edit `CONFIG` in `cite_unseen.R`:

```r
CONFIG <- list(
  treatment_date = as.Date("2022-11-30"),  # ChatGPT release
  hallucination_threshold = 0.30,           # Distance threshold
  seed = 42
)
```

## Output

- `output/match_results.csv` - All citations with match scores
- `output/score_distribution.png` - Histogram of match distances

## Data Format

**WOS files** (`data/wos/*.txt`):
```
WOS:000060312800003	Author 1997 147 55 Title JOURNAL NAME
```

**NBER files** (`data/nber/w#####.txt`):
Text files containing paper content with a "References" section.

## Dependencies

- R >= 4.0
- tidyverse
- stringdist
