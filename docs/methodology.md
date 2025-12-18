# Methodology: Detecting LLM-Hallucinated References

## Research Design Overview

This study employs a causal identification strategy to detect whether Large Language Models (LLMs) have increased the prevalence of unverifiable citations in academic working papers. We exploit the public release of ChatGPT 3.0 on November 30, 2022 as an exogenous treatment affecting the potential for LLM-assisted academic writing.

## Theoretical Framework

### The Hallucination Problem

LLMs are known to "hallucinate"—generate plausible-sounding but factually incorrect information. In academic writing, this manifests as:

1. **Citation hallucinations**: References to non-existent papers
2. **Attribution errors**: Misattributed quotes or findings
3. **Bibliographic mutations**: Real papers with incorrect details

We focus on citation hallucinations, which are detectable through comparison with verified bibliographic databases.

### Identification Strategy

The key identifying assumption is that ChatGPT 3.0's release date represents a sharp discontinuity in:
- Accessibility of LLM writing tools to academics
- Potential for LLM-assisted reference generation

**Pre-treatment period**: Papers submitted before November 30, 2022 are assumed to have minimal LLM influence.

**Post-treatment period**: Papers submitted after this date may have been assisted by LLMs.

## Data Sources

### Web of Science Corpus

The Web of Science (WOS) database serves as our ground truth for verified academic citations. Key properties:

- Comprehensive coverage of peer-reviewed publications
- Standardized citation formatting
- DOI verification where available
- Regular updates with new publications

**Limitations**:
- Coverage gaps in certain disciplines
- Lag between publication and indexing
- Regional and language biases

### NBER Working Papers

National Bureau of Economic Research working papers provide our treatment sample because:

- Consistent formatting over time
- Public availability of full text
- Clear submission date tracking
- High research quality standards
- Potential for LLM assistance given technical nature

## Text Processing Pipeline

### Citation Normalization

To enable fuzzy matching, we normalize all citation strings:

```
Original: "Smith, J. (2020). Machine Learning in Economics.
           Journal of Economic Perspectives, 34(2), 87-108."

Step 1 - Lowercase:
"smith, j. (2020). machine learning in economics.
 journal of economic perspectives, 34(2), 87-108."

Step 2 - Remove non-alphabetic:
"smithjmachinelearningineconomicsjournalofeconomicperspectives"
```

This normalization:
- Eliminates formatting variations
- Removes publication details that vary across citation styles
- Preserves core identifying information (authors, title words, journal name)

### Fuzzy Matching Algorithm

We use **Normalized Levenshtein Distance** to compare citations:

```
normalized_distance = raw_levenshtein_distance / max(len(a), len(b))
```

**Properties**:
- Range: [0, 1] where 0 = identical, 1 = completely different
- Symmetric: distance(a, b) = distance(b, a)
- Captures insertions, deletions, and substitutions

**Why Levenshtein?**
- Handles typos and minor variations
- Robust to word order differences (in normalized form)
- Computationally tractable for large corpora

### Threshold Selection

We classify citations based on match score thresholds:

| Score Range | Classification | Interpretation |
|-------------|----------------|----------------|
| 0.00 | Exact match | Identical to WOS entry |
| 0.00-0.05 | High confidence | Very likely same reference |
| 0.05-0.15 | Medium confidence | Probably same reference |
| 0.15-0.30 | Low confidence | Possibly same reference |
| >0.30 | No match | Likely hallucination candidate |

**Threshold Calibration**:
- Cross-validation with known citation pairs
- Sensitivity analysis across threshold values
- Manual spot-checking of boundary cases

## Estimation Strategy

### Difference-in-Differences (DiD)

Basic specification:

```
Y_it = α + β * Post_t + γ * X_it + ε_it
```

Where:
- Y_it = Match score (or hallucination indicator) for citation i in paper t
- Post_t = 1 if paper published after November 30, 2022
- X_it = Control variables (paper length, citation count, topic)
- β = Treatment effect (main coefficient of interest)

### Regression Discontinuity Design (RDD)

For papers near the treatment date:

```
Y_it = α + β * Post_t + f(Days_t) + β_2 * Post_t × f(Days_t) + ε_it
```

Where:
- Days_t = Days from treatment date
- f(·) = Polynomial function of running variable
- Bandwidth selected via optimal procedures (e.g., IK bandwidth)

### Robustness Checks

1. **Placebo tests**: Fake treatment dates before actual release
2. **Bandwidth sensitivity**: Varying RDD bandwidth
3. **Threshold sensitivity**: Varying hallucination threshold
4. **Donut RDD**: Excluding observations very close to cutoff
5. **Alternative distance metrics**: Jaro-Winkler, cosine similarity

## Potential Confounds and Limitations

### Threats to Validity

1. **Anticipation effects**: Academics may have had early access to LLMs
2. **Secular trends**: Citation practices may evolve independently
3. **Composition effects**: Different authors/topics post-treatment
4. **WOS coverage changes**: Database updates over time

### Measurement Issues

1. **False positives**: Real citations appearing as hallucinations due to:
   - WOS coverage gaps
   - Very new publications
   - Non-English sources
   - Books and working papers

2. **False negatives**: Hallucinations appearing legitimate due to:
   - Coincidental similarity to real papers
   - Common name patterns

### Mitigation Strategies

- Control for publication year of cited work
- Field-specific analysis
- Author fixed effects where possible
- Validation against alternative databases (Scopus, Google Scholar)

## Expected Results Format

### Primary Outputs

1. **Citation-level dataset**:
   - Paper ID, citation number
   - Original and normalized citation
   - Best WOS match and score
   - Hallucination classification

2. **Paper-level aggregates**:
   - Total citations
   - Hallucination count and rate
   - Mean/median match scores

3. **Regression results**:
   - Treatment effects with standard errors
   - Confidence intervals
   - Robustness check tables

### Visualizations

1. Score distribution histograms (pre/post)
2. Time series of average scores
3. RDD plot around treatment date
4. Sensitivity analysis curves

## Ethical Considerations

- No individual author identification
- Aggregate reporting only
- Acknowledgment of false positive risks
- Recommendations for verification, not accusation

## References

[To be populated with relevant methodology papers on:
- LLM hallucination detection
- Citation analysis methods
- Difference-in-differences estimation
- Regression discontinuity design
- Text similarity measures]
