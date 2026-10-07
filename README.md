# Gender stereotypes in descriptions of behaviour: analysis code for my master's thesis

R code for the statistical analysis of my master's thesis 
**What will people say? Differences in descriptions of women and men behaving counter-stereotypical to their gender. **
(University of Warsaw, Psychology, Social Research Specialization, 2026).
This project investigates how women and men who behave in stereotypical or counter-stereotypical ways are described 
in participants' own words, using three text-analysis tools (LIWC, AspectEmo, SADCAT) to measure tone, emotional charge, warmth and competence. 
Descriptions differed mainly by the type of behaviour portrayed rather than by the character's gender or by stereotype congruence, 
and participants' sexism did not predict how counter-stereotypical characters were described.

## Study in brief

An online vignette experiment (**277 participants, 1,107 analysed texts**; one off-topic response excluded; AspectEmo returned results for 1,104 of them).
Each participant read four short stories, one per behaviour type (positive masculine, positive feminine,
negative masculine, negative feminine), and described the main character in their own words (1–85 words, M = 12.8).

Design: 2 (**target gender**: woman vs. man; between participants) × 2 (**stereotype congruence**:
behaviour consistent vs. inconsistent with the stereotype of the character's gender; within participants) ×
2 (**behaviour valence**: positive vs. negative; within participants).

The free-text responses were analysed with three automated text-analysis tools:

| Tool | Outcome(s) | Notes |
|------|-----------|-------|
| [LIWC-22](https://www.liwc.app/) | `Tone` (0–100) | dictionary-based; run on English translations |
| [AspectEmo](https://github.com/CLARIN-PL/aspectemo) via CLARIN (Polish) | `score_all_tokens`, `score_only_charged` | machine-learning model; run on the original Polish text |
| [SADCAT](https://github.com/gandalfnicolas/SADCAT) (Nicolas, Bai & Fiske, 2021) | `warmth`, `competence` (−1 to 1) | Stereotype Content Model dictionaries; run on English translations |

Responses were translated with DeepL and manually corrected; typos in the original responses were fixed
beforehand. Dictionary coverage: 85% (LIWC), 82% (SADCAT warmth), 73% (SADCAT competence).

Participants also completed short ambivalent-sexism inventories towards women and men (BS_K, HS_K, BS_M, HS_M
subscales + overall score), political-views items and demographics.

## Methods

- Linear mixed-effects models (`lme4`, `lmerTest`) with a random intercept for participant, since each
  participant wrote several descriptions
- Likelihood-ratio tests for interactions and model comparison; marginal R² (Nakagawa & Schielzeth, 2013; `MuMIn`)
- Estimated marginal means, contrasts and effect sizes (`emmeans`, `effectsize`)
- Welch t-tests (H2, H3; linear-model assumptions of normality and equal variances were violated) with BCa bootstrap
  validation for skewed distributions, and a participant-level cluster bootstrap for H1 (AspectEmo) (`boot`)
- Paired t-tests for warmth vs. competence within responses (H5)
- Residual diagnostics, linearity checks and a missing-data analysis (mixed-effects logistic model)

## Repository structure

```
python/                        text preparation and exploratory lexical analysis
├── common.py                  shared paths, corpus loading, task-name decoding
├── 01_make_clarin_input.py    corpus -> ZIP of .txt files for CLARIN AspectEmo
├── 02_aggregate_aspectemo.py  CLARIN output -> one row per response (scores)
├── 03_word_counts.py          response length, share of very short responses
└── 04_lexical_exploration.py  most frequent adjectives / verbs / nouns per condition
R/
├── 00_setup.R                 packages, paths, design coding, palette, helper functions
├── 01_prepare_data.R          load LIWC + AspectEmo data, harmonise coding
├── 02_sadcat_coding.R         SADCAT dictionary coding -> warmth and competence
├── 03_missing_data.R          missingness by condition, worldview, participant gender
├── 04_hypotheses_liwc_tone.R  H1, H2, H3, H7, H8 (LIWC Tone)
├── 05_hypotheses_aspectemo.R  H1, H2, H3, H7, H8 + exploratory analyses (AspectEmo)
├── 06_hypotheses_sadcat.R     H5, H6 + exploratory analyses (SADCAT)
├── 07_predictor_comparison.R  story type vs target gender vs congruence (R²m), all outcomes
└── 08_figures.R               bar charts and boxplots
output/
├── figures/                   generated plots
└── tables/                    generated summary tables
```

## Reproducing the analysis

The pipeline has two parts. Run the Python part first (it produces the AspectEmo scores), then the R part.

**Python** (3.10+)
```bash
pip install -r requirements.txt
python -m spacy download pl_core_news_lg        # only for 04_lexical_exploration.py
python python/01_make_clarin_input.py           # -> data/processed/clarin_input.zip
# upload the ZIP to AspectEmo on the CLARIN platform and download the results (ZIP)
python python/02_aggregate_aspectemo.py --results WYNIKI.zip
python python/03_word_counts.py
python python/04_lexical_exploration.py         # add --form lemma for Morfeusz lemmas
```

**R**
1. Open the folder as an R project (the `.here` file marks the project root).
2. Install the required packages:
   ```r
   install.packages(c("here", "tidyverse", "lme4", "lmerTest", "emmeans", "MuMIn",
                      "car", "boot", "effectsize", "readODS", "lmtest", "devtools"))
   devtools::install_github("gandalfnicolas/SADCAT")
   ```
3. Place the data files in `data/raw/` (see below).
4. Run the scripts in numerical order, e.g. `source("R/01_prepare_data.R")`, or from a terminal:
   `Rscript R/04_hypotheses_liwc_tone.R > output/04_results.txt`.
   Scripts 03–07 print their results to the console.

## Data availability

The raw data contain participants' free-text responses and are **not** included in this repository.
Expected files in `data/raw/`:

- `Korpus_magisterka.ods` – Polish corpus, one row per response (input for the Python scripts)
- `Korpus magisterka (eng).ods` – English translation of the corpus (input for SADCAT)
- `Bazaodp-Data.csv` – LIWC output + questionnaire data
- `data/processed/aspectemo_scores.csv` is produced by `python/02_aggregate_aspectemo.py`;
  `R/01_prepare_data.R` joins it to the questionnaire and design variables of `Bazaodp-Data.csv`
  by `Comment ID` (no manual merging needed)

## Author

Marianna Wesołowska
https://www.linkedin.com/in/marianna-wesołowska
marianna.z.wesolowska@gmail.com