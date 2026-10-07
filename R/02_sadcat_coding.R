# =============================================================================
# 02_sadcat_coding.R
# Code the (English-translated) responses with SADCAT dictionaries
# (Nicolas, Bai & Fiske, 2021) and derive the two outcome variables:
#   warmth     = mean(Sociability, Morality)
#   competence = mean(Ability, Assertiveness)
#
# Input:  data/raw/Korpus magisterka (eng).ods
# Output: data/processed/sadcat.rds, coverage tables in output/tables/
# =============================================================================

source(here::here("R", "00_setup.R"))

# SADCAT is not on CRAN:
#   install.packages("devtools")
#   devtools::install_github("gandalfnicolas/SADCAT")
# On first use, the `textdata` package asks to download the NRC lexicon;
# run once interactively and confirm the download.
if (!requireNamespace("SADCAT", quietly = TRUE)) {
  stop("Package 'SADCAT' is not installed. See the comment at the top of this script.",
       call. = FALSE)
}
library(SADCAT)

# preprocess_text() and match_dictionaries() are only available in recent
# versions of SADCAT (install the current GitHub version; the older 0.1.0
# release documented on rdrr.io does not have them).
cat("SADCAT version:", as.character(packageVersion("SADCAT")), "\n")

# ---- Load corpus and decode the design from the task name -------------------
# Task names follow the pattern "<gender>_<stereotype>_<valence>", e.g. "K_SK_poz":
#   position 1:   K = woman (kobieta),  M = man (mezczyzna)
#   positions 3-4: SK = feminine stereotype, SM = masculine stereotype
#   positions 6-8: poz = positive, neg = negative
corpus <- readODS::read_ods(path_raw(FILE_SADCAT_CORPUS))
names(corpus) <- make.names(names(corpus))

gender_code     <- substr(corpus$Task.Name, 1, 1)
stereotype_code <- substr(corpus$Task.Name, 3, 4)
valence_code    <- substr(corpus$Task.Name, 6, 8)

stopifnot(all(gender_code     %in% c("K", "M")),
          all(stereotype_code %in% c("SK", "SM")),
          all(valence_code    %in% c("poz", "neg")))

corpus <- corpus %>%
  mutate(
    task_name      = Task.Name,
    participant_id = factor(Participant.Private.ID),
    target_gender  = if_else(gender_code == "K", "woman", "man"),
    stereotype     = if_else(stereotype_code == "SK", "feminine", "masculine"),
    behaviour      = if_else(valence_code == "poz", "positive", "negative")
  ) %>%
  add_design_factors()

# ---- Step 1: text preprocessing ---------------------------------------------
# preprocess_text() adds tv (lower-cased text), tv2 (spell-checked) and tv3
# (singularised). Spell-checking is switched off: it requires Java + WordNet,
# and typos in the translated responses were corrected by hand beforehand.
#
# NOTE: as in the thesis analysis, the lower-cased text `tv` is passed on to
# the dictionary matching. The package default for match_dictionaries() is the
# singularised column `tv3`. match_dictionaries() itself drops word-final "s",
# so the two choices will usually give very similar results; using "tv3"
# would be a change to the analysis, not a bug fix.
cat("Step 1: preprocessing...\n")
prepared <- preprocess_text(data = corpus, text_col = "Response.eng", spellcheck = FALSE)
corpus$text_clean <- prepared$tv

# ---- Step 2: dictionary matching --------------------------------------------
# match_dictionaries() can also compute valence-based variables (*_Valy,
# *_ValyNoNA, *_Valence), but only if the data contain the columns named by
# `valence_col` / `valence_nona_col` (defaults "ValenceYesNA" / "ValenceNoNA").
# These columns were not created for the thesis, so those variables are
# skipped. Warmth and competence are derived from the direction scores
# (*_direction). In SADCAT, *_direction is a copy of *_dirx3 =
# (high-pole match) - (low-pole match), each coded 0/1, so every score is
# -1, 0 or 1, and NA means that no word of the dimension was found. Valence
# plays no role in this computation.
cat("Step 2: dictionary matching...\n")
corpus <- match_dictionaries(corpus, text_col = "text_clean")

cat("SADCAT direction columns found:\n")
print(names(corpus)[grepl("_direction", names(corpus))])

# ---- Warmth and competence --------------------------------------------------
# Texts matching neither sub-dimension get NaN from rowMeans(); converted to NA.
sadcat <- corpus %>%
  mutate(
    warmth     = rowMeans(cbind(Sociability_direction, Morality_direction),   na.rm = TRUE),
    competence = rowMeans(cbind(Ability_direction, Assertiveness_direction), na.rm = TRUE),
    warmth     = if_else(is.nan(warmth),     NA_real_, warmth),
    competence = if_else(is.nan(competence), NA_real_, competence),
    warmth_minus_comp = warmth - competence
  )

# ---- Dictionary coverage per condition --------------------------------------
# Coverage = share of texts with a non-missing score (0 means ambivalent /
# neutral coverage and counts as covered).
coverage <- sadcat %>%
  group_by(behaviour, stereotype, target_gender) %>%
  summarise(
    n_total        = n(),
    pct_sociability   = round(100 * mean(!is.na(Sociability_direction)),    1),
    pct_morality      = round(100 * mean(!is.na(Morality_direction)),       1),
    pct_warmth        = round(100 * mean(!is.na(warmth)),                   1),
    pct_ability       = round(100 * mean(!is.na(Ability_direction)),        1),
    pct_assertiveness = round(100 * mean(!is.na(Assertiveness_direction)),  1),
    pct_competence    = round(100 * mean(!is.na(competence)),               1),
    .groups = "drop"
  )

cat("\n=== SADCAT coverage per condition ===\n")
print(coverage, n = Inf)
write_csv(coverage, path_tables("sadcat_coverage_by_condition.csv"))

cat(sprintf("\nOverall coverage: warmth %d/%d (%.1f%%), competence %d/%d (%.1f%%)\n",
            sum(!is.na(sadcat$warmth)),     nrow(sadcat), 100 * mean(!is.na(sadcat$warmth)),
            sum(!is.na(sadcat$competence)), nrow(sadcat), 100 * mean(!is.na(sadcat$competence))))

saveRDS(sadcat, path_processed("sadcat.rds"))
