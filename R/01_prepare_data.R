# =============================================================================
# 01_prepare_data.R
# Load the LIWC data and the AspectEmo scores, harmonise variable names /
# coding and save analysis-ready objects to data/processed/.
#
# The AspectEmo dataset is built by joining the scores produced by
# python/02_aggregate_aspectemo.py with the design and participant-level
# variables of the LIWC file, using `Comment ID` as the key.
#
# Outputs: data/processed/liwc.rds, data/processed/aspectemo.rds
# (SADCAT data are prepared separately in 02_sadcat_coding.R.)
# =============================================================================

source(here::here("R", "00_setup.R"))

# ---- LIWC (Tone) + questionnaire measures -----------------------------------
# readr::read_csv() handles UTF-8 with and without BOM. Column names are
# trimmed because the export contains a trailing space in one of them.
liwc_raw <- readr::read_csv(path_raw(FILE_LIWC),
                            show_col_types = FALSE,
                            name_repair = trimws)

liwc <- liwc_raw %>%
  rename(
    participant_id          = `Participant Private ID`,
    task_name               = `Task Name`,
    target_gender_code      = `Target gender (0 = woman, 1 = man)`,
    stereotype_code         = `Stereotype 0 = female, 1 = male`,
    behaviour_code          = `Behaviour (0 = negative, 1 = positive)`,
    congruence_code         = `Stereotype congruence (0 = counter-stereotypical, 1 = stereotypical)`,
    participant_gender_code = `Participant gender_code`
  ) %>%
  mutate(
    Tone          = as.numeric(Tone),   # read_csv sometimes guesses character
    target_gender = if_else(target_gender_code == 1, "man", "woman"),
    stereotype    = if_else(stereotype_code == 1, "masculine", "feminine"),
    behaviour     = if_else(behaviour_code == 1, "positive", "negative")
  ) %>%
  add_design_factors() %>%
  mutate(
    participant_id     = factor(participant_id),
    participant_gender = code_participant_gender(participant_gender_code),
    # Participant-level predictors, grand-mean centred (over all rows)
    across(c(Sexism, BS_K, HS_K, BS_M, HS_M, Worldview),
           center_grand_mean, .names = "{.col}_c")
  )

# Sanity check: congruence derived from the design must match the exported code
congruence_from_code <- if_else(liwc$congruence_code == 1,
                                "stereotypical", "counter-stereotypical")
if (any(congruence_from_code != as.character(liwc$congruence), na.rm = TRUE)) {
  warning("Derived congruence differs from the exported congruence code in ",
          sum(congruence_from_code != as.character(liwc$congruence), na.rm = TRUE),
          " row(s) of the LIWC file.")
}

cat("LIWC: rows =", nrow(liwc),
    "| participants =", nlevels(liwc$participant_id),
    "| missing Tone =", sum(is.na(liwc$Tone)), "\n")
cat("Participant gender:\n")
print(table(liwc$participant_gender, useNA = "always"))

saveRDS(liwc, path_processed("liwc.rds"))

# ---- AspectEmo (score_all_tokens, score_only_charged) -----------------------
# One row per response in the LIWC file (the full set of responses), with
# AspectEmo scores joined by `Comment ID`. Responses for which CLARIN returned
# no result get NA scores (and count as missing in 03_missing_data.R).
stopifnot("`Comment ID` is missing from the LIWC file" = "Comment ID" %in% names(liwc),
          !anyDuplicated(liwc$`Comment ID`))

aspectemo_scores <- readr::read_csv(path_processed("aspectemo_scores.csv"),
                                    show_col_types = FALSE)

aspectemo <- liwc %>%
  select(comment_id = `Comment ID`, participant_id, task_name,
         target_gender, stereotype, behaviour, congruence, story_type,
         participant_gender_code, participant_gender,
         Sexism, BS_K, HS_K, BS_M, HS_M, Worldview, ends_with("_c")) %>%
  left_join(
    aspectemo_scores %>%
      select(comment_id, task_name_scores = task_name,
             score_all_tokens, score_only_charged,
             total_tokens, n_labelled_aspects),
    by = "comment_id"
  )

# Sanity checks: every score row found its response, and conditions agree
unmatched <- setdiff(aspectemo_scores$comment_id, aspectemo$comment_id)
if (length(unmatched) > 0) {
  warning(length(unmatched), " AspectEmo result(s) have no matching Comment ID in the LIWC file: ",
          paste(head(unmatched, 10), collapse = ", "))
}
task_mismatch <- !is.na(aspectemo$task_name_scores) &
  aspectemo$task_name_scores != aspectemo$task_name
if (any(task_mismatch)) {
  warning(sum(task_mismatch), " response(s) have a different Task Name in the LIWC file and in the AspectEmo scores.")
}
aspectemo <- aspectemo %>% select(-task_name_scores)

cat("AspectEmo: rows =", nrow(aspectemo),
    "| with scores =", sum(!is.na(aspectemo$score_all_tokens)),
    "| without CLARIN result =", sum(is.na(aspectemo$total_tokens)),
    "| missing score_only_charged =", sum(is.na(aspectemo$score_only_charged)), "\n")

saveRDS(aspectemo, path_processed("aspectemo.rds"))
