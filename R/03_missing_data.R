# =============================================================================
# 03_missing_data.R
# Missing-data analysis for LIWC Tone and AspectEmo score_only_charged.
#
# Missingness is broken down by
#   1) story type (8 conditions, `task_name`)
#   2) participant worldview (1-5)
#   3) participant gender (woman / man; "other" is excluded from tests and the
#      model because of its very small n)
# For each: descriptive table (n, n missing, % missing) + chi-square test.
# Finally, a mixed-effects logistic model tests which of the three factors
# predicts missingness while controlling for the other two.
#
# Note: the chi-square tests treat observations as independent although each
# participant contributes several; the final glmer accounts for this.
# =============================================================================

source(here::here("R", "00_setup.R"))

liwc      <- readRDS(path_processed("liwc.rds"))
aspectemo <- readRDS(path_processed("aspectemo.rds"))

missing_by <- function(data, var) {
  data %>%
    group_by(across(all_of(var))) %>%
    summarise(n = n(), n_NA = sum(missing),
              pct_NA = round(100 * n_NA / n, 1), .groups = "drop")
}

analyse_missingness <- function(dv, data, label) {
  data <- data %>%
    mutate(task_name = factor(task_name),
           missing   = is.na(.data[[dv]]))

  cat("\n\n", strrep("=", 70), "\n", sep = "")
  cat("MISSING DATA:", label, "\n")
  cat(strrep("=", 70), "\n")
  cat(sprintf("Overall: %d / %d (%.1f%%) missing\n",
              sum(data$missing), nrow(data), 100 * mean(data$missing)))

  # 1) by story type
  cat("\n--- by story type (8 conditions) ---\n")
  tab_story <- missing_by(data, "task_name") %>% arrange(desc(pct_NA))
  print(tab_story)
  cat("\nChi-square (missing x story type):\n")
  print(chisq.test(table(data$missing, data$task_name)))

  # 2) by worldview
  cat("\n--- by participant worldview (1-5) ---\n")
  print(missing_by(data, "Worldview"))
  cat("\nChi-square (missing x worldview):\n")
  print(chisq.test(table(data$missing, data$Worldview)))

  # 3) by participant gender
  cat("\n--- by participant gender ---\n")
  print(missing_by(data, "participant_gender"))
  data_gender <- data %>%
    filter(participant_gender != "other") %>%
    mutate(participant_gender = droplevels(participant_gender))
  cat("\nChi-square (missing x participant gender, without 'other'):\n")
  print(chisq.test(table(data_gender$missing, data_gender$participant_gender)))

  # Joint model
  cat("\n--- Joint logistic mixed model: story type + worldview + participant gender ---\n")
  m <- glmer(
    missing ~ task_name + Worldview_c + participant_gender + (1 | participant_id),
    data    = data_gender,
    family  = binomial,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
  print(summary(m))

  invisible(list(tab_story = tab_story, model = m))
}

res_liwc <- analyse_missingness("Tone",               liwc,      "LIWC Tone")
res_ae   <- analyse_missingness("score_only_charged", aspectemo, "AspectEmo score_only_charged")

# ---- Side-by-side summary: % missing per story type -------------------------
missing_by_story <- full_join(
  res_liwc$tab_story %>% select(task_name, n_LIWC = n, pct_NA_LIWC = pct_NA),
  res_ae$tab_story   %>% select(task_name, n_AspectEmo = n, pct_NA_AspectEmo = pct_NA),
  by = "task_name"
) %>% arrange(task_name)

cat("\n\n", strrep("=", 70), "\n", sep = "")
cat("SUMMARY: % missing per story type, LIWC vs AspectEmo\n")
cat(strrep("=", 70), "\n")
print(missing_by_story)
write_csv(missing_by_story, path_tables("missing_by_story_type.csv"))
