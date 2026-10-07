# =============================================================================
# 07_predictor_comparison.R
# Which design factor explains most variance in each text-based outcome?
#
# Outcomes: LIWC Tone, AspectEmo score_only_charged, SADCAT warmth and
# competence. Predictors compared:
#   1) story_type    -- 4 categories: feminine/masculine x positive/negative
#   2) target_gender -- man / woman
#   3) congruence    -- stereotypical / counter-stereotypical
# plus: does adding target_gender or congruence to story_type improve fit?
#
# Effect measure: marginal R^2 (R2m; Nakagawa & Schielzeth, 2013).
# All models: random intercept for participant, ML estimation (needed for LRTs).
# =============================================================================

source(here::here("R", "00_setup.R"))

liwc      <- readRDS(path_processed("liwc.rds"))
aspectemo <- readRDS(path_processed("aspectemo.rds"))
sadcat    <- readRDS(path_processed("sadcat.rds"))

#' Fit the five predictor models (+ null model) for one outcome and report
#' R2m, LRTs, estimated marginal means, pairwise contrasts (Bonferroni),
#' raw means and residual diagnostics.
#' `data` must contain: story_type, target_gender, congruence, participant_id
#' and have no missing values on `dv`.
compare_predictors <- function(dv, data, label) {
  rhs <- c(null          = "1",
           story_type    = "story_type",
           target_gender = "target_gender",
           congruence    = "congruence",
           type_gender   = "story_type + target_gender",
           type_cong     = "story_type + congruence")

  # Note: single weak predictors may trigger "boundary (singular) fit"
  # warnings -- expected when they explain almost no variance.
  models <- lapply(rhs, function(r) {
    lmer(as.formula(sprintf("`%s` ~ %s + (1 | participant_id)", dv, r)),
         data = data, REML = FALSE)
  })

  r2 <- tibble(
    DV    = label,
    model = c("story type (4 categories)", "target gender", "congruence",
              "story type + target gender", "story type + congruence"),
    R2m   = sapply(models[-1], function(m) MuMIn::r.squaredGLMM(m)[1, "R2m"])
  )

  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("OUTCOME: ", label, " (N = ", nrow(data), ")\n", sep = "")
  cat(strrep("=", 70), "\n")
  print(r2 %>% mutate(R2m = round(R2m, 3)))

  cat("\n--- Story type vs null model (LRT) ---\n")
  print(anova(models$null, models$story_type))
  cat("\n--- Does target gender add anything beyond story type? (LRT) ---\n")
  print(anova(models$story_type, models$type_gender))
  cat("\n--- Does congruence add anything beyond story type? (LRT) ---\n")
  print(anova(models$story_type, models$type_cong))

  emm <- emmeans(models$story_type, pairwise ~ story_type, adjust = "bonferroni")
  cat("\n--- Estimated marginal means per story type ---\n")
  print(emm$emmeans)
  cat("\n--- Pairwise contrasts (Bonferroni) ---\n")
  print(emm$contrasts)

  cat("\n--- Raw means per story type ---\n")
  data %>%
    group_by(story_type) %>%
    summarise(n = n(), M = round(mean(.data[[dv]]), 3), SD = round(sd(.data[[dv]]), 3),
              .groups = "drop") %>%
    print()

  diagnose_lmm(models$story_type, paste0(label, " ~ story_type (main model)"))
  invisible(list(models = models, r2 = r2))
}

analyses <- list(
  list(dv = "Tone",               data = liwc,      label = "LIWC Tone"),
  list(dv = "score_only_charged", data = aspectemo, label = "AspectEmo score_only_charged"),
  list(dv = "warmth",             data = sadcat,    label = "SADCAT warmth"),
  list(dv = "competence",         data = sadcat,    label = "SADCAT competence")
)

results <- list()
for (a in analyses) {
  complete <- a$data[!is.na(a$data[[a$dv]]), ]
  cat(sprintf("\n%s: N (complete cases) = %d of %d (missing: %.1f%%)\n",
              a$label, nrow(complete), nrow(a$data),
              100 * (1 - nrow(complete) / nrow(a$data))))
  results[[a$label]] <- compare_predictors(a$dv, complete, a$label)
}

# ---- Summary table ----------------------------------------------------------
table_r2m <- purrr::map_dfr(results, "r2") %>% mutate(R2m = round(R2m, 3))

cat("\n\n", strrep("=", 70), "\nSUMMARY: marginal R2 for all outcomes and models\n",
    strrep("=", 70), "\n", sep = "")
print(table_r2m, n = Inf)
write_csv(table_r2m, path_tables("R2m_story_type_comparison.csv"))
