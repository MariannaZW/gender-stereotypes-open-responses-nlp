# =============================================================================
# 05_hypotheses_aspectemo.R
# Hypothesis tests H1, H2, H3, H7, H8 plus exploratory analyses for the
# AspectEmo outcomes:
#   score_all_tokens   -- sentiment score over all tokens
#   score_only_charged -- score over emotionally charged tokens only
#
# Models: LMM with random intercept for participant; plain lm only where the
# LMM is singular / not identifiable (H7 and EXP-H7 for score_all_tokens, as
# in the thesis). Between-subjects comparisons (H2, H3): Welch t-test +
# Cohen's d, with BCa bootstrap if |skewness| >= 2 (same procedure as for
# LIWC Tone in 04_hypotheses_liwc_tone.R).
#
# Reference levels: congruence = stereotypical, target_gender = man,
# behaviour = negative.
#
# Input: data/processed/aspectemo.rds (see 01_prepare_data.R)
# =============================================================================

source(here::here("R", "00_setup.R"))

dat <- readRDS(path_processed("aspectemo.rds"))
OUTCOMES <- c("score_all_tokens", "score_only_charged")

cat("Participant gender coding check:\n")
print(table(dat$participant_gender_code, dat$participant_gender, useNA = "always"))
cat("\nDesign check (congruence x target gender x behaviour):\n")
print(table(dat$congruence, dat$target_gender, dat$behaviour))

# ---- Helpers ----------------------------------------------------------------
# Models are fitted inside functions with the formula built there, so that
# emmeans can later recover the data from the formula's environment.

complete_for <- function(data, dv) data[!is.na(data[[dv]]), ]

fit_lmm <- function(dv, rhs, data) {
  d <- complete_for(data, dv)
  lmer(as.formula(paste(dv, "~", rhs, "+ (1 | participant_id)")), data = d, REML = TRUE)
}

fit_lm <- function(dv, rhs, data) {
  d <- complete_for(data, dv)
  lm(as.formula(paste(dv, "~", rhs)), data = d)
}

effect_size_for <- function(model, spec) {
  eff_size(emmeans(model, spec), sigma = sigma(model), edf = df.residual(model))
}

# =============================================================================
# H1: negative behaviours -- counter-stereotypical vs stereotypical
# =============================================================================
cat("\n\n### H1: negative behaviours, congruence ###\n")

df_h1 <- dat %>% filter(behaviour == "negative")
h1_models <- list()

for (dv in OUTCOMES) {
  cat("\n", strrep("=", 60), "\nH1 |", dv, "\n", strrep("=", 60), "\n", sep = "")
  m <- fit_lmm(dv, "congruence + target_gender", df_h1)
  h1_models[[dv]] <- m

  p_sw <- diagnose_lmm(m, paste("H1", dv), plots = TRUE)
  interpret_normality(p_sw, nobs(m), paste("H1", dv))

  print(summary(m))
  cat("\nEstimated marginal means (congruence):\n")
  print(emmeans(m, ~ congruence))
  cat("\nContrast:\n")
  print(pairs(emmeans(m, ~ congruence)))
  cat("\nEffect size (Cohen's d):\n")
  print(effect_size_for(m, ~ congruence))

  cat("\nLevene test (median) by congruence:\n")
  print(car::leveneTest(as.formula(paste(dv, "~ congruence")),
                        data = complete_for(df_h1, dv), center = median))
}

# Cluster bootstrap for the congruence coefficient, score_only_charged
# (residual skewness is ~2 for this outcome). Participants -- not rows -- are
# resampled, because observations are repeated within participants. The
# statistic is the lm coefficient of congruence with the same fixed effects.
cat("\n--- H1 | score_only_charged: cluster bootstrap BCa (5000 replicates) ---\n")
d_boot <- complete_for(df_h1, "score_only_charged")
by_participant <- split(d_boot, droplevels(d_boot$participant_id))
participant_ids <- names(by_participant)
cat("Participants in the bootstrap:", length(participant_ids), "\n")

congruence_coef_boot <- function(ids_pool, i) {
  resampled <- bind_rows(by_participant[ids_pool[i]])
  coefs <- coef(lm(score_only_charged ~ congruence + target_gender, data = resampled))
  unname(coefs[grep("^congruence", names(coefs))])
}

set.seed(2026)
boot_h1 <- boot::boot(data = participant_ids, statistic = congruence_coef_boot, R = 5000)
print(boot::boot.ci(boot_h1, type = c("perc", "bca")))

# =============================================================================
# H2: man vs woman, positive feminine-stereotypical behaviours
# =============================================================================
cat("\n\n### H2: man vs woman, positive feminine behaviours ###\n")

# Reference = woman, so d > 0 means man > woman.
df_h2 <- dat %>%
  filter(stereotype == "feminine", behaviour == "positive") %>%
  mutate(target_gender = fct_relevel(target_gender, "woman"))

for (dv in OUTCOMES) {
  m <- fit_lm(dv, "target_gender", df_h2)
  p_sw <- diagnose_lm(m, paste("H2", dv), plots = TRUE)
  interpret_normality(p_sw, nobs(m), paste("H2", dv), test = "lm")
  print(summary(m))
  between_groups_test(df_h2, dv, "target_gender", label = paste("H2 --", dv))
}

# =============================================================================
# H3: man + feminine (positive) vs woman + masculine (positive)
# =============================================================================
cat("\n\n### H3: man + feminine vs woman + masculine (positive behaviours) ###\n")

# Cohen's d = man_feminine - woman_masculine
df_h3 <- dat %>%
  filter(behaviour == "positive") %>%
  filter((target_gender == "man" & stereotype == "feminine") |
           (target_gender == "woman" & stereotype == "masculine")) %>%
  mutate(h3_group = factor(if_else(target_gender == "man", "man_feminine", "woman_masculine"),
                           levels = c("woman_masculine", "man_feminine")))

for (dv in OUTCOMES) {
  m <- fit_lm(dv, "h3_group", df_h3)
  p_sw <- diagnose_lm(m, paste("H3", dv), plots = TRUE)
  interpret_normality(p_sw, nobs(m), paste("H3", dv), test = "lm")
  print(summary(m))
  between_groups_test(df_h3, dv, "h3_group", label = paste("H3 --", dv))
}

# =============================================================================
# H7: sexism as moderator of congruence
# =============================================================================
cat("\n\n### H7: congruence x sexism ###\n")

# lm for score_all_tokens (LMM singular in the thesis analysis), LMM for
# score_only_charged. NOTE: the lm ignores repeated measures; check
# isSingular() of the corresponding LMM before reporting.
h7_random_intercept <- c(score_all_tokens = FALSE, score_only_charged = TRUE)
h7_rhs <- "congruence * Sexism_c + target_gender + behaviour"

h7_models <- list()
for (dv in OUTCOMES) {
  cat("\n", strrep("=", 60), "\nH7 |", dv, "\n", strrep("=", 60), "\n", sep = "")
  m <- if (h7_random_intercept[[dv]]) fit_lmm(dv, h7_rhs, dat) else fit_lm(dv, h7_rhs, dat)
  h7_models[[dv]] <- m

  p_sw <- if (inherits(m, "merMod")) diagnose_lmm(m, paste("H7", dv), plots = TRUE)
          else diagnose_lm(m, paste("H7", dv), plots = TRUE)
  interpret_normality(p_sw, nobs(m), paste("H7", dv))

  print(summary(m))
  cat("\nSimple slopes of sexism per congruence condition:\n")
  print(emtrends(m, ~ congruence, var = "Sexism_c"))
}

# =============================================================================
# H8: participant gender as moderator of congruence
# =============================================================================
cat("\n\n### H8: congruence x participant gender ###\n")

df_h8 <- dat %>%
  filter(participant_gender != "other") %>%
  mutate(participant_gender = droplevels(participant_gender))
cat(sprintf("N after excluding the non-binary participant(s): %d (removed %d)\n",
            nrow(df_h8), nrow(dat) - nrow(df_h8)))

for (dv in OUTCOMES) {
  cat("\n", strrep("=", 60), "\nH8 |", dv, "\n", strrep("=", 60), "\n", sep = "")
  m <- fit_lmm(dv, "congruence * participant_gender + target_gender + behaviour", df_h8)

  p_sw <- diagnose_lmm(m, paste("H8", dv), plots = TRUE)
  interpret_normality(p_sw, nobs(m), paste("H8", dv))

  print(summary(m))
  cat("\nCongruence effect per participant gender (Bonferroni):\n")
  print(pairs(emmeans(m, ~ congruence | participant_gender), adjust = "bonferroni"))
  cat("\nEffect size:\n")
  print(effect_size_for(m, ~ congruence | participant_gender))
}

# =============================================================================
# EXPLORATORY ANALYSES
# =============================================================================
cat("\n\n", strrep("=", 70), "\nEXPLORATORY ANALYSES\n", strrep("=", 70), "\n", sep = "")

# ---- EXP-H1: congruence x target gender (negative behaviours) ---------------
cat("\n### EXP-H1: congruence x target gender ###\n")

for (dv in OUTCOMES) {
  cat("\n--- ", dv, " ---\n", sep = "")
  m <- fit_lmm(dv, "congruence * target_gender", df_h1)
  print(summary(m))
  cat("\nCongruence effect per target gender (Bonferroni):\n")
  print(pairs(emmeans(m, ~ congruence | target_gender), adjust = "bonferroni"))
}

# ---- EXP-H7: sexism subscales as moderators ---------------------------------
# Multiple testing: 4 subscales per outcome -> Bonferroni threshold
# .05 / 4 = .0125 per outcome. If both outcomes are counted as one family
# (8 tests), the threshold would be .05 / 8 = .00625.
cat("\n\n### EXP-H7: sexism subscales as moderators ###\n")
cat("Bonferroni threshold per outcome (4 subscales): p <", .05 / 4, "\n")

for (sub in c("BS_K", "HS_K", "BS_M", "HS_M")) {
  sub_c <- paste0(sub, "_c")
  cat("\n", strrep("-", 60), "\nSubscale: ", sub, "\n", strrep("-", 60), "\n", sep = "")
  rhs <- paste0("congruence * ", sub_c, " + target_gender + behaviour")

  for (dv in OUTCOMES) {
    m <- if (h7_random_intercept[[dv]]) fit_lmm(dv, rhs, dat) else fit_lm(dv, rhs, dat)
    coefs <- summary(m)$coefficients
    cat("\n", dv, ":\n", sep = "")
    print(coefs[grep("congruence|BS_|HS_", rownames(coefs)), , drop = FALSE])
    cat("\nSimple slopes per congruence condition:\n")
    print(emtrends(m, ~ congruence, var = sub_c))
  }
}

cat("\nNOTE: exploratory results -- treat as preliminary.\n")

# ---- Descriptives: positive behaviours --------------------------------------
cat("\nMeans for positive behaviours by target gender and stereotype:\n")
dat %>%
  filter(behaviour == "positive") %>%
  group_by(target_gender, stereotype) %>%
  summarise(n = n(),
            mean_all     = round(mean(score_all_tokens,   na.rm = TRUE), 3),
            mean_charged = round(mean(score_only_charged, na.rm = TRUE), 3),
            .groups = "drop") %>%
  arrange(desc(mean_charged)) %>%
  print()
