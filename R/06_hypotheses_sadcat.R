# =============================================================================
# 06_hypotheses_sadcat.R
# Hypothesis tests H5 and H6 plus exploratory analyses for the SADCAT outcomes
# (Nicolas, Bai & Fiske, 2021):
#   warmth     = Sociability + Morality
#   competence = Ability + Assertiveness
#
# H5.1  Are women described as warmer than competent? (paired t-test)
# H5.2  Are women described as warmer than men? (LMM)
# H5.3  Is the warmth - competence gap larger for women than for men? (LMM)
# H6    Are counter-stereotypical men described as less agentic than
#       stereotypical men? (LMM)
# Exploratory: women by congruence; men vs women on masculine behaviours;
# stereotype x valence interaction.
#
# All LMMs: random intercept for participant, behaviour valence as covariate
# (unless stated otherwise), REML estimation; interaction tests use ML + LRT.
#
# Input: data/processed/sadcat.rds (see 02_sadcat_coding.R)
# =============================================================================

source(here::here("R", "00_setup.R"))

df <- readRDS(path_processed("sadcat.rds"))

# ---- Helpers ----------------------------------------------------------------

#' Estimated marginal means contrast + standardised effect size (d) for a
#' single factor of a fitted model.
report_contrast <- function(model, var, label) {
  cat("\n---", label, "---\n")
  emm <- emmeans(model, as.formula(paste("~", var)))
  print(pairs(emm))
  cat("Effect size (Cohen's d):\n")
  print(eff_size(emm, sigma = sigma(model), edf = df.residual(model)))
  invisible(emm)
}

fit_reml <- function(formula, data) {
  # Re-point the formula to this frame so emmeans can later find `data`
  environment(formula) <- environment()
  lmer(formula, data = data, REML = TRUE)
}

# =============================================================================
# H5.1: warmth vs competence within women (paired t-test); men for comparison
# =============================================================================
cat("\n\n### H5.1: are women described as warmer than competent? ###\n")

paired_warmth_competence <- function(data, label) {
  d <- data %>% filter(!is.na(warmth), !is.na(competence))
  diffs <- d$warmth - d$competence
  n <- length(diffs)

  cat("\n", strrep("=", 60), "\n", label, " (N complete pairs = ", n, ")\n",
      strrep("=", 60), "\n", sep = "")
  cat(sprintf("Means: warmth = %.3f, competence = %.3f, difference = %.3f\n",
              mean(d$warmth), mean(d$competence), mean(diffs)))

  print(t.test(d$warmth, d$competence, paired = TRUE))
  cat("\nEffect size (paired Cohen's d):\n")
  print(effectsize::cohens_d(d$warmth, d$competence, paired = TRUE))

  # Assumptions of the paired t-test concern the DIFFERENCES, not the raw scores
  cat("\n--- Assumptions: differences (warmth - competence) ---\n")
  sw <- shapiro.test(if (n > 5000) { set.seed(42); sample(diffs, 5000) } else diffs)
  print(sw)
  cat(sprintf("Skewness of differences: %.3f\n", skewness_of(diffs)))
  cat(sprintf("Excess kurtosis of differences: %.3f\n", excess_kurtosis_of(diffs)))
  n_out <- sum(abs(scale(diffs)) > 3)
  cat(sprintf("Observations with |standardised difference| > 3: %d (%.1f%%)\n",
              n_out, 100 * n_out / n))

  par(mfrow = c(1, 2))
  qqnorm(diffs, main = paste("Q-Q of differences:", label)); qqline(diffs, col = "red")
  hist(diffs, breaks = 30, col = "lightblue",
       main = paste("Differences:", label), xlab = "warmth - competence")
  par(mfrow = c(1, 1))

  interpret_normality(sw$p.value, n, label, test = "t-test")
  invisible(NULL)
}

paired_warmth_competence(filter(df, target_gender == "woman"), "Women")
paired_warmth_competence(filter(df, target_gender == "man"),   "Men (for comparison)")

# =============================================================================
# H5.2: warmth (and competence) by target gender
# =============================================================================
cat("\n\n### H5.2: are women described as warmer than men? ###\n")

m_warmth <- fit_reml(warmth     ~ target_gender + behaviour + (1 | participant_id), df)
m_comp   <- fit_reml(competence ~ target_gender + behaviour + (1 | participant_id), df)

cat("\n--- LMM: warmth ~ target gender + behaviour ---\n")
print(summary(m_warmth))
report_contrast(m_warmth, "target_gender", "Warmth: man - woman")
report_contrast(m_comp,   "target_gender", "Competence: man - woman (for comparison)")

# =============================================================================
# H5.3: warmth - competence gap by target gender
# =============================================================================
cat("\n\n### H5.3: is the warmth - competence gap larger for women than men? ###\n")

m_diff <- fit_reml(warmth_minus_comp ~ target_gender + behaviour + (1 | participant_id), df)
cat("\n--- LMM: (warmth - competence) ~ target gender + behaviour ---\n")
print(summary(m_diff))
report_contrast(m_diff, "target_gender", "Warmth - competence: man - woman")

cat("\nMean warmth, competence and gap by target gender:\n")
df %>%
  group_by(target_gender) %>%
  summarise(mean_warmth     = round(mean(warmth,            na.rm = TRUE), 3),
            mean_competence = round(mean(competence,        na.rm = TRUE), 3),
            mean_gap        = round(mean(warmth_minus_comp, na.rm = TRUE), 3),
            .groups = "drop") %>%
  print()

# =============================================================================
# H6: men -- counter-stereotypical vs stereotypical, competence
# =============================================================================
cat("\n\n### H6: counter-stereotypical men described as less agentic ###\n")

df_men <- df %>% filter(target_gender == "man")
cat(sprintf("Observations (men only): %d | complete competence: %d\n",
            nrow(df_men), sum(!is.na(df_men$competence))))

df_h6 <- df_men %>%
  filter(!is.na(competence)) %>%
  mutate(group_h6 = interaction(congruence, behaviour, drop = TRUE))

# (a) Assumptions on the raw outcome (before modelling)
check_group_assumptions(df_h6, "competence", "congruence", title = "H6 | by congruence")
cat("\nLevene (median) by behaviour:\n")
print(car::leveneTest(competence ~ behaviour, data = df_h6, center = median))
cat("\nLevene (median) by congruence x behaviour:\n")
print(car::leveneTest(competence ~ group_h6, data = df_h6, center = median))

# (b) Main model
m_h6_comp <- fit_reml(competence ~ congruence + behaviour + (1 | participant_id), df_men)
m_h6_warm <- fit_reml(warmth     ~ congruence + behaviour + (1 | participant_id), df_men)

cat("\n--- H6 | competence ---\n")
print(summary(m_h6_comp))
report_contrast(m_h6_comp, "congruence", "H6 competence: stereotypical - counter-stereotypical")
report_contrast(m_h6_warm, "congruence", "H6 warmth (for comparison)")

cat("\nMeans by congruence (men only):\n")
df_men %>%
  group_by(congruence) %>%
  summarise(mean_warmth     = round(mean(warmth,     na.rm = TRUE), 3),
            mean_competence = round(mean(competence, na.rm = TRUE), 3),
            n = n(), .groups = "drop") %>%
  print()

# (c) Residual assumptions + bootstrap if strongly skewed
res_h6  <- residuals(m_h6_comp)
sw_h6   <- shapiro.test(if (length(res_h6) > 5000) { set.seed(42); sample(res_h6, 5000) } else res_h6)
skew_h6 <- skewness_of(res_h6)
interpret_normality(sw_h6$p.value, length(res_h6), "H6 competence ~ congruence + behaviour")

plot(fitted(m_h6_comp), res_h6, main = "Residuals vs fitted: H6 competence",
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

if (abs(skew_h6) >= 2) {
  cat("\n|residual skewness| >= 2 -> BCa bootstrap for the congruence contrast\n")
  set.seed(2026)
  boot_h6 <- boot::boot(
    data = df_h6,
    statistic = function(d, i) {
      m <- tapply(d$competence[i], d$congruence[i], mean)
      unname(m["stereotypical"] - m["counter-stereotypical"])
    },
    R = 5000, strata = df_h6$congruence
  )
  print(boot::boot.ci(boot_h6, type = c("perc", "bca")))
} else {
  cat(sprintf("\nResidual skewness within bounds (|%.2f| < 2) -- bootstrap not required.\n", skew_h6))
}

cat("\nR2m (marginal R^2) of the H6 competence model:\n")
print(MuMIn::r.squaredGLMM(m_h6_comp))

# =============================================================================
# Exploratory 1: women -- warmth and competence by congruence
# =============================================================================
cat("\n\n### Exploratory: women by congruence ###\n")

df_women <- df %>% filter(target_gender == "woman")

df_women %>%
  group_by(congruence) %>%
  summarise(mean_warmth     = round(mean(warmth,     na.rm = TRUE), 3),
            mean_competence = round(mean(competence, na.rm = TRUE), 3),
            n = n(), .groups = "drop") %>%
  print()

m_w_comp <- fit_reml(competence ~ congruence + behaviour + (1 | participant_id), df_women)
m_w_warm <- fit_reml(warmth     ~ congruence + behaviour + (1 | participant_id), df_women)
report_contrast(m_w_comp, "congruence", "Women, competence: stereotypical - counter-stereotypical")
report_contrast(m_w_warm, "congruence", "Women, warmth: stereotypical - counter-stereotypical")

# =============================================================================
# Exploratory 2: masculine behaviours -- men vs women
# (is "agentic" behaviour described differently depending on target gender?)
# =============================================================================
cat("\n\n### Exploratory: masculine behaviours, men vs women ###\n")

df_masc <- df %>% filter(stereotype == "masculine")

df_masc %>%
  group_by(target_gender) %>%
  summarise(mean_warmth     = round(mean(warmth,     na.rm = TRUE), 3),
            mean_competence = round(mean(competence, na.rm = TRUE), 3),
            n = n(), .groups = "drop") %>%
  print()

m_masc_comp <- fit_reml(competence ~ target_gender + behaviour + (1 | participant_id), df_masc)
m_masc_warm <- fit_reml(warmth     ~ target_gender + behaviour + (1 | participant_id), df_masc)
report_contrast(m_masc_comp, "target_gender", "Masculine behaviours, competence: man - woman")
report_contrast(m_masc_warm, "target_gender", "Masculine behaviours, warmth: man - woman")

# Bootstrap of the gender coefficient. NOTE: this resamples observations and
# ignores the nesting within participants, so it is a robustness check only.
set.seed(42)
boot_masc <- boot::boot(
  data = df_masc %>% filter(!is.na(competence)),
  statistic = function(d, i) {
    unname(coef(lm(competence ~ target_gender + behaviour, data = d[i, ]))["target_genderwoman"])
  },
  R = 5000
)
cat("\nBootstrap: masculine behaviours, competence ~ target gender:\n")
print(boot::boot.ci(boot_masc, type = c("perc", "bca")))

# =============================================================================
# Exploratory 3: stereotype x valence interaction (story type)
# =============================================================================
cat("\n\n### Exploratory: stereotype x valence ###\n")

cat("\nMeans by stereotype, valence and target gender:\n")
df %>%
  group_by(stereotype, behaviour, target_gender) %>%
  summarise(mean_warmth     = round(mean(warmth,     na.rm = TRUE), 3),
            mean_competence = round(mean(competence, na.rm = TRUE), 3),
            n = n(), .groups = "drop") %>%
  arrange(stereotype, behaviour, target_gender) %>%
  print(n = Inf)

interaction_models <- list()
for (dv in c("competence", "warmth")) {
  cat("\n", strrep("-", 60), "\nDV:", dv, "\n", strrep("-", 60), "\n", sep = "")

  # Coefficients and marginal means: REML
  m_int <- fit_reml(as.formula(paste(dv, "~ stereotype * behaviour + (1 | participant_id)")), df)
  cat("\nCoefficients (REML):\n")
  print(summary(m_int)$coefficients)
  cat("\nEstimated marginal means:\n")
  print(emmeans(m_int, ~ stereotype * behaviour))

  # Formal LRT of the interaction: ML
  m_add_ML <- lmer(as.formula(paste(dv, "~ stereotype + behaviour + (1 | participant_id)")),
                   data = df, REML = FALSE)
  m_int_ML <- lmer(as.formula(paste(dv, "~ stereotype * behaviour + (1 | participant_id)")),
                   data = df, REML = FALSE)
  cat("\nLRT of the stereotype x behaviour interaction:\n")
  print(anova(m_add_ML, m_int_ML))

  interaction_models[[dv]] <- m_int
}

# =============================================================================
# Residual diagnostics for all models above
# =============================================================================
all_models <- list(
  "H5.2 warmth ~ target gender"                  = m_warmth,
  "H5.2 competence ~ target gender"              = m_comp,
  "H5.3 warmth - competence ~ target gender"     = m_diff,
  "H6 competence ~ congruence (men)"             = m_h6_comp,
  "H6 warmth ~ congruence (men)"                 = m_h6_warm,
  "Women competence ~ congruence"                = m_w_comp,
  "Women warmth ~ congruence"                    = m_w_warm,
  "Masculine behaviours competence ~ gender"     = m_masc_comp,
  "Masculine behaviours warmth ~ gender"         = m_masc_warm,
  "competence ~ stereotype x valence"            = interaction_models$competence,
  "warmth ~ stereotype x valence"                = interaction_models$warmth
)
purrr::iwalk(all_models, ~ diagnose_lmm(.x, .y, plots = TRUE))
