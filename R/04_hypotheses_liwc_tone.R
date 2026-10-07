# =============================================================================
# 04_hypotheses_liwc_tone.R
# Hypothesis tests H1, H2, H3, H7, H8 for the LIWC outcome `Tone`.
#
# Statistical strategy
#   H1      within-subjects (repeated measures): LMM with random intercept
#           for participant.
#   H2, H3  between-subjects comparisons (one observation per participant in
#           each comparison): Welch t-test as the primary test, always;
#           stratified BCa bootstrap as validation when |skewness| >= 2.
#           Cohen's d is reported for both, so effect sizes are comparable
#           with the AspectEmo analysis.
#   H7, H8  within-subjects, full sample: LMM with interaction + residual
#           diagnostics. For H7 the linearity of Tone ~ sexism is checked
#           (scatter plot with LOESS + likelihood-ratio test of a quadratic
#           term).
#
# Input: data/processed/liwc.rds (see 01_prepare_data.R)
# =============================================================================

source(here::here("R", "00_setup.R"))

# For H1, H7 and H8 the reference level of congruence is "counter-stereotypical"
# (as in the thesis), so interaction coefficients are relative to that group.
dat <- readRDS(path_processed("liwc.rds")) %>%
  mutate(congruence = fct_relevel(congruence, "counter-stereotypical"))

cat("Rows:", nrow(dat), "| Participants:", nlevels(dat$participant_id), "\n")

lmm_control <- lmerControl(optimizer = "bobyqa")

# =============================================================================
# H1: Negative behaviours -- counter-stereotypical vs stereotypical (Tone)
# =============================================================================
cat("\n\n", strrep("#", 70), "\n# H1: negative behaviours, counter-stereotypical vs stereotypical\n",
    strrep("#", 70), "\n", sep = "")

dat_h1 <- dat %>% filter(behaviour == "negative")
check_group_assumptions(dat_h1, "Tone", "congruence", title = "H1 | negative behaviours")

m_h1 <- lmer(Tone ~ congruence + (1 | participant_id),
             data = dat_h1, REML = FALSE, control = lmm_control)
diagnose_lmm(m_h1, "H1 Tone ~ congruence")
cat("\nFixed effects (Satterthwaite):\n")
print(summary(m_h1)$coefficients, digits = 4)

emm_h1 <- emmeans(m_h1, ~ congruence)
print(emm_h1)
# Level order is (counter-stereotypical, stereotypical) -> counter minus stereotypical
print(contrast(emm_h1, list("counter - stereotypical" = c(1, -1)), adjust = "none"))

# =============================================================================
# H2: Man vs woman, positive behaviours stereotypical for women
# =============================================================================
cat("\n\n", strrep("#", 70), "\n# H2: man vs woman, positive feminine-stereotypical behaviour\n",
    strrep("#", 70), "\n", sep = "")

# Reference group = woman, so d > 0 means man > woman.
dat_h2 <- dat %>%
  filter(behaviour == "positive", stereotype == "feminine") %>%
  mutate(target_gender = fct_relevel(target_gender, "woman"))

check_group_assumptions(dat_h2, "Tone", "target_gender", title = "H2")
between_groups_test(dat_h2, "Tone", "target_gender", label = "H2 -- Tone")

# =============================================================================
# H3: Man/feminine-stereotypical vs woman/masculine-stereotypical (positive)
# =============================================================================
cat("\n\n", strrep("#", 70), "\n# H3: man + feminine stereotype vs woman + masculine stereotype (positive)\n",
    strrep("#", 70), "\n", sep = "")

dat_h3 <- dat %>%
  filter(behaviour == "positive") %>%
  mutate(h3_group = case_when(
    target_gender == "man"   & stereotype == "feminine"  ~ "man_feminine",
    target_gender == "woman" & stereotype == "masculine" ~ "woman_masculine",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(h3_group)) %>%
  mutate(h3_group = factor(h3_group, levels = c("woman_masculine", "man_feminine")))

check_group_assumptions(dat_h3, "Tone", "h3_group", title = "H3")
between_groups_test(dat_h3, "Tone", "h3_group", label = "H3 -- Tone")

# =============================================================================
# H7: Sexism x congruence -> Tone (overall scale + four subscales)
# =============================================================================
cat("\n\n", strrep("#", 70), "\n# H7: sexism x congruence -> Tone\n",
    "# Scales: Sexism (overall), BS_K, HS_K, BS_M, HS_M\n", strrep("#", 70), "\n", sep = "")

#' Linearity check for DV ~ sexism: scatter plot with linear and LOESS fits
#' per congruence level, plus an LRT of a quadratic term (shared curvature
#' across congruence levels; not a hypothesis about curvature differences).
check_linearity_sexism <- function(data, dv, predictor_c, label) {
  cat("\n  >> Linearity check:", label, "\n")

  p <- ggplot(data, aes(x = .data[[predictor_c]], y = .data[[dv]])) +
    geom_point(alpha = .25, size = 1.4, colour = "grey40") +
    geom_smooth(method = "lm",    formula = y ~ x, se = TRUE,  colour = "#0072B2", linewidth = 1) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "#D55E00",
                linewidth = 1, linetype = "dashed") +
    facet_wrap(~ congruence) +
    labs(title    = paste("Linearity:", label),
         subtitle = "Blue = linear fit | Orange (dashed) = LOESS",
         x = predictor_c, y = dv) +
    theme_minimal(base_size = 11)
  print(p)
  file <- paste0("linearity_", gsub("[^a-zA-Z0-9]", "_", label), ".png")
  ggsave(path_figures(file), p, width = 7, height = 4.5, dpi = 300, bg = "white")

  f_lin  <- as.formula(paste0(dv, " ~ ", predictor_c, " * congruence + (1 | participant_id)"))
  f_quad <- as.formula(paste0(dv, " ~ ", predictor_c, " + I(", predictor_c, "^2) + congruence + ",
                              predictor_c, ":congruence + (1 | participant_id)"))
  m_lin  <- tryCatch(lmer(f_lin,  data = data, REML = FALSE, control = lmm_control),
                     error = function(e) NULL)
  m_quad <- tryCatch(lmer(f_quad, data = data, REML = FALSE, control = lmm_control),
                     error = function(e) NULL)
  if (is.null(m_lin) || is.null(m_quad)) {
    cat("     Quadratic model did not converge -- formal test skipped.\n")
    return(invisible(NULL))
  }
  lrt <- anova(m_lin, m_quad)
  p_quad <- lrt$`Pr(>Chisq)`[2]
  cat(sprintf("     Quadratic-term LRT: Chisq(%d) = %.3f, p = %.4f %s\n",
              lrt$Df[2], lrt$Chisq[2], p_quad,
              if (p_quad < .05) "*** NON-LINEAR ***" else "-- linearity OK"))
  invisible(list(plot = p, lrt = lrt))
}

sexism_vars <- c("Sexism", "BS_K", "HS_K", "BS_M", "HS_M")
h7_results  <- list()

for (sv in sexism_vars) {
  sv_c <- paste0(sv, "_c")
  cat("\n", strrep("*", 60), "\nSEXISM SCALE:", sv, "\n", strrep("*", 60), "\n", sep = "")

  # Does the participant's sexism level differ between congruence conditions?
  cors <- cor.test(dat[[sv]], dat$congruence_code, method = "pearson")
  cat(sprintf("Correlation of %s with congruence code: r = %.3f, p = %.4f\n",
              sv, cors$estimate, cors$p.value))

  m_h7 <- tryCatch(
    lmer(as.formula(paste0("Tone ~ ", sv_c, " * congruence + (1 | participant_id)")),
         data = dat, REML = FALSE, control = lmm_control),
    error = function(e) { cat("  Model did not converge:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(m_h7)) next

  diagnose_lmm(m_h7, paste("Tone ~", sv, "x congruence"))
  coef_tab <- summary(m_h7)$coefficients
  cat("\n  Fixed effects (Satterthwaite):\n")
  print(round(coef_tab, 4))

  int_row <- grep(paste0(sv_c, ":congruence"), rownames(coef_tab), fixed = TRUE)
  p_int <- NA_real_
  if (length(int_row) > 0) {
    p_int <- coef_tab[int_row, "Pr(>|t|)"]
    cat(sprintf("\n  >> Interaction %s x congruence: p = %.4f %s\n", sv, p_int,
                if (p_int < .05) "*** SIGNIFICANT ***" else if (p_int < .10) "(trend)" else ""))
  }

  if (!is.na(p_int) && p_int < .10) {
    cat("\n  Simple slopes (sexism within each congruence condition):\n")
    slopes <- emtrends(m_h7, ~ congruence, var = sv_c)
    print(slopes)
    cat("\n  Difference between slopes:\n")
    print(contrast(slopes, "pairwise"))
  }

  check_linearity_sexism(dat, "Tone", sv_c, label = paste0("Tone_vs_", sv))
  h7_results[[sv]] <- list(model = m_h7, p_interaction = p_int)
}

# =============================================================================
# H8: Participant gender x congruence -> Tone
# =============================================================================
cat("\n\n", strrep("#", 70), "\n# H8: participant gender x congruence -> Tone\n",
    strrep("#", 70), "\n", sep = "")

# Participants coded "other" (n too small for a separate group effect) are excluded.
dat_h8 <- dat %>%
  filter(participant_gender != "other") %>%
  mutate(participant_gender = droplevels(participant_gender))

m_h8 <- lmer(Tone ~ participant_gender * congruence + (1 | participant_id),
             data = dat_h8, REML = FALSE, control = lmm_control)

diagnose_lmm(m_h8, "H8 Tone ~ participant gender x congruence")
coef_h8 <- summary(m_h8)$coefficients
cat("\nFixed effects (Satterthwaite):\n")
print(round(coef_h8, 4))

int_row_h8 <- grep("participant_gender.*:congruence", rownames(coef_h8))
if (length(int_row_h8) > 0) {
  p_int_h8 <- coef_h8[int_row_h8, "Pr(>|t|)"]
  cat(sprintf("\n>> Interaction participant gender x congruence: p = %.4f %s\n", p_int_h8,
              if (p_int_h8 < .05) "*** SIGNIFICANT ***" else if (p_int_h8 < .10) "(trend)" else ""))
}

emm_h8 <- emmeans(m_h8, ~ congruence | participant_gender)
cat("\nEstimated marginal means (congruence within participant gender):\n")
print(emm_h8)
cat("\nSimple effects: counter-stereotypical - stereotypical, per participant gender:\n")
print(contrast(emm_h8, "pairwise", adjust = "none"))

cat("\n", strrep("=", 70), "\nLIWC Tone analysis (H1, H2, H3, H7, H8) finished.\n",
    strrep("=", 70), "\n", sep = "")
