# =============================================================================
# 00_setup.R
# Shared setup sourced by every analysis script: packages, file locations,
# factor coding, colour palette and small helper functions.
# =============================================================================

# ---- Packages ---------------------------------------------------------------
# Packages used via `pkg::fun()` (not attached): here, MuMIn, car, boot,
# effectsize, readODS, lmtest. Attached: tidyverse, lme4, lmerTest, emmeans.
# (car is deliberately not attached: it masks dplyr::recode.)

required_packages <- c(
  "here", "tidyverse", "lme4", "lmerTest", "emmeans", "MuMIn",
  "car", "boot", "effectsize", "readODS", "lmtest"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  stop(
    "Missing packages: ", paste(missing_packages, collapse = ", "), "\n",
    "Install them with: install.packages(c(\"",
    paste(missing_packages, collapse = "\", \""), "\"))",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4)
  library(lmerTest)   # Satterthwaite p-values for lmer() models
  library(emmeans)
})

# ---- Paths ------------------------------------------------------------------
# Raw data is NOT part of the repository (see README). Put the files below
# into data/raw/ before running the pipeline.

FILE_LIWC          <- "Bazaodp-Data.csv"
FILE_SADCAT_CORPUS <- "Korpus magisterka (eng).ods"

path_raw       <- function(...) here::here("data", "raw", ...)
path_processed <- function(...) here::here("data", "processed", ...)
path_figures   <- function(...) here::here("output", "figures", ...)
path_tables    <- function(...) here::here("output", "tables", ...)

for (d in c(here::here("data", "raw"), here::here("data", "processed"),
            here::here("output", "figures"), here::here("output", "tables"))) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ---- Experimental design coding ---------------------------------------------
# Design: target gender (man / woman) x behaviour stereotype (masculine /
# feminine) x behaviour valence (positive / negative).
# Congruence = does the behaviour match the stereotype of the target's gender?

LEVELS_STORY_TYPE <- c("feminine_positive", "feminine_negative",
                       "masculine_positive", "masculine_negative")

#' Build the standard design factors from three character columns:
#'   target_gender: "man" / "woman"
#'   stereotype:    "masculine" / "feminine"
#'   behaviour:     "positive" / "negative"
#' Reference levels (first level) are: man, masculine, negative,
#' stereotypical, feminine_positive.
add_design_factors <- function(data) {
  data %>%
    mutate(
      congruence = if_else(
        (target_gender == "woman" & stereotype == "feminine") |
          (target_gender == "man" & stereotype == "masculine"),
        "stereotypical", "counter-stereotypical"
      ),
      story_type    = paste(stereotype, behaviour, sep = "_"),
      target_gender = factor(target_gender, levels = c("man", "woman")),
      stereotype    = factor(stereotype,    levels = c("masculine", "feminine")),
      behaviour     = factor(behaviour,     levels = c("negative", "positive")),
      congruence    = factor(congruence,
                             levels = c("stereotypical", "counter-stereotypical")),
      story_type    = factor(story_type,    levels = LEVELS_STORY_TYPE)
    )
}

#' Participant gender from the numeric export code (0 = woman, 1 = man,
#' 99 = other). Codes are used instead of text labels to avoid locale /
#' encoding problems with Polish characters.
code_participant_gender <- function(code) {
  factor(code, levels = c(0, 1, 99), labels = c("woman", "man", "other"))
}

center_grand_mean <- function(x) x - mean(x, na.rm = TRUE)

# ---- Plot labels and palette ------------------------------------------------
# All figure labels live here, so they can be translated in one place
# (e.g. to Polish for the thesis version of the figures).

GROUP_LEVELS <- c(
  "Woman\nmasculine neg.", "Man\nmasculine neg.",
  "Woman\nfeminine neg.",  "Man\nfeminine neg.",
  "Woman\nmasculine pos.", "Man\nmasculine pos.",
  "Woman\nfeminine pos.",  "Man\nfeminine pos."
)

GROUP_COLOURS <- setNames(
  c("#9B8EC4", "#5B4A8A", "#C47F7F", "#8A3A3A",   # negative behaviours
    "#56B4E9", "#0072B2", "#E69F00", "#D55E00"),  # positive behaviours
  GROUP_LEVELS
)

make_group_label <- function(target_gender, stereotype, behaviour) {
  paste0(
    if_else(target_gender == "woman", "Woman", "Man"), "\n",
    stereotype, " ",
    if_else(behaviour == "positive", "pos.", "neg.")
  )
}

# ---- Descriptive helpers ----------------------------------------------------
skewness_of <- function(x) {
  x <- x[!is.na(x)]
  mean((x - mean(x))^3) / sd(x)^3
}

excess_kurtosis_of <- function(x) {
  x <- x[!is.na(x)]
  mean((x - mean(x))^4) / sd(x)^4 - 3
}

# ---- Assumption checks ------------------------------------------------------

#' Residual diagnostics for a fitted (g)lmer model: Shapiro-Wilk, skewness,
#' excess kurtosis, share of |standardised residuals| > 3, singular-fit flag,
#' optional Q-Q plot and histogram.
diagnose_lmm <- function(model, label, plots = FALSE) {
  cat("\n--- Diagnostics:", label, "---\n")
  res <- residuals(model)
  n   <- length(res)

  if (n > 5000) {          # shapiro.test() accepts at most 5000 values
    set.seed(42)
    res_sample <- sample(res, 5000)
    cat("Shapiro-Wilk (random sample, n = 5000):\n")
  } else {
    res_sample <- res
    cat(sprintf("Shapiro-Wilk (n = %d):\n", n))
  }
  sw <- shapiro.test(res_sample)
  print(sw)

  cat(sprintf("Residual skewness:        %.3f\n", skewness_of(res)))
  cat(sprintf("Residual excess kurtosis: %.3f\n", excess_kurtosis_of(res)))
  n_out <- sum(abs(scale(res)) > 3)
  cat(sprintf("Observations with |standardised residual| > 3: %d (%.1f%%)\n",
              n_out, 100 * n_out / n))
  cat("Singular fit:", isSingular(model), "\n")

  if (plots) {
    par(mfrow = c(1, 2))
    qqnorm(res, main = paste("Q-Q:", label)); qqline(res, col = "red")
    hist(res, breaks = 30, col = "lightblue",
         main = paste("Residuals:", label), xlab = "Residuals")
    par(mfrow = c(1, 1))
  }
  invisible(sw$p.value)
}

#' Plain-language reading of a Shapiro-Wilk result (decision rule used in the
#' thesis: n >= 200 and |skewness| < 2 -> parametric test is acceptable;
#' otherwise add a bootstrap).
interpret_normality <- function(p_shapiro, n, label, test = "LMM") {
  cat(sprintf("\nNormality interpretation for %s:\n", label))
  if (p_shapiro >= 0.05) {
    cat("  OK: no evidence against normality (p >= .05)\n")
  } else if (n >= 200) {
    cat(sprintf("  NOTE: p < .05, but n >= 200 and the %s is robust to moderate\n", test))
    cat("  departures from normality. If |skewness| < 2, continue with the\n")
    cat("  parametric test; if |skewness| >= 2, add a bootstrap.\n")
  } else {
    cat("  VIOLATION: p < .05 and n < 200 -- consider a bootstrap.\n")
  }
}

#' Per-group Shapiro-Wilk, skewness / kurtosis and Levene's test (median
#' centred) for a raw outcome. `group_var` must be a factor column.
check_group_assumptions <- function(data, dv, group_var, title = "") {
  cat("\n--- Assumption check:", title, "| DV:", dv, "---\n")
  y <- data[[dv]]
  g <- data[[group_var]]

  cat("Shapiro-Wilk per group:\n")
  for (lev in levels(g)) {
    sub <- y[g == lev]; sub <- sub[!is.na(sub)]
    if (length(unique(sub)) == 1) {
      cat(sprintf("  %-22s n = %d  ZERO VARIANCE (value = %.4f)\n",
                  lev, length(sub), sub[1]))
    } else if (length(sub) >= 3 && length(sub) <= 5000) {
      sw <- shapiro.test(sub)
      cat(sprintf("  %-22s W = %.4f  p = %.4f  n = %d\n",
                  lev, sw$statistic, sw$p.value, length(sub)))
    } else {
      cat(sprintf("  %-22s n = %d  (Shapiro-Wilk not applicable)\n", lev, length(sub)))
    }
  }

  cat("Skewness / excess kurtosis per group:\n")
  for (lev in levels(g)) {
    sub <- y[g == lev]; sub <- sub[!is.na(sub)]
    cat(sprintf("  %-22s skew = %6.3f  kurt = %6.3f\n",
                lev, skewness_of(sub), excess_kurtosis_of(sub)))
  }

  lev_test <- tryCatch(
    car::leveneTest(y ~ g, data = data.frame(y, g), center = median),
    error = function(e) NULL
  )
  if (!is.null(lev_test)) {
    cat(sprintf("Levene test (median): F(%d, %d) = %.3f, p = %.4f\n",
                lev_test$Df[1], lev_test$Df[2],
                lev_test$`F value`[1], lev_test$`Pr(>F)`[1]))
  }
  invisible(NULL)
}


#' Residual diagnostics for a fitted lm: Shapiro-Wilk, Breusch-Pagan,
#' skewness, excess kurtosis, outliers, optional plots.
diagnose_lm <- function(model, label, plots = FALSE) {
  cat("\n--- Diagnostics:", label, "---\n")
  res <- residuals(model)
  n   <- length(res)
  if (n > 5000) {
    set.seed(42); res_sample <- sample(res, 5000)
    cat("Shapiro-Wilk (random sample, n = 5000):\n")
  } else {
    res_sample <- res
    cat(sprintf("Shapiro-Wilk (n = %d):\n", n))
  }
  sw <- shapiro.test(res_sample)
  print(sw)
  cat("Breusch-Pagan test (homoscedasticity):\n")
  print(lmtest::bptest(model))
  cat(sprintf("Residual skewness:        %.3f\n", skewness_of(res)))
  cat(sprintf("Residual excess kurtosis: %.3f\n", excess_kurtosis_of(res)))
  n_out <- sum(abs(scale(res)) > 3)
  cat(sprintf("Observations with |standardised residual| > 3: %d (%.1f%%)\n",
              n_out, 100 * n_out / n))
  if (plots) {
    par(mfrow = c(1, 2))
    qqnorm(res, main = paste("Q-Q:", label)); qqline(res, col = "red")
    plot(fitted(model), res, main = paste("Residuals vs fitted:", label),
         xlab = "Fitted values", ylab = "Residuals")
    abline(h = 0, col = "red", lty = 2)
    par(mfrow = c(1, 1))
  }
  invisible(sw$p.value)
}

# ---- Between-subjects test: Welch + Cohen's d + optional BCa bootstrap --------
# Used for H2 and H3 with both LIWC and AspectEmo outcomes. Cohen's d is
# (group 2) - (group 1), i.e. the sign follows the factor level order.

# Statistics for boot(): `d` has columns y (outcome) and g (two-level factor);
# both return (group 2) - (group 1).
diff_means_boot <- function(d, i) {
  x <- d[i, ]
  m <- tapply(x$y, x$g, mean)
  unname(m[2] - m[1])
}

cohens_d_boot <- function(d, i) {
  x  <- d[i, ]
  m  <- tapply(x$y, x$g, mean)
  s  <- tapply(x$y, x$g, sd)
  n  <- tapply(x$y, x$g, length)
  sp <- sqrt(((n[1] - 1) * s[1]^2 + (n[2] - 1) * s[2]^2) / (sum(n) - 2))
  unname((m[2] - m[1]) / sp)
}

boot_ci_safe <- function(b) {
  tryCatch(
    boot::boot.ci(b, type = "bca"),
    error = function(e) {
      cat("  BCa interval failed -- falling back to percentile interval\n")
      boot::boot.ci(b, type = "perc")
    }
  )
}

ci_bounds <- function(ci) {
  slot <- if (!is.null(ci$bca)) ci$bca else ci$percent
  tail(as.numeric(slot[1, ]), 2)
}

between_groups_test <- function(data, dv, group_var, label,
                                skew_threshold = 2, n_boot = 5000, seed = 2026) {
  cat("\n\n", strrep("=", 70), "\n", sep = "")
  cat("Hypothesis:", label, "| DV:", dv, "\n")
  cat(strrep("-", 70), "\n")

  d <- data %>%
    transmute(y = .data[[dv]], g = .data[[group_var]]) %>%
    filter(!is.na(y)) %>%
    mutate(g = droplevels(factor(g)))
  d <- as.data.frame(d)
  stopifnot(nlevels(d$g) == 2)

  zero_var <- tapply(d$y, d$g, function(x) length(unique(x)) == 1)
  if (any(zero_var)) {
    cat("WARNING: zero variance in group(s):",
        paste(names(zero_var)[zero_var], collapse = ", "), "-- test skipped.\n")
    return(invisible(NULL))
  }

  cat("\nDescriptive statistics:\n")
  desc <- tapply(d$y, d$g, function(x) {
    c(n = length(x), M = mean(x), SD = sd(x),
      skew = skewness_of(x), kurt = excess_kurtosis_of(x))
  })
  print(do.call(rbind, desc))
  need_boot <- any(abs(sapply(desc, function(x) x["skew"])) >= skew_threshold)

  lev <- car::leveneTest(y ~ g, data = d, center = median)
  cat(sprintf("\nLevene test (median): F(%d, %d) = %.3f, p = %.4f\n",
              lev$Df[1], lev$Df[2], lev$`F value`[1], lev$`Pr(>F)`[1]))

  cat("\n>> Primary test: Welch t-test (used regardless of skewness)\n")
  print(t.test(y ~ g, data = d, var.equal = FALSE))
  cat(sprintf("Effect size: Cohen's d = %.3f (%s - %s)\n",
              cohens_d_boot(d, seq_len(nrow(d))), levels(d$g)[2], levels(d$g)[1]))

  if (!need_boot) {
    cat(sprintf("\n>> |skewness| < %g in both groups -- bootstrap not required.\n",
                skew_threshold))
    return(invisible(NULL))
  }

  cat(sprintf("\n>> |skewness| >= %g detected -- BCa bootstrap validation (R = %d)\n",
              skew_threshold, n_boot))
  set.seed(seed)
  b_diff <- boot::boot(d, diff_means_boot, R = n_boot, strata = d$g)
  b_d    <- boot::boot(d, cohens_d_boot,   R = n_boot, strata = d$g)
  ci_diff <- ci_bounds(boot_ci_safe(b_diff))
  ci_d    <- ci_bounds(boot_ci_safe(b_d))

  cat(sprintf("  Mean difference = %.4f, 95%% CI [%.4f, %.4f]\n",
              b_diff$t0, ci_diff[1], ci_diff[2]))
  cat(sprintf("  Cohen's d       = %.4f, 95%% CI [%.4f, %.4f]\n",
              b_d$t0, ci_d[1], ci_d[2]))
  cat("  >>", if (prod(ci_diff) > 0) "CI excludes zero -- reliable effect"
              else "CI includes zero -- no reliable effect", "\n")
  invisible(NULL)
}
