# =============================================================================
# 08_figures.R
# Figures for all outcomes.
#
# Bar charts: mean +/- SE by target gender x stereotype x valence. Negative and
# positive behaviours are separated by an empty slot on the x-axis, so that
# horizontal grid lines run uninterrupted across the whole plot. The y-axis
# shows the FULL theoretical range of each instrument, not just the data range.
# SE is computed over observations and ignores nesting within participants.
#
# Output: PNG files in output/figures/
# =============================================================================

source(here::here("R", "00_setup.R"))

liwc      <- readRDS(path_processed("liwc.rds"))
aspectemo <- readRDS(path_processed("aspectemo.rds"))
sadcat    <- readRDS(path_processed("sadcat.rds"))

PLOT_SPACER <- " "   # label of the empty x-axis slot between the two panels
X_LEVELS    <- c(GROUP_LEVELS[1:4], PLOT_SPACER, GROUP_LEVELS[5:8])

add_group <- function(data, dv) {
  data %>%
    filter(!is.na(.data[[dv]])) %>%
    mutate(group = factor(make_group_label(target_gender, stereotype, behaviour),
                          levels = X_LEVELS))
}

#' Bar chart (mean +/- SE) on the full instrument range [y_min, y_max].
make_bar_plot <- function(data, dv, y_label, title, y_min, y_max, y_step, file = NULL) {
  plot_data <- add_group(data, dv)

  margin  <- (y_max - y_min) * 0.10          # room above y_max for panel labels
  label_y <- y_max + margin * 0.5

  p <- ggplot(plot_data, aes(x = group, y = .data[[dv]], fill = group)) +
    stat_summary(fun = mean, geom = "bar", alpha = .85, width = .6) +
    stat_summary(fun.data = mean_se, geom = "errorbar",
                 width = .20, linewidth = .6, colour = "grey30") +
    scale_fill_manual(values = GROUP_COLOURS, drop = TRUE) +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(breaks = seq(y_min, y_max, by = y_step)) +
    coord_cartesian(ylim = c(y_min, y_max + margin), expand = FALSE) +
    # x = 2.5 / 7.5 are the centres of the 4 negative / 4 positive groups
    annotate("text", x = 2.5, y = label_y, label = "Negative behaviours",
             size = 5, colour = "black") +
    annotate("text", x = 7.5, y = label_y, label = "Positive behaviours",
             size = 5, colour = "black") +
    labs(title = title, x = NULL, y = y_label) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position    = "none",
      plot.title         = element_text(face = "bold", size = 13),
      axis.text          = element_text(size = 13, colour = "black"),
      axis.title.y       = element_text(size = 14, colour = "black", face = "plain"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank()
    )

  print(p)
  if (!is.null(file)) ggsave(path_figures(file), p, width = 11, height = 5, dpi = 300, bg = "white")
  invisible(p)
}

# ---- LIWC Tone (range 0-100) ------------------------------------------------
make_bar_plot(
  liwc, "Tone",
  y_label = "Tone (LIWC)",
  title   = "Tone of the description (LIWC) by target gender,\nstereotype type and behaviour valence",
  y_min = 0, y_max = 100, y_step = 20,
  file = "bar_tone_liwc.png"
)

# ---- AspectEmo (range -2 to 2) ----------------------------------------------
make_bar_plot(
  aspectemo, "score_only_charged",
  y_label = "Emotional charge (AspectEmo)",
  title   = "Emotional charge of the description (AspectEmo) by target gender,\nstereotype type and behaviour valence",
  y_min = -2, y_max = 2, y_step = 0.5,
  file = "bar_emotional_charge_aspectemo.png"
)

# ---- SADCAT competence and warmth (range -1 to 1) ---------------------------
make_bar_plot(
  sadcat, "competence",
  y_label = "Competence (SADCAT)",
  title   = "Competence of the description (SADCAT) by target gender,\nstereotype type and behaviour valence",
  y_min = -1, y_max = 1, y_step = 0.25,
  file = "bar_competence_sadcat.png"
)

make_bar_plot(
  sadcat, "warmth",
  y_label = "Warmth (SADCAT)",
  title   = "Warmth of the description (SADCAT) by target gender,\nstereotype type and behaviour valence",
  y_min = -1, y_max = 1, y_step = 0.25,
  file = "bar_warmth_sadcat.png"
)

# ---- LIWC Tone: boxplots of the 8 groups ------------------------------------
box_data <- liwc %>%
  filter(!is.na(Tone)) %>%
  mutate(
    group = factor(make_group_label(target_gender, stereotype, behaviour), levels = GROUP_LEVELS),
    panel = factor(if_else(behaviour == "negative", "Negative behaviours", "Positive behaviours"),
                   levels = c("Negative behaviours", "Positive behaviours"))
  )

p_box <- ggplot(box_data, aes(x = group, y = Tone, fill = group)) +
  geom_boxplot(alpha = .85, width = .55, linewidth = .7, staplewidth = .5,
               outlier.size = .6, outlier.alpha = .25) +
  facet_wrap(~ panel, scales = "free_x") +
  scale_fill_manual(values = GROUP_COLOURS) +
  labs(title = "Tone of the description by target gender,\nstereotype type and behaviour valence",
       x = NULL, y = "Tone (LIWC)") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "none",
    plot.title         = element_text(face = "bold", size = 12),
    axis.text.x        = element_text(size = 8.5, lineheight = .9),
    panel.grid.major.x = element_blank(),
    panel.spacing      = unit(1.4, "lines"),
    strip.text         = element_text(face = "bold", size = 11),
    plot.margin        = margin(t = 10, r = 15, b = 10, l = 10)
  )

print(p_box)
ggsave(path_figures("box_tone_liwc.png"), p_box, width = 13, height = 5.5, dpi = 300, bg = "white")
