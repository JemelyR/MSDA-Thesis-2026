library(tidyverse)
library(stargazer)
library(sandwich)
library(lmtest)

# ── DATA ──────────────
d <- read.csv("csv/all_participant_scores.csv", check.names = FALSE)
d <- d %>% rename(participant_id = `Participant ID`)
d <- d %>% mutate(VR_Enjoyment = replace_na(VR_Enjoyment, 0))

themes_raw <- read.csv("csv/themes_by_person.csv", check.names = FALSE)
themes_raw <- themes_raw %>% rename(participant_id = Participant)

theme_cols <- names(themes_raw)[3:8]

to_binary <- function(x) as.integer(!is.na(x) & trimws(x) == "\u2713")

themes_raw <- themes_raw %>%
  mutate(across(all_of(theme_cols), to_binary))

names(themes_raw)[3:8] <- c(
  "T1_NegotiatingConnection",
  "T2_FrustrationInteract",
  "T3_SpecificityItems",
  "T4_CharEmotionalState",
  "T5_Aesthetics",
  "T6_BuildingBackstory"
)

d.themes <- merge(d, themes_raw, by = "participant_id") %>%
  select(-Group.y) %>%
  rename(Group = Group.x)

dir.create("results", showWarnings = FALSE)

robust_se <- function(model) sqrt(diag(vcovHC(model, type = "HC2")))

# ── THEMES ──────────────
theme_vars <- c(
  "T1_NegotiatingConnection",
  "T2_FrustrationInteract",
  "T3_SpecificityItems",
  "T4_CharEmotionalState",
  "T5_Aesthetics",
  "T6_BuildingBackstory"
)

theme_labels <- c(
  "T1: Negotiating Connection",
  "T2: Frustration at Inability to Interact",
  "T3: Specificity in Story Items",
  "T4: Describing Character's Emotional State",
  "T5: Aesthetics of Scene",
  "T6: Building the Backstory"
)

# ── MODELS ──────────────
all_models <- list()

for (outcome in theme_vars) {
  f1 <- as.formula(paste(outcome, "~ Group"))
  f2 <- as.formula(paste(outcome, "~ Group + VR_Experience + VR_Enjoyment"))
  f3 <- as.formula(paste(outcome, "~ Group + Pre_PT + Pre_FS + Pre_EC + Pre_PD"))
  f4 <- as.formula(paste(outcome, "~ Group + VR_Experience + VR_Enjoyment + Pre_PT + Pre_FS + Pre_EC + Pre_PD"))

  all_models[[paste0(outcome, "_m1")]] <- lm(f1, data = d.themes)
  all_models[[paste0(outcome, "_m2")]] <- lm(f2, data = d.themes)
  all_models[[paste0(outcome, "_m3")]] <- lm(f3, data = d.themes)
  all_models[[paste0(outcome, "_m4")]] <- lm(f4, data = d.themes)
}

# ── PER THEME ──────────────
for (i in seq_along(theme_vars)) {
  outcome <- theme_vars[i]
  label   <- theme_labels[i]

  mods <- list(
    all_models[[paste0(outcome, "_m1")]],
    all_models[[paste0(outcome, "_m2")]],
    all_models[[paste0(outcome, "_m3")]],
    all_models[[paste0(outcome, "_m4")]]
  )

  ses <- lapply(mods, robust_se)

  stargazer(
    mods,
    type             = "latex",
    se               = ses,
    out              = paste0("results/lpm_", outcome, ".tex"),
    float            = TRUE,
    table.placement  = "H",
    omit.stat        = c("f", "ser"),
    no.space         = TRUE,
    single.row       = TRUE,
    font.size        = "small",
    column.sep.width = "1pt",
    column.labels    = c("(1) Group", "(2) +VR", "(3) +Empathy", "(4) Full"),
    dep.var.labels   = label,
    covariate.labels = c(
      "Group (Test vs Control)",
      "VR Experience", "VR Enjoyment",
      "Pre PT", "Pre FS", "Pre EC", "Pre PD"
    ),
    title        = paste0("Linear Probability Models: ", label),
    label        = paste0("tab:lpm_", outcome),
    notes        = "HC2 robust SEs in parentheses. $^{*}$p$<$0.10, $^{**}$p$<$0.05, $^{***}$p$<$0.01.",
    notes.label  = "",
    notes.append = FALSE,
    notes.align  = "l"
  )

  message("Saved: results/lpm_", outcome, ".tex")
}

# ── COMBINED TABLE ──────────────
all_model_list <- unlist(
  lapply(theme_vars, function(o) list(
    all_models[[paste0(o, "_m1")]],
    all_models[[paste0(o, "_m2")]],
    all_models[[paste0(o, "_m3")]],
    all_models[[paste0(o, "_m4")]]
  )),
  recursive = FALSE
)

all_ses <- lapply(all_model_list, robust_se)

stargazer(
  all_model_list,
  type             = "latex",
  se               = all_ses,
  out              = "results/lpm_themes_all_combined.tex",
  float            = TRUE,
  table.placement  = "H",
  omit.stat        = c("f", "ser"),
  no.space         = TRUE,
  single.row       = TRUE,
  font.size        = "footnotesize",
  column.sep.width = "1pt",
  column.labels    = rep(c("(1)", "(2)", "(3)", "(4)"), length(theme_vars)),
  column.separate  = rep(4, length(theme_vars)),
  dep.var.labels   = theme_labels,
  covariate.labels = c(
    "Group (Test vs Control)",
    "VR Experience", "VR Enjoyment",
    "Pre PT", "Pre FS", "Pre EC", "Pre PD"
  ),
  title        = "Linear Probability Models: All Thematic Outcomes (OLS with HC2 Robust SEs). Dependent variables are binary indicators of theme presence.",
  label        = "tab:lpm_themes_all",
  notes        = "HC2 robust SEs in parentheses. $^{*}$p$<$0.10, $^{**}$p$<$0.05, $^{***}$p$<$0.01. Columns (1)--(4): Group only; +VR; +Empathy; Full.",
  notes.label  = "",
  notes.append = FALSE,
  notes.align  = "l"
)

# ── GROUP ONLY  ──────────────
group_only_models <- lapply(theme_vars, function(v) all_models[[paste0(v, "_m1")]])
group_only_ses    <- lapply(group_only_models, robust_se)

stargazer(
  group_only_models,
  type             = "latex",
  se               = group_only_ses,
  out              = "results/lpm_themes_group_only.tex",
  float            = TRUE,
  table.placement  = "H",
  omit.stat        = c("f", "ser"),
  no.space         = TRUE,
  single.row       = TRUE,
  font.size        = "small",
  column.sep.width = "1pt",
  column.labels    = theme_labels,
  dep.var.labels   = "Theme Present (0/1)",
  covariate.labels = "Group (Test vs Control)",
  title        = "Effect of Haptic Condition on Theme Presence: Group Only Models",
  label        = "tab:lpm_themes_group_only",
  notes        = "HC2 robust SEs in parentheses. $^{*}$p$<$0.10, $^{**}$p$<$0.05, $^{***}$p$<$0.01.",
  notes.label  = "",
  notes.append = FALSE,
  notes.align  = "l"
)

message("Done — LaTeX tables saved to results/")


# Linear Probability Models estimated by OLS following:
# Angrist, J. D., & Pischke, J. S. (2009). Mostly Harmless Econometrics.
#   Princeton University Press.
# Gomila, R. (2021). Logistic or linear? Estimating causal effects of
#   experimental treatments on binary outcomes using regression analysis.
#   Journal of Experimental Psychology: General, 150(4), 700-709.
# HC2 robust standard errors via sandwich::vcovHC (Zeileis, 2004,
#   Journal of Statistical Software)