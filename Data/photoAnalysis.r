library(tidyverse)
library(stargazer)
library(sandwich)
library(lmtest)

photo <- read.csv("csv/photo_interaction_data.csv", check.names = FALSE)
d     <- read.csv("csv/all_participant_scores.csv",  check.names = FALSE)

photo <- photo %>% rename(
  participant_id    = Participant,
  attempted_pickup  = `Attempted to pick up photo`,
  successful_pickup = `Successfully picked up photo`,
  alerted_character = `Attempted to alert the character to the photo`,
  tried_give_back   = `Tried to physically give the photo back`,
  physically_got_up = `Physically got up`
)

d <- d %>% rename(participant_id = `Participant ID`)

to_binary <- function(x) as.integer(!is.na(x) & x == "\u2713")

photo <- photo %>%
  mutate(across(
    c(attempted_pickup, successful_pickup, alerted_character,
      tried_give_back, physically_got_up),
    to_binary
  ))

d <- d %>% mutate(VR_Enjoyment = replace_na(VR_Enjoyment, 0))

d.photo <- merge(d, photo, by = "participant_id") %>%
  select(-Group.y) %>%
  rename(Group = Group.x)

dir.create("results", showWarnings = FALSE)

photo_vars <- c(
  "attempted_pickup", "successful_pickup", "alerted_character",
  "tried_give_back", "physically_got_up"
)

# ── DESCRIPTIVE PLOT ──────────────────────────────────────────────────────────
d.photo %>%
  pivot_longer(cols = all_of(photo_vars), names_to = "behaviour", values_to = "value") %>%
  group_by(Group, behaviour) %>%
  summarize(
    mean_val = mean(value, na.rm = TRUE),
    sd_val   = sd(value, na.rm = TRUE),
    sem_val  = sd_val / sqrt(n()),
    lci      = mean_val - 1.96 * sem_val,
    uci      = mean_val + 1.96 * sem_val,
    .groups  = "drop"
  ) %>%
  ggplot(aes(Group, mean_val, color = behaviour)) +
  geom_pointrange(aes(ymin = lci, ymax = uci), position = position_dodge(0.4)) +
  facet_wrap(~ behaviour, scales = "free_y") +
  labs(
    title = "Photo Behaviour by Group (proportion +/- 95% CI)",
    y = "Proportion", x = NULL
  )

# ── MODELS ────────────────────────────────────────────────────────────────────
# Model 1: Group only
# Model 2: Group + VR experience/enjoyment
# Model 3: Group + pre-study empathy subscales
# Model 4: Group + VR + pre-study empathy (full)
# HC2 robust SEs via sandwich::vcovHC
# ─────────────────────────────────────────────────────────────────────────────

robust_se <- function(model) sqrt(diag(vcovHC(model, type = "HC2")))

all_models <- list()

for (outcome in photo_vars) {
  f1 <- as.formula(paste(outcome, "~ Group"))
  f2 <- as.formula(paste(outcome, "~ Group + VR_Experience + VR_Enjoyment"))
  f3 <- as.formula(paste(outcome, "~ Group + Pre_PT + Pre_FS + Pre_EC + Pre_PD"))
  f4 <- as.formula(paste(outcome, "~ Group + VR_Experience + VR_Enjoyment + Pre_PT + Pre_FS + Pre_EC + Pre_PD"))

  all_models[[paste0(outcome, "_m1")]] <- lm(f1, data = d.photo)
  all_models[[paste0(outcome, "_m2")]] <- lm(f2, data = d.photo)
  all_models[[paste0(outcome, "_m3")]] <- lm(f3, data = d.photo)
  all_models[[paste0(outcome, "_m4")]] <- lm(f4, data = d.photo)
}

# ── ONE LATEX TABLE PER OUTCOME ───────────────────────────────────────────────

outcome_titles <- c(
  attempted_pickup  = "Attempted to Pick Up Photo",
  successful_pickup = "Successfully Picked Up Photo",
  alerted_character = "Attempted to Alert the Character",
  tried_give_back   = "Tried to Physically Give the Photo Back",
  physically_got_up = "Physically Got Up"
)

for (outcome in photo_vars) {

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
    dep.var.labels   = outcome_titles[[outcome]],
    covariate.labels = c(
      "Group (Test vs Control)",
      "VR Experience", "VR Enjoyment",
      "Pre PT", "Pre FS", "Pre EC", "Pre PD"
    ),
    title = paste0(
      "Linear Probability Models: ", outcome_titles[[outcome]]
    ),
    label        = paste0("tab:lpm_", outcome),
    notes        = "HC2 robust SEs in parentheses. $^{*}$p$<$0.10, $^{**}$p$<$0.05, $^{***}$p$<$0.01.",
    notes.label  = "",
    notes.append = FALSE,
    notes.align  = "l"
  )

  message("Saved: results/lpm_", outcome, ".tex")
}

# ── COMBINED TABLE (all outcomes, all four models) ────────────────────────────

all_model_list <- unlist(
  lapply(photo_vars, function(o) list(
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
  out              = "results/lpm_all_outcomes_combined.tex",
  float            = TRUE,
  table.placement  = "H",
  omit.stat        = c("f", "ser"),
  no.space         = TRUE,
  single.row       = TRUE,
  font.size        = "footnotesize",
  column.sep.width = "1pt",
  column.labels    = rep(c("(1)", "(2)", "(3)", "(4)"), length(photo_vars)),
  column.separate  = rep(4, length(photo_vars)),
  dep.var.labels   = gsub("_", " ", photo_vars),
  covariate.labels = c(
    "Group (Test vs Control)",
    "VR Experience", "VR Enjoyment",
    "Pre PT", "Pre FS", "Pre EC", "Pre PD"
  ),
  title        = "Linear Probability Models: All Photo Behaviours",
  label        = "tab:lpm_all",
  notes        = "HC2 robust SEs in parentheses. $^{*}$p$<$0.10, $^{**}$p$<$0.05, $^{***}$p$<$0.01. Columns (1)--(4): Group only; +VR; +Empathy; Full.",
  notes.label  = "",
  notes.append = FALSE,
  notes.align  = "l"
)

message("Done. Individual tables: results/lpm_<outcome>.tex")
message("Combined table:          results/lpm_all_outcomes_combined.tex")