library(tidyverse)
library(stargazer)

d <- read.csv("csv/all_participant_scores.csv")

d %>% names()

d <- d %>%
  mutate(VR_Enjoyment = replace_na(VR_Enjoyment, 0))

d.long_post <- d %>% pivot_longer(
  cols = c("Post_Cognitive_Total", "Post_Affective_Total", "Post_Compassion_Total", "Post_Distress"),
  names_to = "post_var",
  values_to = "score"
)

d.long_post %>% group_by(
  Group,
  post_var
) %>%
  summarize(
    mean.score = mean(score),
    sd.score = sd(score),
    sem.score = sd.score / sqrt(n()),
    lci.score = mean.score - (1.96 * sem.score),
    uci.score = mean.score + (1.96 * sem.score)
  ) %>%
  ggplot(aes(Group, mean.score, color = post_var)) +
  geom_pointrange(
    aes(
      ymin = lci.score,
      ymax = uci.score
    ),
    position = "dodge"
  ) + facet_wrap(~ post_var, scales = "free")


# ── EMPATHY ───────────────────────────────────────────

#############
#COMPASSION
#############

model.compassion1 <- d %>% 
  lm(
    Post_Compassion_Total ~ Group,
    data = .
  )

model.compassion2 <- d %>%
  lm(
    Post_Compassion_Total ~ Group * (Pre_EC + Pre_FS),
    data = .
  )

model.compassion3 <- d %>%
  lm(
    Post_Compassion_Total ~ Group * (Pre_PT + Pre_FS + Pre_EC + Pre_PD),
    data = .
  )

# stargazer(
#   model.compassion1,
#   model.compassion2,
#   model.compassion3,
#   type = "html",
#   out = "results/compassion.html"
# )
stargazer(
  model.compassion1, model.compassion2, model.compassion3,
  type = "latex",
  out = "results/compassion.tex",
  float = TRUE,
  table.placement = "H",
  omit.stat = c("f", "ser"),
  no.space = TRUE,
  single.row = TRUE,
  font.size = "small",
  column.sep.width = "1pt",
  title = "Compassion Models",
  dep.var.labels = "Post Compassion Total",
  label = "tab:compassion"
)

############
#AFFECTIVE
############

model.affective1 <- d %>%
  lm(
    Post_Affective_Total ~ Group,
    data = .
  )

model.affective2 <- d %>%
  lm(
    Post_Affective_Total ~ Group * Pre_FS,
    data = .
  )

model.affective3 <- d %>%
  lm(
    Post_Affective_Total ~ Group * (Pre_PT + Pre_FS + Pre_EC + Pre_PD),
    data = .
  )

# stargazer(
#   model.affective1,
#   model.affective2,
#   model.affective3,
#   type = "html",
#   out = "results/affective.html"
# )
stargazer(
  model.affective1, model.affective2, model.affective3,
  type = "latex",
  out = "results/affective.tex",
  float = TRUE,
  table.placement = "H",
  omit.stat = c("f", "ser"),
  no.space = TRUE,
  single.row = TRUE,
  font.size = "small",
  column.sep.width = "1pt",
  title = "Affective Models",
  dep.var.labels = "Post Affective Total",
  label = "tab:affective"
)


############
#COGNITIVE
############

model.cognitive1 <- d %>%
  lm(
    Post_Cognitive_Total ~ Group,
    data = .
  )

model.cognitive2 <- d %>%
  lm(
    Post_Cognitive_Total ~ Group * Pre_PT,
    data = .
  )

model.cognitive3 <- d %>%
  lm(
    Post_Cognitive_Total ~ Group * (Pre_PT + Pre_FS + Pre_EC + Pre_PD),
    data = .
  )

# stargazer(
#   model.cognitive1,
#   model.cognitive2,
#   model.cognitive3,
#   type = "html",
#   out = "results/cognitive.html"
# )
stargazer(
  model.cognitive1,
  model.cognitive2,
  model.cognitive3,
  type = "latex",
  out = "results/cognitive.tex",
  float = TRUE,
  table.placement = "H",
  omit.stat = c("f", "ser"),
  no.space = TRUE,
  single.row = TRUE,
  font.size = "small",
  column.sep.width = "1pt",
  title = "Cognitive Models",
  dep.var.labels = "Post Cognitive Total",
  label = "tab:cognitive"
)
###########
#DISTRESS
###########

model.distress1 <- d %>%
  lm(
    Post_Distress ~ Group,
    data = .
  )

model.distress2 <- d %>%
  lm(
    Post_Distress ~ Group * Pre_PD,
    data = .
  )

model.distress3 <- d %>%
  lm(
    Post_Distress ~ Group * (Pre_PT + Pre_FS + Pre_EC + Pre_PD),
    data = .
  )

# stargazer(
#   model.distress1,
#   model.distress2,
#   model.distress3,
#   type = "html",
#   out = "results/distress.html"
# )
stargazer(
  model.distress1, model.distress2, model.distress3,
  type = "latex",
  out = "results/distress.tex",
  float = TRUE,
  table.placement = "H",
  omit.stat = c("f", "ser"),
  no.space = TRUE,
  single.row = TRUE,
  font.size = "small",
  column.sep.width = "1pt",
  title = "Distress Models",
  dep.var.labels = "Post Distress",
  label = "tab:distress"
)
# ── ENGAGEMENT & IMMERSION ───────────────────────────────────────────

################
#UES ATTENTION
################

model.attention1 <- d %>%
  lm(
    UES_Focused_Attention ~ Group,
    data = .
  )

model.attention2 <- d %>%
  lm(
    UES_Focused_Attention ~ Group * Pre_PT,
    data = .
  )

# stargazer(
#   model.attention1,
#   model.attention2,
#   type = "html",
#   out = "results/focused_attention.html"
# )
stargazer(
  model.attention1, model.attention2,
  type = "latex",
  out = "results/focused_attention.tex",
  float = TRUE,
  table.placement = "H",
  omit.stat = c("f", "ser"),
  no.space = TRUE,
  single.row = TRUE,
  font.size = "small",
  column.sep.width = "1pt",
  title = "Focused Attention Models",
  dep.var.labels = "UES Focused Attention",
  label = "tab:focused_attention"
)


####################
#UES REWARD FACTOR
####################

model.reward1 <- d %>%
  lm(
    UES_Reward_Factor ~ Group,
    data = .
  )

model.reward2 <- d %>%
  lm(
    UES_Reward_Factor ~ Group * (VR_Experience + VR_Enjoyment),
    data = .
  )

# stargazer(
#   model.reward1,
#   model.reward2,
#   type = "html",
#   out = "results/reward_factor.html"
# )
stargazer(
  model.reward1, model.reward2,
  type = "latex",
  out = "results/reward_factor.tex",
  float = TRUE,
  table.placement = "H",
  omit.stat = c("f", "ser"),
  no.space = TRUE,
  single.row = TRUE,
  font.size = "small",
  column.sep.width = "1pt",
  title = "Reward Factor Models",
  dep.var.labels = "UES Reward Factor",
  label = "tab:reward_factor"
)


################
#FELT IMMERSED
################

model.immersed1 <- d %>%
  lm(
    Felt_Immersed ~ Group,
    data = .
  )

model.immersed2 <- d %>%
  lm(
    Felt_Immersed ~ Group * Pre_PD,
    data = .
  )

# stargazer(
#   model.immersed1,
#   model.immersed2,
#   type = "html",
#   out = "results/felt_immersed.html"
# )
stargazer(
  model.immersed1, model.immersed2,
  type = "latex",
  out = "results/felt_immersed.tex",
  float = TRUE,
  table.placement = "H",
  omit.stat = c("f", "ser"),
  no.space = TRUE,
  single.row = TRUE,
  font.size = "small",
  column.sep.width = "1pt",
  title = "Felt Immersed Models",
  dep.var.labels = "Felt Immersed",
  label = "tab:felt_immersed"
)


#######################
#EMOTIONAL CONNECTION
#######################

model.emotional1 <- d %>%
  lm(
    Emotional_Connection ~ Group,
    data = .
  )

model.emotional2 <- d %>%
  lm(
    Emotional_Connection ~ Group * Pre_EC,
    data = .
  )

# stargazer(
#   model.emotional1,
#   model.emotional2,
#   type = "html",
#   out = "results/emotional_connection.html"
# )
stargazer(
  model.emotional1, model.emotional2,
  type = "latex",
  out = "results/emotional_connection.tex",
  float = TRUE,
  table.placement = "H",
  omit.stat = c("f", "ser"),
  no.space = TRUE,
  single.row = TRUE,
  font.size = "small",
  column.sep.width = "1pt",
  title = "Emotional Connection Models",
  dep.var.labels = "Emotional Connection",
  label = "tab:emotional_connection"
)


#########
#OTHERS
#########

model.aesthetic <- d %>%
  lm(
    UES_Aesthetic_Elements ~ Group,
    data = .
  )
model.physical  <- d %>%
  lm(
    Physical_Connection ~ Group,
    data = .
  )
model.others    <- d %>%
  lm(
    Others_Enjoy ~ Group,
    data = .
  )
model.likely    <- d %>%
  lm(
    Likely_Participate ~ Group,
    data = .
  )

# stargazer(
#   model.aesthetic,
#   model.physical,
#   model.others,
#   model.likely,
#   type = "html",
#   # column.labels = c(
#   #   "Aesthetic", "Physical", "Others Enjoy", "Likely Participate"
#   # ),
#   out = "results/engagement_simple.html"
# )
stargazer(
  model.aesthetic, model.physical, model.others, model.likely,
  type = "latex",
  out = "results/engagement_simple.tex",
  float = TRUE,
  table.placement = "H",
  omit.stat = c("f", "ser"),
  no.space = TRUE,
  single.row = TRUE,
  font.size = "small",
  column.sep.width = "1pt",
  title = "Other Engagement Outcomes (Main Effects)",
  dep.var.labels = c(
    "UES Aesthetic Elements", "Physical Connection", "Others Enjoy", "Likely Participate"
  ),
  label = "tab:engagement_simple"
)


# ── VIDEO ANALYSIS ───────────────────────────────────────────

batch <- read.csv("csv/video_batch_summary.csv")

# Merge with participant scores on ID
d.video <- merge(d, batch, by.x = "Participant.ID", by.y = "participant")

#############################
#TOTAL CHARACTER VISIBILITY
############################

model.video.time1 <- d.video %>%
  lm(
    on_screen_time_sec ~ Group,
    data = .
  )

model.video.time2 <- d.video %>%
  lm(
    on_screen_time_sec ~ Group + Pre_PT + Pre_FS + Pre_EC + Pre_PD,
    data = .
  )

# stargazer(
#   model.video.time1,
#   model.video.time2,
#   type = "html",
#   out = "results/video_total_time.html"
# )
stargazer(
  model.video.time1, model.video.time2,
  type = "latex",
  out = "results/video_total_time.tex",
  float = TRUE,
  table.placement = "H",
  omit.stat = c("f", "ser"),
  no.space = TRUE,
  single.row = TRUE,
  font.size = "small",
  column.sep.width = "1pt",
  title = "Video: Total Character Visibility",
  dep.var.labels = "On-screen time (sec)",
  label = "tab:video_total_time"
)

#################################
#SUSTAINED CHARACTER VISIBILITY
################################

model.video.cont1 <- d.video %>%
  lm(
    longest_continuous_sec ~ Group,
    data = .
  )

model.video.cont2 <- d.video %>%
  lm(
    longest_continuous_sec ~ Group + Pre_PT + Pre_FS + Pre_EC + Pre_PD,
    data = .
  )

# stargazer(
#   model.video.cont1,
#   model.video.cont2,
#   type = "html",
#   out = "results/video_longest_continuous.html"
# )
stargazer(
  model.video.cont1, model.video.cont2,
  type = "latex",
  out = "results/video_longest_continuous.tex",
  float = TRUE,
  table.placement = "H",
  omit.stat = c("f", "ser"),
  no.space = TRUE,
  single.row = TRUE,
  font.size = "small",
  column.sep.width = "1pt",
  title = "Video: Longest Continuous Character Visibility",
  dep.var.labels = "Longest continuous (sec)",
  label = "tab:video_longest_continuous"
)
