library(tidyverse)
library(readxl)
library(plotly)

survey_date <- "January 2022"

# ---- Common functions ----
clean_after_loading <- function(d) {
  d %>% 
    rename(Answer = `...1`) %>% 
    filter(!is.na(Answer)) %>%   # keep only percentages
    filter(Answer != "Return to index")
    # rename(London = `London...23`)
}

#' Regional bar plots
#' 
#' @param d Data to plot
#' @param answer_to_label String specifying which of the survey answers to label on the plot (e.g. "Always/a lot")
plot_regional_answers <- function(d, answer_to_label, plot_title, question) {
  
  # Choose colour scale based on the number of possible responses/answers
  if (length(unique(d$Answer)) == 4) {
    bar_palette <- c(
      "#c51b8a",  # Always/a lot
      "#fa9fb5",  # Sometimes
      "#fde0dd",  # Never
      "#bdbdbd"   # Prefer not to say
    )
    
  } else if (length(unique(d$Answer)) == 3) {
    bar_palette <- c(
      "#fdae61",  # Yes
      "#abd9e9",  # No
      "#bdbdbd"   # Prefer not to say
    )
    
  } else if (length(unique(d$Answer)) == 11) {
    bar_palette <- viridis::magma(11)
    
  } else {
    bar_palette <- c(
      viridis::rocket(length(unique(d$Answer)) - 1),
      "#bdbdbd"   # Prefer not to say
    )
  }
  
  # print(length(unique(d$Answer)))
  
  d %>% 
    mutate(
      bar_label = if_else(
        Answer == answer_to_label,
        paste0(round(Percentage, 3) * 100, "%"),
        ""
      )
    ) %>% 
    
    # Arrange bars based on the frequency of the 'worst' answers
    mutate(Percentage_worst = ifelse(Answer == answer_to_label, Percentage, 0)) %>% 
    arrange(desc(Percentage_worst)) %>% 
    mutate(Region = factor(Region, levels = unique(Region))) %>% 
    
    ggplot(aes(x = Region, y = Percentage, fill = Answer)) +
    geom_col(colour = "white") +
    geom_text(aes(label = bar_label), size = 3, position = position_stack(vjust = 0.5)) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = bar_palette) +
    
    theme_classic() +
    labs(
      x = "",
      y = "Percentage of respondents",
      fill = "",
      title = plot_title,
      subtitle = paste0("Survey question: ", question),
      caption = paste0("Source: British Red Cross / Opinium survey, ", survey_date)
    ) +
    theme(
      # plot.caption = element_text(hjust = 0), #Default is hjust=1
      plot.title.position = "plot",
      plot.caption.position =  "plot", 
      legend.position = "top"
    )
}

# ---- Adverse experiences ----
adverse <- read_excel("data/OP18518 British Red Cross Understanding Vulnerability.xlsx", skip = 2, sheet = "OP18518_Q1")

adverse_regional <- 
  adverse %>% 
  clean_after_loading() %>% 
  slice(-1) %>% 
  
  select(
    Answer, 
    `North East`:`Northern Ireland`
    # `North East`:`Northern Ireland`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage")

plt_adverse <- 
  adverse_regional %>% 
  ggplot(aes(x = Answer, y = Percentage, fill = Region)) +
  geom_col(position = "dodge") +
  coord_flip() +
  
  scale_y_continuous(labels = scales::percent) +
  
  theme_classic() +
  labs(
    x = "",
    y = "Percentage of respondents",
    fill = "",
    title = "Adverse events",
    subtitle = "Survey question: In the last three months, have you experienced any of the following?"
    # caption = "Source: Opinium survey"
  ) +
  theme(
    # plot.caption = element_text(hjust = 0), #Default is hjust=1
    plot.title.position = "plot",
    plot.caption.position =  "plot", 
    legend.position = "top"
  )

ggplotly(plt_adverse)

# ---- Received money ----
money <- read_excel("data/OP18518 British Red Cross Understanding Vulnerability.xlsx", skip = 2, sheet = "OP18518_Q2_Merged")

money_regional <- 
  money %>% 
  clean_after_loading() %>% 
  slice(-1) %>% 
  
  select(
    Answer, 
    `North East`:`Northern Ireland`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage")

plt_money <- 
  money_regional %>% 
  ggplot(aes(x = Answer, y = Percentage, fill = Region)) +
  geom_col(position = "dodge") +
  coord_flip() +
  
  scale_y_continuous(labels = scales::percent) +
  
  theme_classic() +
  labs(
    x = "",
    y = "Percentage of respondents",
    fill = "",
    title = "Received money",
    subtitle = "Survey question: In the last three months, have you received money from any of the following?"
    # caption = "Source: Opinium survey"
  ) +
  theme(
    # plot.caption = element_text(hjust = 0), #Default is hjust=1
    plot.title.position = "plot",
    plot.caption.position =  "plot", 
    legend.position = "top"
  )

ggplotly(plt_money)

# ---- Day-to-day activities limited ----
activities <- read_excel("data/OP18518 British Red Cross Understanding Vulnerability.xlsx", skip = 2, sheet = "OP18518_Q3")

activities_regional <- 
  activities %>% 
  clean_after_loading() %>% 
  slice(-1) %>% 
  
  select(
    Answer, 
    `North East`:`Northern Ireland`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  filter(Answer != "NET Yes") %>% 
  mutate(Answer = factor(
    Answer,
    levels = rev(c("No", "Yes, limited a little", "Yes, limited a lot"))
  ))

(plt_activities <- 
    plot_regional_answers(
      activities_regional,
      answer_to_label = "Yes, limited a lot", 
      plot_title = "Daily activities limited", 
      question = "Are your day-to-day activities limited because of a health problem or disability which has lasted, or is expected to last, at least 12 months?"
    ))

ggsave(
  plot = plt_activities,
  filename = "output/regional/2022-01/activities-limited.png",
  height = 150, width = 150, units = "mm"
)

# ---- Mobility ----
mobility <- read_excel("data/OP18518 British Red Cross Understanding Vulnerability.xlsx", skip = 2, sheet = "OP18518_Q4")

mobility_regional <- 
  mobility %>% 
  clean_after_loading() %>% 
  slice(-1) %>% 
  
  select(
    Answer, 
    `North East`:`Northern Ireland`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  mutate(
    Answer = case_when(
      Answer %in% c("I have slight problems in walking about", "I have moderate problems in walking about") ~ "NET: Slight/moderate problems",
      Answer %in% c("I have severe problems in walking about", "I have extreme problems in walking about") ~ "NET: Severe/extreme problems",
      TRUE ~ Answer
    )
  ) %>% 
  
  group_by(Region, Answer) %>% 
  summarise(Percentage = sum(Percentage)) %>% 
  ungroup() %>% 
  
  filter(
    Answer %in% c("NET: Slight/moderate problems", "NET: Severe/extreme problems", "I have no problems in walking about", "Prefer not to say")
  ) %>% 
  
  mutate(Answer = str_remove(Answer, "NET: ")) %>% 
  
  mutate(Answer = if_else(Answer == "I have no problems in walking about", "No problems", str_remove(Answer, "NET: "))) %>% 
  
  mutate(Answer = factor(
    Answer,
    levels = rev(c("Prefer not to say", "No problems", "Slight/moderate problems", "Severe/extreme problems"))
  )) %>% 
  
  relocate(Region, Answer, Percentage)

(plt_mobility <- 
    plot_regional_answers(
      mobility_regional,
      answer_to_label = "Severe/extreme problems", 
      plot_title = "Mobility", 
      question = "Please tick the response that best describes your health today regarding mobility."
    ))

ggsave(
  plot = plt_mobility,
  filename = "output/regional/2022-01/mobility.png",
  height = 150, width = 150, units = "mm"
)

# ---- Usual activities ----
usual_activities <- read_excel("data/OP18518 British Red Cross Understanding Vulnerability.xlsx", skip = 2, sheet = "OP18518_Q5")

usual_activities_regional <- 
  usual_activities %>% 
  clean_after_loading() %>% 
  slice(-1) %>% 
  
  select(
    Answer, 
    `North East`:`Northern Ireland`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  mutate(
    Answer = case_when(
      Answer %in% c("I have slight problems doing my usual activities", "I have moderate problems doing my usual activities") ~ "NET: Slight/moderate problems",
      Answer %in% c("I have severe problems doing my usual activities", "I have extreme problems doing my usual activities") ~ "NET: Severe/extreme problems",
      TRUE ~ Answer
    )
  ) %>% 
  
  group_by(Region, Answer) %>% 
  summarise(Percentage = sum(Percentage)) %>% 
  ungroup() %>% 
  
  filter(
    Answer %in% c("NET: Slight/moderate problems", "NET: Severe/extreme problems", "I have no problems doing my usual activities", "Prefer not to say")
  ) %>% 
  
  mutate(Answer = if_else(Answer == "I have no problems doing my usual activities", "No problems", str_remove(Answer, "NET: "))) %>% 
  
  mutate(Answer = factor(
    Answer,
    levels = rev(c("Prefer not to say", "No problems", "Slight/moderate problems", "Severe/extreme problems"))
  )) %>% 
  
  relocate(Region, Answer, Percentage)

(plt_usual_activities <- 
    plot_regional_answers(
      usual_activities_regional,
      answer_to_label = "Severe/extreme problems", 
      plot_title = "Usual activities", 
      question = "Please tick the response that best describes your health today regarding usual activities (e.g. work, study, housework, family or leisure activities)."
    ))

ggsave(
  plot = plt_usual_activities,
  filename = "output/regional/2022-01/usual-activities.png",
  height = 150, width = 150, units = "mm"
)

# ---- Life satisfaction ----
satisfaction <- read_excel("data/OP18518 British Red Cross Understanding Vulnerability.xlsx", skip = 2, sheet = "OP18518_Q6")

satisfaction_regional <- 
  satisfaction %>% 
  clean_after_loading() %>% 
  filter(!Answer %in% c("Base: all respondents", "Average")) %>% 
  
  select(
    Answer, 
    `North East`:`Northern Ireland`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  mutate(Answer = str_remove(Answer, "\\[[0-9]+\\]\\s")) %>% 
  mutate(Answer = factor(Answer)) %>% 
  mutate(Answer = fct_relevel(Answer, "10 – Completely", after = Inf)) %>% 
  
  relocate(Region, Answer, Percentage)

(plt_satisfaction <- 
    plot_regional_answers(
      satisfaction_regional,
      answer_to_label = "0 – Not at all", 
      plot_title = "Life satisfaction", 
      question = "How satisfied are you with your life nowadays?"
    ))

ggsave(
  plot = plt_satisfaction,
  filename = "output/regional/2022-01/satisfaction.png",
  height = 150, width = 150, units = "mm"
)

# ---- Worthwhile ----
worthwhile <- read_excel("data/OP18518 British Red Cross Understanding Vulnerability.xlsx", skip = 2, sheet = "OP18518_Q6 (2)")

worthwhile_regional <- 
  worthwhile %>% 
  clean_after_loading() %>% 
  filter(!Answer %in% c("Base: all respondents", "Average")) %>% 
  
  select(
    Answer, 
    `North East`:`Northern Ireland`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  mutate(Answer = str_remove(Answer, "\\[[0-9]+\\]\\s")) %>% 
  mutate(Answer = factor(Answer)) %>% 
  mutate(Answer = fct_relevel(Answer, "10 – Completely", after = Inf)) %>% 
  
  relocate(Region, Answer, Percentage)

(plt_worthwhile <- 
    plot_regional_answers(
      worthwhile_regional,
      answer_to_label = "0 – Not at all", 
      plot_title = "Worthwhile", 
      question = "To what extent do you feel the things you do in your life are worthwhile?"
    ))

ggsave(
  plot = plt_worthwhile,
  filename = "output/regional/2022-01/worthwhile.png",
  height = 150, width = 150, units = "mm"
)

# ---- Happiness ----
happiness <- read_excel("data/OP18518 British Red Cross Understanding Vulnerability.xlsx", skip = 2, sheet = "OP18518_Q6 (3)")

happiness_regional <- 
  happiness %>% 
  clean_after_loading() %>% 
  filter(!Answer %in% c("Base: all respondents", "Average")) %>% 
  
  select(
    Answer, 
    `North East`:`Northern Ireland`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  mutate(Answer = str_remove(Answer, "\\[[0-9]+\\]\\s")) %>% 
  mutate(Answer = factor(Answer)) %>% 
  mutate(Answer = fct_relevel(Answer, "10 – Completely", after = Inf)) %>% 
  
  relocate(Region, Answer, Percentage)

(plt_happiness <- 
    plot_regional_answers(
      happiness_regional,
      answer_to_label = "0 – Not at all", 
      plot_title = "Happiness", 
      question = "How happy did you feel yesterday?"
    ))

ggsave(
  plot = plt_happiness,
  filename = "output/regional/2022-01/happiness.png",
  height = 150, width = 150, units = "mm"
)

# ---- Anxious ----
anxious <- read_excel("data/OP18518 British Red Cross Understanding Vulnerability.xlsx", skip = 2, sheet = "OP18518_Q6 (4)")

anxious_regional <- 
  anxious %>% 
  clean_after_loading() %>% 
  filter(!Answer %in% c("Base: all respondents", "Average")) %>% 
  
  select(
    Answer, 
    `North East`:`Northern Ireland`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  mutate(Answer = str_remove(Answer, "\\[[0-9]+\\]\\s")) %>% 
  mutate(Answer = factor(Answer)) %>% 
  mutate(Answer = fct_relevel(Answer, "10 – Completely", after = Inf)) %>% 
  mutate(Answer = fct_rev(Answer)) %>% 
  
  relocate(Region, Answer, Percentage)

(plt_anxious <- 
    plot_regional_answers(
      anxious_regional,
      answer_to_label = "10 – Completely", 
      plot_title = "Anxiety", 
      question = "How anxious did you feel yesterday?"
    ))

ggsave(
  plot = plt_anxious,
  filename = "output/regional/2022-01/anxious.png",
  height = 150, width = 150, units = "mm"
)

# ---- No interest/pleasure ----
interest <- read_excel("data/OP18518 British Red Cross Understanding Vulnerability.xlsx", skip = 2, sheet = "OP18518_Q7")

interest_regional <- 
  interest %>% 
  clean_after_loading() %>% 
  filter(!Answer %in% c("Base: all respondents", "NET Bothered in the past two weeks")) %>% 
  
  select(
    Answer, 
    `North East`:`Northern Ireland`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  mutate(
    Answer = factor(
      Answer,
      levels = c("Nearly every day", "More than half the days", "Several days", "Not at all", "Prefer not to say")
    )
  ) %>% 
  
  relocate(Region, Answer, Percentage)

(plt_interest <- 
    plot_regional_answers(
      interest_regional,
      answer_to_label = "Nearly every day", 
      plot_title = "Bothered by no interest/pleasure", 
      question = "Over the last two weeks, how often, if at all, have you been bothered by not having interest or pleasure in doing things?"
    ))

ggsave(
  plot = plt_interest,
  filename = "output/regional/2022-01/interest.png",
  height = 150, width = 150, units = "mm"
)

# - Summarise -
plt_interest_summary <- 
  interest_regional %>% 
  mutate(
    Answer = fct_collapse(Answer, "Bothered in the past two weeks" = c("Nearly every day", "More than half the days"))
  ) %>% 
  
  group_by(Region, Answer) %>% 
  summarise(Percentage = sum(Percentage)) %>% 
  ungroup() %>% 
  
  plot_regional_answers(
    answer_to_label = "Bothered in the past two weeks", 
    plot_title = "Bothered by no interest/pleasure", 
    question = "Over the last two weeks, how often, if at all, have you been bothered by not having interest or pleasure in doing things?"
  )

ggsave(
  plot = plt_interest_summary,
  filename = "output/regional/2022-01/interest-summary.png",
  height = 150, width = 150, units = "mm"
)

# ---- Down/depressed ----
down <- read_excel("data/OP18518 British Red Cross Understanding Vulnerability.xlsx", skip = 2, sheet = "OP18518_Q8")

down_regional <- 
  down %>% 
  clean_after_loading() %>% 
  filter(!Answer %in% c("Base: all respondents", "NET Bothered in the past two weeks")) %>% 
  
  select(
    Answer, 
    `North East`:`Northern Ireland`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  mutate(
    Answer = factor(
      Answer,
      levels = c("Nearly every day", "More than half the days", "Several days", "Not at all", "Prefer not to say")
    )
  ) %>% 
  
  relocate(Region, Answer, Percentage)

(plt_down <- 
    plot_regional_answers(
      down_regional,
      answer_to_label = "Nearly every day", 
      plot_title = NULL,  #"Down/depressed", 
      question = "Over the last two weeks, how often have you been bothered by feeling down, depressed, or hopeless?"
    ))

ggsave(
  plot = plt_down,
  filename = "output/regional/2022-01/down.png",
  height = 150, width = 220, units = "mm"
)

# - Summarise -
plt_down_summary <- 
  down_regional %>% 
  mutate(
    Answer = fct_collapse(Answer, "Bothered in the past two weeks" = c("Nearly every day", "More than half the days"))
  ) %>% 
  
  group_by(Region, Answer) %>% 
  summarise(Percentage = sum(Percentage)) %>% 
  ungroup() %>% 
  
  plot_regional_answers(
    answer_to_label = "Bothered in the past two weeks", 
    plot_title = "Down, depressed, helpless", 
    question = "Over the last two weeks, how often have you been bothered by feeling down, depressed, or hopeless?"
  )

ggsave(
  plot = plt_down_summary,
  filename = "output/regional/2022-01/down-summary.png",
  height = 150, width = 150, units = "mm"
)

# ---- Received mental health support ----
mh_received <- read_excel("data/OP18518 British Red Cross Understanding Vulnerability.xlsx", skip = 2, sheet = "OP18518_Q9")

mh_received_regional <- 
  mh_received %>% 
  clean_after_loading() %>% 
  slice(-1) %>% 
  
  select(
    Answer, 
    `North East`:`Northern Ireland`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  mutate(Answer = factor(
    Answer,
    levels = rev(c("Prefer not to say", "No", "Yes"))
  ))

(plt_mh_rec <- 
    plot_regional_answers(
      mh_received_regional,
      answer_to_label = "Yes", 
      plot_title = "Received MH support", 
      question = "Have you received support for your mental health in the past three months?"
    ))

ggsave(
  plot = plt_mh_rec,
  filename = "output/regional/2022-01/mental-health-support-received.png",
  height = 150, width = 150, units = "mm"
)

# ---- Awaiting mental health support ----
mh_awaiting <- read_excel("data/OP18518 British Red Cross Understanding Vulnerability.xlsx", skip = 2, sheet = "OP18518_Q10")

mh_awaiting <- 
  mh_awaiting %>% 
  clean_after_loading() %>% 
  slice(-1)

mh_awaiting_regional <- 
  mh_awaiting %>% 
  select(
    Answer, 
    `North East`:`Northern Ireland`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  mutate(Answer = factor(
    Answer,
    levels = rev(c("Prefer not to say", "No", "Yes"))
  ))

(plt_mh_wait <- 
    plot_regional_answers(
      mh_awaiting_regional,
      answer_to_label = "Yes", 
      plot_title = "Awaiting MH support", 
      question = "Are you currently waiting for support with your mental health?"
    ))

ggsave(
  plot = plt_mh_wait,
  filename = "output/regional/2022-01/mental-health-support-waiting.png",
  height = 150, width = 150, units = "mm"
)

# ---- Loneliness ----
lonely <- read_excel("data/OP18518 British Red Cross Understanding Vulnerability.xlsx", skip = 2, sheet = "OP18518_Q11")

lonely <- 
  lonely %>% 
  clean_after_loading()

lonely_regional <- 
  lonely %>% 
  select(
    Answer, 
    `North East`:`Northern Ireland`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  mutate(
    Answer = case_when(
      Answer %in% c("Always", "A lot of the time") ~ "Always/a lot",
      Answer %in% c("Some of the time", "Hardly ever") ~ "Sometimes/hardly ever",
      TRUE ~ Answer
    )
  ) %>% 
  
  filter(
    Answer %in% c("Always/a lot", "Sometimes/hardly ever", "Never", "Prefer not to say")
  ) %>%
  
  group_by(Region, Answer) %>% 
  summarise(Percentage = sum(Percentage)) %>% 
  ungroup() %>% 
  
  mutate(Answer = factor(
    Answer,
    levels = rev(c("Prefer not to say", "Never", "Sometimes/hardly ever", "Always/a lot"))
  )) %>% 
  
  relocate(Region, Answer, Percentage)

(plt_lonely <- 
    plot_regional_answers(
      lonely_regional,
      answer_to_label = "Always/a lot", 
      plot_title = "Loneliness", 
      question = "How often, if at all, do you feel lonely?"
    ))

ggsave(
  plot = plt_lonely,
  filename = "output/regional/2022-01/loneliness.png",
  height = 150, width = 150, units = "mm"
)
