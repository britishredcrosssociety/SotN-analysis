library(tidyverse)
library(readxl)
library(plotly)

survey_date <- "July 2021"

# ---- Common functions ----
clean_after_loading <- function(d) {
  d %>% 
    rename(Answer = `...1`) %>% 
    filter(!is.na(Answer)) %>%   # keep only percentages
    filter(Answer != "Return to index") |> 
    rename(London = `London...23`)
}

#' Regional bar plots
#' 
#' @param d Data to plot
#' @param answer_to_label String specifying which of the survey answers to label on the plot (e.g. "Always/a lot")
plot_answers <- function(d, answer_to_label, plot_title, question) {
  
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
    mutate(SES = factor(SES, levels = unique(SES))) %>% 
    
    ggplot(aes(x = SES, y = Percentage, fill = Answer)) +
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
adverse <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q3")

adverse_regional <- 
  adverse %>% 
  clean_after_loading() %>% 
  slice(-1) %>% 
  
  select(
    Answer, 
    `ABC1`:`C2DE`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "SES", values_to = "Percentage")

plt_adverse <- 
  adverse_regional %>% 
  ggplot(aes(x = Answer, y = Percentage, fill = SES)) +
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
money <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q4")

money_regional <- 
  money %>% 
  clean_after_loading() %>% 
  slice(-1) %>% 
  
  select(
    Answer, 
    # `ABC1`:`C2DE`
    `Upper/middle class` = `ABC1`,
    `Working class and non-working` = `C2DE`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "SES", values_to = "Percentage")

plt_money <- 
  money_regional %>% 
  ggplot(aes(x = Answer, y = Percentage, fill = SES)) +
  geom_col(position = "dodge") +
  coord_flip() +
  
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#af8dc3", "#7fbf7b")) +
  
  theme_classic() +
  labs(
    x = "",
    y = "Percentage of respondents",
    fill = "Social grade, based on occupation:",
    title = "People on lower incomes reported receiving less social support",
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
ggsave(plot = plt_money, file = "output/socioeconomic-status/support received by ses - money.png", width = 250, height = 150, units = "mm")

# ---- Received non-cash items ----
support <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q5")

support_regional <- 
  support %>% 
  clean_after_loading() %>% 
  slice(-1) %>% 
  
  select(
    Answer, 
    `Upper/middle class` = `ABC1`,
    `Working class and non-working` = `C2DE`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "SES", values_to = "Percentage")

plt_support <- 
  support_regional %>% 
  ggplot(aes(x = Answer, y = Percentage, fill = SES)) +
  geom_col(position = "dodge") +
  coord_flip() +
  
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#af8dc3", "#7fbf7b")) +
  
  theme_classic() +
  labs(
    x = "",
    y = "Percentage of respondents",
    fill = "Social grade, based on occupation:",
    title = "People on lower incomes reported receiving less social support",
    subtitle = "Survey question: In the last three months, have you received help getting non-cash items such as food, clothing, toiletries, \nprepaid cards for utilities such as energy, or other items from the following?",
    caption = paste0("Source: British Red Cross / Opinium survey, ", survey_date)
  ) +
  theme(
    # plot.caption = element_text(hjust = 0), #Default is hjust=1
    plot.title.position = "plot",
    plot.caption.position =  "plot", 
    legend.position = "top"
  )

ggplotly(plt_support)
ggsave(plot = plt_money, file = "output/socioeconomic-status/support received by ses - other.png", width = 250, height = 150, units = "mm")

# ---- Day-to-day activities limited ----
activities <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q6")

activities_regional <- 
  activities %>% 
  clean_after_loading() %>% 
  slice(-1) %>% 
  
  select(
    Answer, 
    `Upper/middle class` = `ABC1`,
    `Working class and non-working` = `C2DE`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "SES", values_to = "Percentage") %>% 
  
  filter(Answer != "NET: Yes") %>% 
  mutate(Answer = factor(
    Answer,
    levels = rev(c("No", "Yes, limited a little", "Yes, limited a lot"))
  ))

(plt_activities <- 
    plot_answers(
      activities_regional,
      answer_to_label = "Yes, limited a lot", 
      plot_title = "Daily activities limited", 
      question = "Are your day-to-day activities limited because of a health problem or disability which has lasted, or is expected to last, at least 12 months?"
    ))

ggsave(
  plot = plt_activities,
  filename = "output/socioeconomic-status/activities-limited.png",
  height = 150, width = 150, units = "mm"
)

# ---- Mobility ----
mobility <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q7")

mobility_regional <- 
  mobility %>% 
  clean_after_loading() %>% 
  slice(-1) %>% 
  
  select(
    Answer, 
    `Upper/middle class` = `ABC1`,
    `Working class and non-working` = `C2DE`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "SES", values_to = "Percentage") %>% 
  
  filter(
    Answer %in% c("NET: Slight/moderate problems", "NET: Severe/extreme problems", "I have no problems in walking about", "Prefer not to say")
  ) %>% 
  
  mutate(Answer = str_remove(Answer, "NET: ")) %>% 
  
  mutate(Answer = if_else(Answer == "I have no problems in walking about", "No problems", str_remove(Answer, "NET: "))) %>% 
  
  mutate(Answer = factor(
    Answer,
    levels = rev(c("Prefer not to say", "No problems", "Slight/moderate problems", "Severe/extreme problems"))
  )) %>% 
  
  relocate(SES, Answer, Percentage)

(plt_mobility <- 
    plot_answers(
      mobility_regional,
      answer_to_label = "Severe/extreme problems", 
      plot_title = "Mobility", 
      question = "Please tick the response that best describes your health today regarding mobility."
    ))

ggsave(
  plot = plt_mobility,
  filename = "output/socioeconomic-status/mobility.png",
  height = 150, width = 150, units = "mm"
)

# ---- Self-care ----
self_care <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q8")

self_care_regional <- 
  self_care %>% 
  clean_after_loading() %>% 
  slice(-1) %>% 
  
  select(
    Answer, 
    `Upper/middle class` = `ABC1`,
    `Working class and non-working` = `C2DE`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "SES", values_to = "Percentage") %>% 
  
  filter(
    Answer %in% c("NET: Slight/moderate problems", "NET: Severe/extreme problems", "I have no problems washing or dressing myself", "Prefer not to say")
  ) %>% 
  
  mutate(Answer = if_else(Answer == "I have no problems washing or dressing myself", "No problems", str_remove(Answer, "NET: "))) %>% 
  
  mutate(Answer = factor(
    Answer,
    levels = rev(c("Prefer not to say", "No problems", "Slight/moderate problems", "Severe/extreme problems"))
  )) %>% 
  
  relocate(SES, Answer, Percentage)

(plt_self_care <- 
    plot_answers(
      self_care_regional,
      answer_to_label = "Severe/extreme problems", 
      plot_title = "Self-care", 
      question = "Please tick the response that best describes your health today regarding self-care."
    ))

ggsave(
  plot = plt_self_care,
  filename = "output/socioeconomic-status/self-care.png",
  height = 150, width = 150, units = "mm"
)

# ---- Usual activities ----
usual_activities <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q9")

usual_activities_regional <- 
  usual_activities %>% 
  clean_after_loading() %>% 
  slice(-1) %>% 
  
  select(
    Answer, 
    `Upper/middle class` = `ABC1`,
    `Working class and non-working` = `C2DE`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "SES", values_to = "Percentage") %>% 
  
  filter(
    Answer %in% c("NET: Slight/moderate problems", "NET: Severe/extreme problems", "I have no problems doing my usual activities", "Prefer not to say")
  ) %>% 
  
  mutate(Answer = if_else(Answer == "I have no problems doing my usual activities", "No problems", str_remove(Answer, "NET: "))) %>% 
  
  mutate(Answer = factor(
    Answer,
    levels = rev(c("Prefer not to say", "No problems", "Slight/moderate problems", "Severe/extreme problems"))
  )) %>% 
  
  relocate(SES, Answer, Percentage)

(plt_usual_activities <- 
    plot_answers(
      usual_activities_regional,
      answer_to_label = "Severe/extreme problems", 
      plot_title = "Usual activities", 
      question = "Please tick the response that best describes your health today regarding usual activities (e.g. work, study, housework, family or leisure activities)."
    ))

ggsave(
  plot = plt_usual_activities,
  filename = "output/socioeconomic-status/usual-activities.png",
  height = 150, width = 150, units = "mm"
)

# ---- Pain/discomfort ----
pain <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q10")

pain_regional <- 
  pain %>% 
  clean_after_loading() %>% 
  slice(-1) %>% 
  
  select(
    Answer, 
    `Upper/middle class` = `ABC1`,
    `Working class and non-working` = `C2DE`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "SES", values_to = "Percentage") %>% 
  
  filter(
    Answer %in% c("NET: Slight/moderate pain or discomfort", "NET: Severe/extreme pain or discomfort", "I have no pain or discomfort", "Prefer not to say")
  ) %>% 
  
  mutate(Answer = if_else(Answer == "I have no pain or discomfort", "No pain/discomfort", str_remove(Answer, "NET: "))) %>% 
  
  mutate(Answer = factor(
    Answer,
    levels = rev(c("Prefer not to say", "No pain/discomfort", "Slight/moderate pain or discomfort", "Severe/extreme pain or discomfort"))
  )) %>% 
  
  relocate(SES, Answer, Percentage)

(plt_pain <- 
    plot_answers(
      pain_regional,
      answer_to_label = "Severe/extreme pain or discomfort", 
      plot_title = "Pain/discomfort", 
      question = "Please tick the response that best describes your health today regarding pain/discomfort."
    ))

ggsave(
  plot = plt_pain,
  filename = "output/socioeconomic-status/pain.png",
  height = 150, width = 150, units = "mm"
)

# ---- Life satisfaction ----
satisfaction <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q11")

satisfaction_regional <- 
  satisfaction %>% 
  clean_after_loading() %>% 
  filter(!Answer %in% c("Base: all respondents", "Average")) %>% 
  
  select(
    Answer, 
    `Upper/middle class` = `ABC1`,
    `Working class and non-working` = `C2DE`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "SES", values_to = "Percentage") %>% 
  
  mutate(Answer = str_remove(Answer, "\\[[0-9]+\\]\\s")) %>% 
  mutate(Answer = factor(Answer)) %>% 
  mutate(Answer = fct_relevel(Answer, "10 – Completely", after = Inf)) %>% 
  
  relocate(SES, Answer, Percentage)

(plt_satisfaction <- 
    plot_answers(
      satisfaction_regional,
      answer_to_label = "0 – Not at all", 
      plot_title = "Life satisfaction", 
      question = "How satisfied are you with your life nowadays?"
    ))

ggsave(
  plot = plt_satisfaction,
  filename = "output/socioeconomic-status/satisfaction.png",
  height = 150, width = 150, units = "mm"
)

# ---- Worthwhile ----
worthwhile <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q11 (2)")

worthwhile_regional <- 
  worthwhile %>% 
  clean_after_loading() %>% 
  filter(!Answer %in% c("Base: all respondents", "Average")) %>% 
  
  select(
    Answer, 
    `Upper/middle class` = `ABC1`,
    `Working class and non-working` = `C2DE`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "SES", values_to = "Percentage") %>% 
  
  mutate(Answer = str_remove(Answer, "\\[[0-9]+\\]\\s")) %>% 
  mutate(Answer = factor(Answer)) %>% 
  mutate(Answer = fct_relevel(Answer, "10 – Completely", after = Inf)) %>% 
  
  relocate(SES, Answer, Percentage)

(plt_worthwhile <- 
    plot_answers(
      worthwhile_regional,
      answer_to_label = "0 – Not at all", 
      plot_title = "Worthwhile", 
      question = "To what extent do you feel the things you do in your life are worthwhile?"
    ))

ggsave(
  plot = plt_worthwhile,
  filename = "output/socioeconomic-status/worthwhile.png",
  height = 150, width = 150, units = "mm"
)

# ---- Happiness ----
happiness <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q11 (3)")

happiness_regional <- 
  happiness %>% 
  clean_after_loading() %>% 
  filter(!Answer %in% c("Base: all respondents", "Average")) %>% 
  
  select(
    Answer, 
    `Upper/middle class` = `ABC1`,
    `Working class and non-working` = `C2DE`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "SES", values_to = "Percentage") %>% 
  
  mutate(Answer = str_remove(Answer, "\\[[0-9]+\\]\\s")) %>% 
  mutate(Answer = factor(Answer)) %>% 
  mutate(Answer = fct_relevel(Answer, "10 – Completely", after = Inf)) %>% 
  
  relocate(SES, Answer, Percentage)

(plt_happiness <- 
    plot_answers(
      happiness_regional,
      answer_to_label = "0 – Not at all", 
      plot_title = "Happiness", 
      question = "How happy did you feel yesterday?"
    ))

ggsave(
  plot = plt_happiness,
  filename = "output/socioeconomic-status/happiness.png",
  height = 150, width = 150, units = "mm"
)

# ---- Anxious ----
anxious <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q11 (4)")

anxious_regional <- 
  anxious %>% 
  clean_after_loading() %>% 
  filter(!Answer %in% c("Base: all respondents", "Average")) %>% 
  
  select(
    Answer, 
    `Upper/middle class` = `ABC1`,
    `Working class and non-working` = `C2DE`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "SES", values_to = "Percentage") %>% 
  
  mutate(Answer = str_remove(Answer, "\\[[0-9]+\\]\\s")) %>% 
  mutate(Answer = factor(Answer)) %>% 
  mutate(Answer = fct_relevel(Answer, "10 – Completely", after = Inf)) %>% 
  
  relocate(SES, Answer, Percentage)

(plt_anxious <- 
    plot_answers(
      anxious_regional,
      answer_to_label = "0 – Not at all", 
      plot_title = "Anxiety", 
      question = "How anxious did you feel yesterday?"
    ))

ggsave(
  plot = plt_anxious,
  filename = "output/socioeconomic-status/anxious.png",
  height = 150, width = 150, units = "mm"
)

# ---- No interest/pleasure ----
interest <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q12")

interest_regional <- 
  interest %>% 
  clean_after_loading() %>% 
  filter(!Answer %in% c("Base: all respondents", "NET: Has been bothered")) %>% 
  
  select(
    Answer, 
    `Upper/middle class` = `ABC1`,
    `Working class and non-working` = `C2DE`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "SES", values_to = "Percentage") %>% 
  
  mutate(
    Answer = factor(
      Answer,
      levels = c("Nearly every day", "More than half the days", "Several days", "Not at all", "Prefer not to say")
    )
  ) %>% 
  
  relocate(SES, Answer, Percentage)

(plt_interest <- 
    plot_answers(
      interest_regional,
      answer_to_label = "Nearly every day", 
      plot_title = "Bothered by no interest/pleasure", 
      question = "Over the last two weeks, how often, if at all, have you been bothered by not having interest or pleasure in doing things?"
    ))

ggsave(
  plot = plt_interest,
  filename = "output/socioeconomic-status/interest.png",
  height = 150, width = 150, units = "mm"
)

# ---- Down/depressed ----
down <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q13")

down_regional <- 
  down %>% 
  clean_after_loading() %>% 
  filter(!Answer %in% c("Base: all respondents", "NET: Has been bothered")) %>% 
  
  select(
    Answer, 
    `Upper/middle class` = `ABC1`,
    `Working class and non-working` = `C2DE`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "SES", values_to = "Percentage") %>% 
  
  mutate(
    Answer = factor(
      Answer,
      levels = c("Nearly every day", "More than half the days", "Several days", "Not at all", "Prefer not to say")
    )
  ) %>% 
  
  relocate(SES, Answer, Percentage)

(plt_down <- 
    plot_answers(
      down_regional,
      answer_to_label = "Nearly every day", 
      plot_title = NULL,  #"Down/depressed", 
      question = "Over the last two weeks, how often have you been bothered by feeling down, depressed, or hopeless?"
    ))

ggsave(
  plot = plt_down,
  filename = "output/socioeconomic-status/down.png",
  height = 150, width = 220, units = "mm"
)

# ---- Received mental health support ----
mh_received <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q14")

mh_received_regional <- 
  mh_received %>% 
  clean_after_loading() %>% 
  slice(-1) %>% 
  
  select(
    Answer, 
    `Upper/middle class` = `ABC1`,
    `Working class and non-working` = `C2DE`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "SES", values_to = "Percentage") %>% 
  
  mutate(Answer = factor(
    Answer,
    levels = rev(c("Prefer not to say", "No", "Yes"))
  ))

(plt_mh_rec <- 
    plot_answers(
      mh_received_regional,
      answer_to_label = "Yes", 
      plot_title = "Received MH support", 
      question = "Have you received support for your mental health in the past three months?"
    ))

ggsave(
  plot = plt_mh_rec,
  filename = "output/socioeconomic-status/mental-health-support-received.png",
  height = 150, width = 150, units = "mm"
)

# ---- Awaiting mental health support ----
mh_awaiting <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q15")

mh_awaiting <- 
  mh_awaiting %>% 
  clean_after_loading() %>% 
  slice(-1)

mh_awaiting_regional <- 
  mh_awaiting %>% 
  select(
    Answer, 
    `Upper/middle class` = `ABC1`,
    `Working class and non-working` = `C2DE`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "SES", values_to = "Percentage") %>% 
  
  mutate(Answer = factor(
    Answer,
    levels = rev(c("Prefer not to say", "No", "Yes"))
  ))

(plt_mh_wait <- 
    plot_answers(
      mh_awaiting_regional,
      answer_to_label = "Yes", 
      plot_title = "Awaiting MH support", 
      question = "Are you currently waiting for support with your mental health?"
    ))

ggsave(
  plot = plt_mh_wait,
  filename = "output/socioeconomic-status/mental-health-support-waiting.png",
  height = 150, width = 150, units = "mm"
)

# ---- Loneliness ----
lonely <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q16")

lonely <- 
  lonely %>% 
  clean_after_loading()

lonely_regional <- 
  lonely %>% 
  filter(
    Answer %in% c("NET: Always/a lot", "NET: Sometimes/hardly ever", "Never", "Prefer not to say")
  ) %>% 
  select(
    Answer, 
    `Upper/middle class` = `ABC1`,
    `Working class and non-working` = `C2DE`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "SES", values_to = "Percentage") %>% 
  
  mutate(Answer = str_remove(Answer, "NET: ")) %>% 
  
  mutate(Answer = factor(
    Answer,
    levels = rev(c("Prefer not to say", "Never", "Sometimes/hardly ever", "Always/a lot"))
  )) %>% 
  
  relocate(SES, Answer, Percentage)

(plt_lonely <- 
    plot_answers(
      lonely_regional,
      answer_to_label = "Always/a lot", 
      plot_title = "Loneliness", 
      question = "How often, if at all, do you feel lonely?"
    ))

ggsave(
  plot = plt_lonely,
  filename = "output/socioeconomic-status/loneliness.png",
  height = 150, width = 150, units = "mm"
)
