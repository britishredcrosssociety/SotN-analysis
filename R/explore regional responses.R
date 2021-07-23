library(tidyverse)
library(readxl)

# ---- Common functions ----
clean_after_loading <- function(d) {
  d %>% 
    rename(Answer = `...1`) %>% 
    filter(!is.na(Answer)) %>%   # keep only percentages
    filter(Answer != "Return to index")
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
      viridis::magma(length(unique(d$Answer)) - 1),
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
    geom_col() +
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
      subtitle = paste0("Survey question: ", question)
      # caption = "Source: Opinium survey"
    ) +
    theme(
      # plot.caption = element_text(hjust = 0), #Default is hjust=1
      plot.title.position = "plot",
      plot.caption.position =  "plot", 
      legend.position = "top"
    )
}

# ---- Day-to-day activities limited ----
activities <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q6")

activities_regional <- 
  activities %>% 
  clean_after_loading() %>% 
  slice(-1) %>% 

  select(
    Answer, 
    Scotland:`South West`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  filter(Answer != "NET: Yes") %>% 
  mutate(Answer = factor(
    Answer,
    levels = rev(c("No", "Yes, limited a little", "Yes, limited a lot"))
  ))

plot_regional_answers(
  activities_regional,
  answer_to_label = "Yes, limited a lot", 
  plot_title = "Daily activities limited", 
  question = "Are your day-to-day activities limited because of a health problem or disability which has lasted, or is expected to last, at least 12 months?"
)

# ---- Mobility ----
mobility <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q7")

mobility_regional <- 
  mobility %>% 
  clean_after_loading() %>% 
  slice(-1) %>% 

  select(
    Answer, 
    Scotland:`South West`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
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

plot_regional_answers(
  mobility_regional,
  answer_to_label = "Severe/extreme problems", 
  plot_title = "Mobility", 
  question = "Please tick the response that best describes your health today regarding mobility."
)

# ---- Self-care ----
self_care <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q8")

self_care_regional <- 
  self_care %>% 
  clean_after_loading() %>% 
  slice(-1) %>% 
  
  select(
    Answer, 
    Scotland:`South West`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  filter(
    Answer %in% c("NET: Slight/moderate problems", "NET: Severe/extreme problems", "I have no problems washing or dressing myself", "Prefer not to say")
  ) %>% 
  
  mutate(Answer = if_else(Answer == "I have no problems washing or dressing myself", "No problems", str_remove(Answer, "NET: "))) %>% 
  
  mutate(Answer = factor(
    Answer,
    levels = rev(c("Prefer not to say", "No problems", "Slight/moderate problems", "Severe/extreme problems"))
  )) %>% 
  
  relocate(Region, Answer, Percentage)

plot_regional_answers(
  self_care_regional,
  answer_to_label = "Severe/extreme problems", 
  plot_title = "Self-care", 
  question = "Please tick the response that best describes your health today regarding self-care."
)

# ---- Usual activities ----
usual_activities <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q9")

usual_activities_regional <- 
  usual_activities %>% 
  clean_after_loading() %>% 
  slice(-1) %>% 
  
  select(
    Answer, 
    Scotland:`South West`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  filter(
    Answer %in% c("NET: Slight/moderate problems", "NET: Severe/extreme problems", "I have no problems doing my usual activities", "Prefer not to say")
  ) %>% 
  
  mutate(Answer = if_else(Answer == "I have no problems doing my usual activities", "No problems", str_remove(Answer, "NET: "))) %>% 
  
  mutate(Answer = factor(
    Answer,
    levels = rev(c("Prefer not to say", "No problems", "Slight/moderate problems", "Severe/extreme problems"))
  )) %>% 
  
  relocate(Region, Answer, Percentage)

plot_regional_answers(
  usual_activities_regional,
  answer_to_label = "Severe/extreme problems", 
  plot_title = "Usual activities", 
  question = "Please tick the response that best describes your health today regarding usual activities (e.g. work, study, housework, family or leisure activities)."
)

# ---- Pain/discomfort ----
pain <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q10")

pain_regional <- 
  pain %>% 
  clean_after_loading() %>% 
  slice(-1) %>% 
  
  select(
    Answer, 
    Scotland:`South West`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  filter(
    Answer %in% c("NET: Slight/moderate pain or discomfort", "NET: Severe/extreme pain or discomfort", "I have no pain or discomfort", "Prefer not to say")
  ) %>% 
  
  mutate(Answer = if_else(Answer == "I have no pain or discomfort", "No pain/discomfort", str_remove(Answer, "NET: "))) %>% 
  
  mutate(Answer = factor(
    Answer,
    levels = rev(c("Prefer not to say", "No pain/discomfort", "Slight/moderate pain or discomfort", "Severe/extreme pain or discomfort"))
  )) %>% 
  
  relocate(Region, Answer, Percentage)

plot_regional_answers(
  pain_regional,
  answer_to_label = "Severe/extreme pain or discomfort", 
  plot_title = "Pain/discomfort", 
  question = "Please tick the response that best describes your health today regarding pain/discomfort."
)

# ---- Life satisfaction ----
satisfaction <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q11")

satisfaction_regional <- 
  satisfaction %>% 
  clean_after_loading() %>% 
  filter(!Answer %in% c("Base: all respondents", "Average")) %>% 
  
  select(
    Answer, 
    Scotland:`South West`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  mutate(Answer = str_remove(Answer, "\\[[0-9]+\\]\\s")) %>% 
  mutate(Answer = factor(Answer)) %>% 
  mutate(Answer = fct_relevel(Answer, "10 – Completely", after = Inf)) %>% 
  
  relocate(Region, Answer, Percentage)

plot_regional_answers(
  satisfaction_regional,
  answer_to_label = "0 – Not at all", 
  plot_title = "Life satisfaction", 
  question = "How satisfied are you with your life nowadays?"
)

# ---- Worthwhile ----
worthwhile <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q11 (2)")

worthwhile_regional <- 
  worthwhile %>% 
  clean_after_loading() %>% 
  filter(!Answer %in% c("Base: all respondents", "Average")) %>% 
  
  select(
    Answer, 
    Scotland:`South West`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  mutate(Answer = str_remove(Answer, "\\[[0-9]+\\]\\s")) %>% 
  mutate(Answer = factor(Answer)) %>% 
  mutate(Answer = fct_relevel(Answer, "10 – Completely", after = Inf)) %>% 
  
  relocate(Region, Answer, Percentage)

plot_regional_answers(
  worthwhile_regional,
  answer_to_label = "0 – Not at all", 
  plot_title = "Worthwhile", 
  question = "To what extent do you feel the things you do in your life are worthwhile?"
)

# ---- Happiness ----
happiness <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q11 (3)")

happiness_regional <- 
  happiness %>% 
  clean_after_loading() %>% 
  filter(!Answer %in% c("Base: all respondents", "Average")) %>% 
  
  select(
    Answer, 
    Scotland:`South West`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  mutate(Answer = str_remove(Answer, "\\[[0-9]+\\]\\s")) %>% 
  mutate(Answer = factor(Answer)) %>% 
  mutate(Answer = fct_relevel(Answer, "10 – Completely", after = Inf)) %>% 
  
  relocate(Region, Answer, Percentage)

plot_regional_answers(
  happiness_regional,
  answer_to_label = "0 – Not at all", 
  plot_title = "Happiness", 
  question = "How happy did you feel yesterday?"
)

# ---- Anxious ----
anxious <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q11 (4)")

anxious_regional <- 
  anxious %>% 
  clean_after_loading() %>% 
  filter(!Answer %in% c("Base: all respondents", "Average")) %>% 
  
  select(
    Answer, 
    Scotland:`South West`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  mutate(Answer = str_remove(Answer, "\\[[0-9]+\\]\\s")) %>% 
  mutate(Answer = factor(Answer)) %>% 
  mutate(Answer = fct_relevel(Answer, "10 – Completely", after = Inf)) %>% 
  
  relocate(Region, Answer, Percentage)

plot_regional_answers(
  anxious_regional,
  answer_to_label = "0 – Not at all", 
  plot_title = "Anxiety", 
  question = "How anxious did you feel yesterday?"
)

# ---- No interest/pleasure ----
interest <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q12")

interest_regional <- 
  interest %>% 
  clean_after_loading() %>% 
  filter(!Answer %in% c("Base: all respondents", "NET: Has been bothered")) %>% 
  
  select(
    Answer, 
    Scotland:`South West`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  mutate(
    Answer = factor(
      Answer,
      levels = c("Nearly every day", "More than half the days", "Several days", "Not at all", "Prefer not to say")
    )
  ) %>% 

  relocate(Region, Answer, Percentage)

plot_regional_answers(
  interest_regional,
  answer_to_label = "Nearly every day", 
  plot_title = "Bothered by no interest/pleasure", 
  question = "Over the last two weeks, how often, if at all, have you been bothered by not having interest or pleasure in doing things?"
)

# ---- Down/depressed ----
down <- read_excel("data/OP17272 BRC Understanding Vulnerabilities.xlsx", skip = 2, sheet = "OP17272_BRC_Q13")

down_regional <- 
  down %>% 
  clean_after_loading() %>% 
  filter(!Answer %in% c("Base: all respondents", "NET: Has been bothered")) %>% 
  
  select(
    Answer, 
    Scotland:`South West`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  mutate(
    Answer = factor(
      Answer,
      levels = c("Nearly every day", "More than half the days", "Several days", "Not at all", "Prefer not to say")
    )
  ) %>% 
  
  relocate(Region, Answer, Percentage)

plot_regional_answers(
  down_regional,
  answer_to_label = "Nearly every day", 
  plot_title = "Down/depressed", 
  question = "Over the last two weeks, how often have you been bothered by feeling down, depressed, or hopeless?"
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
    Scotland:`South West`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  mutate(Answer = factor(
    Answer,
    levels = rev(c("Prefer not to say", "No", "Yes"))
  ))

plot_regional_answers(
  mh_awaiting_regional,
  answer_to_label = "Yes", 
  plot_title = "Awaiting MH support", 
  question = "Are you currently waiting for support with your mental health?"
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
    Scotland:`South West`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Region", values_to = "Percentage") %>% 
  
  mutate(Answer = str_remove(Answer, "NET: ")) %>% 
  
  mutate(Answer = factor(
    Answer,
    levels = rev(c("Prefer not to say", "Never", "Sometimes/hardly ever", "Always/a lot"))
  )) %>% 
  
  relocate(Region, Answer, Percentage)

plot_regional_answers(
  lonely_regional,
  answer_to_label = "Always/a lot", 
  plot_title = "Loneliness", 
  question = "How often, if at all, do you feel lonely?"
)
