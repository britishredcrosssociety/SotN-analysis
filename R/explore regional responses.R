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
    bar_pallette <- c(
      "#c51b8a",  # Always/a lot
      "#fa9fb5",  # Sometimes
      "#fde0dd",  # Never
      "#bdbdbd"   # Prefer not to say
    )
    
  } else if (length(unique(d$Answer)) == 3) {
    bar_pallette <- c(
      "#fdae61",  # Yes
      "#abd9e9",  # No
      "#bdbdbd"   # Prefer not to say
    )
    
  }
  
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
    scale_fill_manual(values = bar_pallette) +
    
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
