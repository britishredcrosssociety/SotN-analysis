library(tidyverse)
library(ggfittext)
library(readxl)

# Polling data is stored in our Teams site under Strategic Insight and Foresight > Data > Polls

# ---- Make a lookup of cross-breaks to their sub-categories ----
categories <- read_excel("data/BRC Dummy Tables.xlsx", skip = 0, sheet = "OP17272_BRC_Q16", n_max = 2)

categories <- 
  categories %>% 
  t() %>% 
  as_tibble() %>%
  rename(Category = V1, Subcategory = V2) %>% 
  slice(-c(1:2)) %>%  # Don't need first two rows
  tidyr::fill(Category, .direction = "down")

# ---- Experiences of vulnerability/risks/shocks ----
# Q: In the last three months, have you experienced any of the following? Please tick all that apply.
experiences <- read_excel("data/BRC Dummy Tables.xlsx", skip = 2, sheet = "OP17272_BRC_Q3")

experiences <- 
  experiences %>% 
  rename(Answer = `...1`) %>% 
  filter(!is.na(Answer)) %>%   # keep only percentages
  filter(Answer != "Return to index") %>% 
  slice(-1)

experiences %>% 
  select(
    Answer, 
    `All ethnic Minorities` = `NET: All ethnic Minorities`, 
    `White` = `NET: White background`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Ethnicity", values_to = "Percentage") %>% 
  
  # Link pairs of answers for plotting lines between the dots; source: https://datavizpyr.com/connect-paired-points-with-lines-in-scatterplot-in-ggplot2/
  mutate(paired = rep(1:(n()/2),each = 2)) %>% 
  
  ggplot(aes(x = reorder(Answer, Percentage, sum), y = Percentage)) +
  geom_line(aes(group = paired), lty = 2) +
  geom_point(aes(colour = Ethnicity)) +
  
  coord_flip() +
  
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c("#7b3294", "#008837")) +
  
  theme_classic() +
  labs(
    x = "",
    y = "Percentage of respondents reporting each adverse event",
    colour = "",
    title = "More people from minoritised ethnic groups reported experiencing almost all adverse events compared to white people",
    subtitle = paste(
      "Althought slightly higher %s of white people reported problems with mental and physical health, and substance abuse",
      "Survey question: In the last three months, have you experienced any of the following? (Tick all that apply)",
      sep = "\n"
    ) ,
    caption = "Source: Opinium survey"
  ) +
  theme(
    # plot.caption = element_text(hjust = 0), #Default is hjust=1
    plot.title.position = "plot",
    plot.caption.position =  "plot", 
    legend.position = "top"
  )

ggsave(
  "output/adverse events by dichotomised ethnicity.png", 
  width = 270, 
  height = 150, 
  units = "mm"
)

# ---- Mental health questions with Likert scales ----
lonely <- read_excel("data/BRC Dummy Tables.xlsx", skip = 2, sheet = "OP17272_BRC_Q16")

lonely <- 
  lonely %>% 
  rename(Answer = `...1`) %>% 
  filter(!is.na(Answer)) %>%   # keep only percentages
  filter(Answer != "Return to index")
