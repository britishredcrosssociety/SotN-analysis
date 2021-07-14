library(tidyverse)
library(ggfittext)
library(readxl)

# Polling data is stored in our Teams site under Strategic Insight and Foresight > Data > Polls

# ---- Loneliness ----
lonely <- read_excel("data/BRC Dummy Tables.xlsx", skip = 2, sheet = "OP17272_BRC_Q16")

lonely <- 
  lonely %>% 
  rename(Answer = `...1`) %>% 
  filter(!is.na(Answer)) %>%   # keep only percentages
  filter(Answer != "Return to index")

# Plot loneliness
lonely %>% 
  slice(2:7) %>% 
  
  ggplot(aes(x = Answer, y = Total)) +
  geom_col()


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
