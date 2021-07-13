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
  # mutate(bar_label = paste0(round(`NET: All ethnic Minorities`, 3) * 100, "%")) %>% 
  
  ggplot(aes(x = reorder(Answer, `NET: All ethnic Minorities`, sum), y = `NET: All ethnic Minorities`)) +
  geom_col() +
  # geom_text(aes(label = bar_label), hjust = -0.75) +
  geom_point(aes(y = Total)) +
  
  coord_flip() +
  
  scale_y_continuous(labels = scales::percent) +
  
  theme_classic() +
  labs(
    x = "",
    y = "",
    title = "In the last three months, have you experienced any of the following?",
    subtitle = "Bars show responses from minoritised ethnic groups; dots show UK average",
    caption = "Source: Opinium survey"
  )



