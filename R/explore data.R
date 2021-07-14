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

# Manually rename a couple of the subcategories
categories <- 
  categories %>% 
  mutate(Subcategory = case_when(
    Category == "Gender" & Subcategory == "Other" ~ "Other gender",
    TRUE ~ Subcategory
  ))

# ---- Common data wrangling functions ----
clean_after_loading <- function(d) {
  d %>% 
    rename(Answer = `...1`) %>% 
    filter(!is.na(Answer)) %>%   # keep only percentages
    filter(Answer != "Return to index")
}

wrangle_categories <- function(d) {
  d %>% 
    select(
      Answer, 
      
      # Gender
      Male, Female,
      
      # Age
      `18-34`, `35-54`, `55+`, 
      
      # Ethnicity
      `NET: White background`, 
      `NET: Mixed / multiple ethnic background`, 
      `NET: Asian Background`, 
      `NET: Black/African/Caribbean background`, 
      `NET: Other`, 
      `Don’t think of myself as any of these`,
      `Prefer not to say`,
      
      # Urban/rural
      `Urban area - cities or towns`,
      `Suburban area – residential areas on the outskirts of cities and towns`,
      `Rural area - villages or hamlets`,
      
      # Social grade
      ABC1, C2DE,
      
      # Working status
      `Working full time`,
      `Working part time`, 
      Retired, 
      Unemployed, 
      Other = Other...60,
      
      # Disability
      `Has a disability`, 
      `Does not have a disability`
    ) %>% 
    
    pivot_longer(cols = -Answer, names_to = "Subcategory", values_to = "Percentage") %>% 
    
    left_join(
      categories,
      by = "Subcategory"
    ) %>% 
    
    # Rename some of the subcategories
    mutate(Subcategory = case_when(
      # Ethnicity
      Subcategory == "NET: White background" ~ "White",
      Subcategory == "NET: Mixed / multiple ethnic background" ~ "Mixed/multiple",
      Subcategory == "NET: Asian Background" ~ "Asian",
      Subcategory == "NET: Black/African/Caribbean background" ~ "Black/African/Caribbean",
      Subcategory == "NET: Other" ~ "Other ethnicity",
      Subcategory == "Don’t think of myself as any of these" ~ "Doesn't identify as any",
      
      # Urban/rural
      Subcategory == "Urban area - cities or towns" ~ "Urban",
      Subcategory == "Suburban area – residential areas on the outskirts of cities and towns" ~ "Suburban",
      Subcategory ==  "Rural area - villages or hamlets" ~ "Rural",
      
      # Working status
      Subcategory == "Other...60" ~ "Other",
      
      # Disability
      Subcategory == "Does not have a disability" ~ "No disability",
      
      TRUE ~ Subcategory
    )) %>% 
    
    relocate(Category, Subcategory, Answer, Percentage)
}

# ---- Experiences of vulnerability/risks/shocks ----
# Q: In the last three months, have you experienced any of the following? Please tick all that apply.
experiences <- read_excel("data/BRC Dummy Tables.xlsx", skip = 2, sheet = "OP17272_BRC_Q3")

experiences <- 
  experiences %>% 
  clean_after_loading() %>% 
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

# ---- Support received ----
# Q: In the last three months, have you received help getting non-cash items such as food, clothing, toiletries, prepaid cards for utilities such as energy, or other items from the following? Tick all that apply.
support <- read_excel("data/BRC Dummy Tables.xlsx", skip = 2, sheet = "OP17272_BRC_Q5")

support <- 
  support %>% 
  clean_after_loading() %>% 
  slice(-1)

support_pc_minorities <- 
  support %>% 
  filter(str_detect(Answer, "None")) %>% 
  pull(`NET: All ethnic Minorities`)

support_pc_white <- 
  support %>% 
  filter(str_detect(Answer, "None")) %>% 
  pull(`NET: White background`)

support %>% 
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
  geom_point(aes(colour = Ethnicity), size = 2.5) +
  
  coord_flip() +
  
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c("#7b3294", "#008837")) +
  
  theme_classic() +
  labs(
    x = "",
    y = "Percentage of respondents reporting receiving support",
    colour = "",
    title = glue::glue("{round(1 - support_pc_minorities, 3) * 100}% of people from minoritised ethnic groups received help with non-cash items compared to only {round(1 - support_pc_white, 3) * 100}% of white people"),
    subtitle = "Survey question: In the last three months, have you received help getting non-cash items such as food, clothing, toiletries, \nprepaid cards for utilities such as energy, or other items from the following? (Tick all that apply)",
    caption = "Source: Opinium survey"
  ) +
  theme(
    # plot.caption = element_text(hjust = 0), #Default is hjust=1
    plot.title.position = "plot",
    plot.caption.position =  "plot", 
    legend.position = "top"
  )

ggsave(
  "output/support by dichotomised ethnicity.png", 
  width = 270, 
  height = 150, 
  units = "mm"
)

# ---- Mental health questions with Likert scales ----
lonely <- read_excel("data/BRC Dummy Tables.xlsx", skip = 2, sheet = "OP17272_BRC_Q16")

lonely <- 
  lonely %>% 
  clean_after_loading()

lonely_long <- 
  lonely %>% 
  filter(
    Answer %in% c("NET: Always/a lot", "NET: Sometimes/hardly ever", "Never", "Prefer not to say")
  ) %>% 
  select(
    Answer, 
    
    # Gender
    Male, Female,
    
    # Age
    `18-34`, `35-54`, `55+`, 
    
    # Ethnicity
    `NET: White background`, 
    `NET: Mixed / multiple ethnic background`, 
    `NET: Asian Background`, 
    `NET: Black/African/Caribbean background`, 
    `NET: Other`, 
    `Don’t think of myself as any of these`,
    `Prefer not to say`,
    
    # Urban/rural
    `Urban area - cities or towns`,
    `Suburban area – residential areas on the outskirts of cities and towns`,
    `Rural area - villages or hamlets`,
    
    # Social grade
    ABC1, C2DE,
    
    # Working status
    `Working full time`,
    `Working part time`, 
    Retired, 
    Unemployed, 
    Other = Other...60,
    
    # Disability
    `Has a disability`, 
    `Does not have a disability`
  ) %>% 
  
  pivot_longer(cols = -Answer, names_to = "Subcategory", values_to = "Percentage") %>% 
  
  left_join(
    categories,
    by = "Subcategory"
  ) %>% 
  
  # Rename some of the subcategories
  mutate(Subcategory = case_when(
    # Ethnicity
    Subcategory == "NET: White background" ~ "White",
    Subcategory == "NET: Mixed / multiple ethnic background" ~ "Mixed/multiple",
    Subcategory == "NET: Asian Background" ~ "Asian",
    Subcategory == "NET: Black/African/Caribbean background" ~ "Black/African/Caribbean",
    Subcategory == "NET: Other" ~ "Other ethnicity",
    Subcategory == "Don’t think of myself as any of these" ~ "Doesn't identify as any",
    
    # Urban/rural
    Subcategory == "Urban area - cities or towns" ~ "Urban",
    Subcategory == "Suburban area – residential areas on the outskirts of cities and towns" ~ "Suburban",
    Subcategory ==  "Rural area - villages or hamlets" ~ "Rural",
    
    # Working status
    Subcategory == "Other...60" ~ "Other",
    
    # Disability
    Subcategory == "Does not have a disability" ~ "No disability",
    
    TRUE ~ Subcategory
  )) %>% 
  
  mutate(Answer = str_remove(Answer, "NET: ")) %>% 
  
  mutate(Answer = factor(
    Answer,
    levels = rev(c("Prefer not to say", "Never", "Sometimes/hardly ever", "Always/a lot"))
  )) %>% 
  
  relocate(Category, Subcategory, Answer, Percentage)

# - Plot loneliness by each category -
lonely_long %>% 
  mutate(
    bar_label = if_else(
      Answer == "Always/a lot", 
      paste0(round(Percentage, 3) * 100, "%"),
      ""
    )
  ) %>% 
  
  ggplot(aes(x = Subcategory, y = Percentage, fill = Answer)) +
  geom_col(aes()) +
  geom_text(aes(label = bar_label), size = 3, position = position_stack(vjust = 0.5)) +

  facet_wrap(~Category, ncol = 2, scales = "free") +  # strip.position = "left", 
  coord_flip() +
  
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "#c51b8a",  # Always/a lot
      "#fa9fb5",  # Sometimes
      "#fde0dd",  # Never
      "#bdbdbd"   # Prefer not to say
    )
  ) +
  
  theme_classic() +
  labs(
    x = "",
    y = "Percentage of respondents",
    fill = "",
    title = "Loneliness highest among people who are younger, Black/African/Caribbean, unemployed, urban, or disabled",
    subtitle = "Survey question: How often, if at all, do you feel lonely?",
    caption = "Source: Opinium survey"
  ) +
  theme(
    # plot.caption = element_text(hjust = 0), #Default is hjust=1
    plot.title.position = "plot",
    plot.caption.position =  "plot", 
    legend.position = "top"
  )

ggsave("output/loneliness by all categories.png", height = 200, width = 230, units = "mm")


# ---- Awaiting mental health support ----
mh_awaiting <- read_excel("data/BRC Dummy Tables.xlsx", skip = 2, sheet = "OP17272_BRC_Q15")

mh_awaiting <- 
  mh_awaiting %>% 
  clean_after_loading() %>% 
  slice(-1)

mh_awaiting_long <- 
  mh_awaiting %>% 
  wrangle_categories() %>% 
  
  mutate(Answer = factor(
    Answer,
    levels = rev(c("Prefer not to say", "No", "Yes"))
  ))

# - Plot -
mh_awaiting_long %>% 
  mutate(
    bar_label = if_else(
      Answer == "Yes", 
      paste0(round(Percentage, 3) * 100, "%"),
      ""
    )
  ) %>% 
  
  ggplot(aes(x = Subcategory, y = Percentage, fill = Answer)) +
  geom_col(aes()) +
  geom_text(aes(label = bar_label), size = 3, position = position_stack(vjust = 0.5)) +
  
  facet_wrap(~Category, ncol = 2, scales = "free") +  # strip.position = "left", 
  coord_flip() +
  
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "#fdae61",  # Yes
      "#abd9e9",  # No
      "#bdbdbd"   # Prefer not to say
    )
  ) +
  
  theme_classic() +
  labs(
    x = "",
    y = "Percentage of respondents",
    fill = "",
    title = "People who are younger, male, Black/African/Caribbean, unemployed, urban, or disabled are more likely to be \nwaiting for support with their mental health",
    subtitle = "Survey question: Are you currently waiting for support with your mental health?",
    caption = "Source: Opinium survey"
  ) +
  theme(
    # plot.caption = element_text(hjust = 0), #Default is hjust=1
    plot.title.position = "plot",
    plot.caption.position =  "plot", 
    legend.position = "top"
  )

ggsave("output/waiting for mental health support by all categories.png", height = 200, width = 230, units = "mm")
