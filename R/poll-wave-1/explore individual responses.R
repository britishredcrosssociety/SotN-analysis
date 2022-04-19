library(tidyverse)
library(janitor)
library(readxl)
library(lme4)

eth_cats <- read_csv("data/ethnic-categories-lookup.csv")

omnibus <- read_excel("data/OP17272 Anon Raw Tables Omni & Ethnic Boost.xlsx", sheet = "Omnibus")

boost <- read_excel("data/OP17272 Anon Raw Tables Omni & Ethnic Boost.xlsx", sheet = "Ethnic North Boost", col_types = "text")

all <- bind_rows(omnibus, boost)

# ---- Adverse events ----
# Q: In the last three months, have you experienced any of the following? Please tick all that apply.

# Count number of adverse events reported per person
all_events <- 
  all %>% 
  select(
    altid.a, 
    Ethnicity = OP17272_BRC_Q2.a,
    starts_with("OP17272_BRC_Q3")
  ) %>% 
  
  # Drop columns for "None of the above" and "Prefer not to say"
  select(-c(OP17272_BRC_Q3.a.22, OP17272_BRC_Q3.a.23)) %>% 
  
  rowwise() %>% 
  mutate(n_events = sum(!is.na(c_across(starts_with("OP17272_BRC_Q3")))))

hist(all_events$n_events)

# Bin the counts of adverse events (in a fairly arbitrary way) and summarise
all_events %>% 
  mutate(n_events_grouped = case_when(
    n_events == 0 ~ "0",
    n_events == 1 ~ "1",
    n_events > 1 & n_events <= 5 ~ "2-5",
    n_events > 5 ~ "5+"
  )) %>% 
  
  tabyl(n_events_grouped)

# 
all_events %>% 
  mutate(n_events_grouped = case_when(
    n_events == 0 ~ "0",
    n_events == 1 ~ "1",
    n_events > 1 & n_events <= 5 ~ "2-5",
    n_events > 5 ~ "5+"
  )) %>% 
  
  tabyl(Ethnicity, n_events_grouped) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 1)

# ---- Routes to support ----
# - Money -
# Q: In the last three months, have you received money from any of the following? Tick all that apply.
all_support_money <- 
  all %>% 
  select(
    altid.a, 
    Ethnicity = OP17272_BRC_Q2.a,
    starts_with("OP17272_BRC_Q4")
  ) %>% 
  
  # Drop columns for "None of the above" and "Prefer not to say"
  select(-c(OP17272_BRC_Q4.a.11, OP17272_BRC_Q4.a.12)) %>% 
  
  rowwise() %>% 
  mutate(n_supporters = sum(!is.na(c_across(starts_with("OP17272_BRC_Q4")))))

# Bin the counts of adverse events (in a fairly arbitrary way) and summarise
all_support_money %>% 
  mutate(n_supporters_grouped = case_when(
    n_supporters == 0 ~ "0",
    n_supporters == 1 ~ "1",
    n_supporters > 1 ~ "2+"
  )) %>% 
  
  tabyl(n_supporters_grouped)

plot(table(all_support_money$n_supporters))

# - Non-financial support -
# Q: In the last three months, have you received help getting non-cash items such as food, clothing, toiletries, prepaid cards for utilities such as energy’, or other items from the following? Tick all that apply.
all_support_other <- 
  all %>% 
  select(
    altid.a, 
    Ethnicity = OP17272_BRC_Q2.a,
    starts_with("OP17272_BRC_Q5")
  ) %>% 
  
  # Drop columns for "None of the above" and "Prefer not to say"
  select(-c(OP17272_BRC_Q5.a.12, OP17272_BRC_Q5.a.13)) %>% 
  
  rowwise() %>% 
  mutate(n_supporters = sum(!is.na(c_across(starts_with("OP17272_BRC_Q5")))))

# Bin the counts of adverse events (in a fairly arbitrary way) and summarise
all_support_other %>% 
  mutate(n_supporters_grouped = case_when(
    n_supporters == 0 ~ "0",
    n_supporters == 1 ~ "1",
    n_supporters > 1 ~ "2+"
  )) %>% 
  
  tabyl(n_supporters_grouped)

plot(table(all_support_other$n_supporters))

# ---- Loneliness ----
loneliness <- 
  all %>% 
  select(
    # Demographics
    Age = D2_Age.c,
    Gender = D1_Gender.c,
    Ethnicity = OP17272_BRC_Q2.a,
    Region = D4_Region.c,
    Employment = D3_employment.a,
    Grade = D5_socialgrade.a,
    RUC = OP17272_BRC_Q1.a,
    
    # Questions of interest
    Loneliness = OP17272_BRC_Q16.a
  ) %>% 
  
  mutate(
    Age = as.integer(ifelse(Age == "Over 80", "80", Age))
  ) %>% 
  
  mutate(
    Age_group = cut(Age, c(18, 30, 40, 50, 60, 70, 80, 90), right = FALSE)
  ) %>% 
  
  left_join(eth_cats, by = c("Ethnicity" = "Opinium category")) %>% 
  rename(Ethnicity_grouped = `Broad ONS category`) %>% 
  mutate(Ethnicity_grouped = factor(Ethnicity_grouped)) %>% 
  mutate(Ethnicity_grouped = fct_relevel(Ethnicity_grouped, "White")) %>% 
  
  mutate(
    Loneliness_bin = if_else(
      Loneliness %in% c("Always", "A lot of the time"),
      1,
      0
    )
  )

# Regional profile of people reporting a lot of loneliness
# summary(glm(Loneliness_bin ~ Age + Gender + Ethnicity + Employment + Grade + RUC + Region, data = loneliness))

loneliness_summary <- 
  loneliness %>% 
  filter(Loneliness_bin == 1) %>% 
  count(Age_group, Ethnicity_grouped, Region)

loneliness_totals <- 
  loneliness_summary %>% 
  group_by(Region) %>% 
  summarise(n_total = sum(n)) %>% 
  ungroup()

loneliness_summary %>% 
  left_join(loneliness_totals, by = "Region") %>% 
  mutate(prop = n / n_total) %>% 
  
  ggplot(aes(x = Age_group, y = prop, fill = Ethnicity_grouped)) +
  geom_col() +
  facet_wrap(~Region) +
  scale_y_continuous(labels = scales::percent)

loneliness %>% 
  filter(Region == "Wales") %>% 
  distinct(Ethnicity_grouped)

m_loneliness <- 
   glmer(
    Loneliness_bin ~ Age + Gender + Ethnicity_grouped + (1 | Region), 
    family = "binomial",
    data = loneliness
  )

summary(m_loneliness)

new_data <- expand_grid(
  Age = 18:80,
  Gender = unique(loneliness$Gender),
  Ethnicity_grouped = unique(loneliness$Ethnicity_grouped),
  Region = unique(loneliness$Region)
)

pred_loneliness <- predict(m_loneliness, new_data, type = "response", se.fit = TRUE)

new_data$fit <- pred_loneliness$fit
new_data$se.fit <- pred_loneliness$se.fit

# Calculate CIs
new_data$ci.high <- new_data$fit + 1.96 * new_data$se.fit
new_data$ci.low  <- new_data$fit - 1.96 * new_data$se.fit

# Plot loneliness by age group, sex and ethnicity
new_data %>% 
  ggplot(aes(x = Age, y = fit, shape = Gender, lty = Gender, colour = Ethnicity_grouped)) +
  geom_pointrange(aes(ymin = ci.low, ymax = ci.high), position = position_dodge(width = 1)) +
  facet_wrap(~Region) +
  
  scale_colour_brewer(palette = "Set2") +
  
  labs(x = NULL, y = "Probability of reporting loneliness", colour = NULL, shape = NULL, lty = NULL,
       caption = "Source: BRC/I&I analysis of Understanding Society") +
  theme_classic() +
  theme(legend.position = "bottom", 
        # axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.x = element_blank())


# ---- Multivariate analysis ----
all_summary <- 
  all %>% 
  select(
    altid.a, 
    
    # Demographics
    Age = D2_Age.c,
    Gender = D1_Gender.c,
    Ethnicity = OP17272_BRC_Q2.a,
    
    # Questions of interest
    starts_with("OP17272_BRC_Q3"),
    starts_with("OP17272_BRC_Q4"),
    starts_with("OP17272_BRC_Q5")
  ) %>% 
  
  # Drop columns for "None of the above" and "Prefer not to say"
  select(-c(OP17272_BRC_Q3.a.22, OP17272_BRC_Q3.a.23)) %>% 
  select(-c(OP17272_BRC_Q4.a.11, OP17272_BRC_Q4.a.12)) %>% 
  select(-c(OP17272_BRC_Q5.a.12, OP17272_BRC_Q5.a.13)) %>% 
  
  rowwise() %>% 
  mutate(
    n_events = sum(!is.na(c_across(starts_with("OP17272_BRC_Q3")))),
    n_supporters_money = sum(!is.na(c_across(starts_with("OP17272_BRC_Q4")))),
    n_supporters = sum(!is.na(c_across(starts_with("OP17272_BRC_Q5"))))
  ) %>% 
  
  select(altid.a, Age, Gender, Ethnicity, starts_with("n_")) %>% 
  
  left_join(eth_cats, by = c("Ethnicity" = "Opinium category")) %>% 
  rename(Ethnicity_grouped = `Broad ONS category`) %>% 
  mutate(Ethnicity_grouped = factor(Ethnicity_grouped)) %>% 
  mutate(Ethnicity_grouped = fct_relevel(Ethnicity_grouped, "White")) %>% 
  
  # filter(Gender != "Prefer not to say") %>% 
  
  mutate(Age = as.integer(ifelse(Age == "Over 80", "80", Age))) %>% 
  
  mutate(
    adverse_events_no_support = factor(
      ifelse(n_events > 0 & n_supporters_money == 0 & n_supporters == 0, "Yes", "No"),
      levels = c("No", "Yes")
    )
  )

# Numbers/proportions of respondents experiencing at least one adverse event but receiving no support 
all_summary %>% 
  tabyl(adverse_events_no_support) %>% 
  as_tibble()

# Numbers/proportions of respondents experiencing at least one adverse event but receiving no support - by ethnicity
all_summary %>% 
  tabyl(Ethnicity_grouped, adverse_events_no_support) %>% 
  adorn_percentages("row") %>% 
  
  as_tibble() %>% 
  pivot_longer(cols = No:Yes) %>% 
  
  ggplot(aes(x = Ethnicity_grouped, y = value)) +
  geom_col(aes(fill = name))
  
# - Modelling -
library(tidymodels)
library(dotwhisker)

glm_mod <- 
  logistic_reg() %>% 
  set_engine("glm")

glm_fit <- 
  glm_mod %>% 
  fit(adverse_events_no_support ~ Age + Ethnicity_grouped, data = all_summary)

tidy(glm_fit, conf.int = TRUE)

tidy(glm_fit) %>% 
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2))

all_summary %>% 
  ggplot(aes(x = Age, y = n_events, colour = Ethnicity_grouped)) +
  geom_point() +
  geom_smooth(method = "lm")
