library(tidyverse)
library(janitor)
library(readxl)

omnibus <- read_excel("data/OP17272 Anon Raw Tables Omni & Ethnic Boost.xlsx", sheet = "Omnibus")

boost <- read_excel("data/OP17272 Anon Raw Tables Omni & Ethnic Boost.xlsx", sheet = "Ethnic North Boost", col_types = "text")

all <- bind_rows(omnibus, boost)

# ---- Adverse events ----
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
# Money
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
  mutate(n_events = sum(!is.na(c_across(starts_with("OP17272_BRC_Q4")))))

# Bin the counts of adverse events (in a fairly arbitrary way) and summarise
all_events %>% 
  mutate(n_events_grouped = case_when(
    n_events == 0 ~ "0",
    n_events == 1 ~ "1",
    n_events > 1 & n_events <= 5 ~ "2-5",
    n_events > 5 ~ "5+"
  )) %>% 
  
  tabyl(n_events)
