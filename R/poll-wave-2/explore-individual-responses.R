# ---- Load libs ----
library(tidyverse)
library(tidymodels)
library(readxl)
library(viridis)
library(hrbrthemes)

# Polling data is stored in our Teams site under:
# Strategic Insight and Foresight > Data > Polls

# ---- Load data ----
eth_cats <-
  read_csv(
    "data/ethnic-categories-lookup.csv"
  )

# The original lookup table does not cover the 'White and Asian' category found
# in the second wave of the data
eth_cats_amended <-
  eth_cats |>
  add_row(
    `Opinium category` = "White and Asian",
    `Broad ONS category` = "Mixed or Multiple ethnic groups"
  )

omnibus <-
  read_excel(
    "data/OP18518 British Red Cross Understanding Vulnerability - raw data weighted.xlsx",
    sheet = "Tables"
  )

# ---- Replace ethnic categories with ONS ----
poll <-
  omnibus |>
  left_join(eth_cats_amended, by = c("OP18518_D11.a" = "Opinium category")) |>
  select(-OP18518_D11.a) |>
  rename(OP18518_D11.a = `Broad ONS category`) |>
  relocate(OP18518_D11.a, .after = OP18518_D5.a) |>
  mutate(id = row_number())

# ---- Clean & Prep ----
demographics <-
  poll |>
  select(
    id,
    gender = D1_Gender.c,
    age = D2_Age.c,
    region = D4_Region.c,
    employment = D3_employment.a,
    social_grade = D5_socialgrade.a,
    city = Nearest_City.a,
    ruc = OP18518_D5.a,
    ethnicity = OP18518_D11.a
  ) |>
  mutate(
    age = if_else(
      age == "Over 80",
      "80",
      age
    )
  )

# - Q1 -
experiences <-
  poll |>
  select(
    id,
    benefit_sanctions = OP18518_Q1.a.1,
    delays_benefits = OP18518_Q1.a.2,
    behind_bills = OP18518_Q1.a.3,
    behind_rent_mortgage = OP18518_Q1.a.4,
    serious_debt = OP18518_Q1.a.5,
    evicted_home = OP18518_Q1.a.6,
    homeless = OP18518_Q1.a.7,
    serious_physical_health = OP18518_Q1.a.8,
    domestic_abuse = OP18518_Q1.a.9,
    alcohol_drug_problems = OP18518_Q1.a.10,
    coming_uk = OP18518_Q1.a.11,
    problems_live_work_uk = OP18518_Q1.a.12,
    none_of_the_above = OP18518_Q1.a.13
  ) |>
  pivot_longer(
    cols = -id,
    names_to = "experience",
    values_to = "experience_value"
  ) |>
  mutate(
    experience_value = if_else(
      is.na(experience_value),
      0,
      1
    )
  )

# - Q2 -
support <-
  poll |>
  select(
    id,
    universal_credits_benefits = OP18518_Q2.a.1,
    parents = OP18518_Q2.a.2,
    other_relatives = OP18518_Q2.a.3,
    friends = OP18518_Q2.a.4,
    colleagues = OP18518_Q2.a.5,
    charities = OP18518_Q2.a.6,
    food_banks = OP18518_Q2.a.7,
    welfare_fund = OP18518_Q2.a.8,
    paid_work = OP18518_Q2.a.9,
    begging = OP18518_Q2.a.10,
    advice_service = OP18518_Q2.a.11,
    day_centre = OP18518_Q2.a.12,
    organisations_supporting_migrants = OP18518_Q2.a.13,
    other = OP18518_Q2.a.14,
    none_of_the_above = OP18518_Q2.a.15,
    not_received_support = OP18518_Q2.a.16,
    prefer_not_to_say = OP18518_Q2.a.17
  ) |>
  pivot_longer(
    cols = -id,
    names_to = "support",
    values_to = "support_value"
  ) |>
  mutate(
    support_value = if_else(
      is.na(support_value),
      0,
      1
    )
  )

# - Q3 -
daily_activites <-
  poll |>
  select(
    id,
    daily_activities = OP18518_Q3.a
  )

# - Q4 -
mobility <-
  poll |>
  select(
    id,
    mobility = OP18518_Q4.a
  )

# - Q5 -
usual_activites <-
  poll |>
  select(
    id,
    usual_activities = OP18518_Q5.a
  )

# - Q6 -
satisified <-
  poll |>
  select(
    id,
    satisfied = OP18518_Q6.a.1
  ) |>
  mutate(
    satisfied = case_when(
      satisfied == "0 – Not at all" ~ "0",
      satisfied == "10 – Completely" ~ "10",
      TRUE ~ satisfied
    )
  )

worthwhile <-
  poll |>
  select(
    id,
    worthwhile = OP18518_Q6.a.2
  ) |>
  mutate(
    worthwhile = case_when(
      worthwhile == "0 – Not at all" ~ "0",
      worthwhile == "10 – Completely" ~ "10",
      TRUE ~ worthwhile
    )
  )

happy <-
  poll |>
  select(
    id,
    happy = OP18518_Q6.a.3
  ) |>
  mutate(
    happy = case_when(
      happy == "0 – Not at all" ~ "0",
      happy == "10 – Completely" ~ "10",
      TRUE ~ happy
    )
  )

anxious <-
  poll |>
  select(
    id,
    anxious = OP18518_Q6.a.4
  ) |>
  mutate(
    anxious = case_when(
      anxious == "0 – Not at all" ~ "0",
      anxious == "10 – Completely" ~ "10",
      TRUE ~ anxious
    )
  )

# - Q7 -
pleasure <-
  poll |>
  select(
    id,
    pleasure = OP18518_Q7.a
  )

# - Q8 -
depressed <-
  poll |>
  select(
    id,
    depressed = OP18518_Q8.a
  )

# - Q9 -
received_mental_health_support <-
  poll |>
  select(
    id,
    received_mental_health_support = OP18518_Q9.a
  )

# - Q10 -
waiting_mental_health_support <-
  poll |>
  select(
    id,
    waiting_mental_health_support = OP18518_Q10.a
  )

# - Q11 -
lonely <-
  poll |>
  select(
    id,
    lonely = OP18518_Q11.a
  )

# ---- Mental Health Models ----
mental_health_data <-
  demographics |>
  left_join(anxious, by = "id") |>
  left_join(depressed, by = "id") |>
  left_join(received_mental_health_support, by = "id") |>
  left_join(waiting_mental_health_support, by = "id")

# convert col types
mental_health_types <-
  mental_health_data |>
  mutate(
    across(
      all_of(
        c(
          "gender",
          "region",
          "social_grade",
          "ruc", "ethnicity",
          "depressed",
          "received_mental_health_support",
          "waiting_mental_health_support"
        )
      ),
      factor
    )
  ) |>
  mutate(
    across(
      all_of(c("age", "anxious")), as.integer
    )
  )

# - EDA -
# Is age related to levels of depression?
mental_health_factors |> 
  select(gender, age, ethnicity, depressed) |> 
  ggplot(aes(x = depressed, y = age, fill = depressed)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_viridis(discrete = TRUE) +
  coord_flip() +
  theme_ipsum() +
  geom_jitter(color = "black", size = 0.4, alpha = 0.5) +
  theme(legend.position = "none")

# Is ethnicity related to mental health support?
mental_health_factors |> 
  select(ethnicity, ends_with("health_support")) |> 
  filter(received_mental_health_support != "Prefer not to say") |> 
  count(ethnicity, received_mental_health_support) |> 
  group_by(ethnicity) |> 
  mutate(prop_receiving_support = n / sum(n)) |> 
  ungroup() |> 
  filter(received_mental_health_support == "Yes") |> 
  ggplot(aes(x = ethnicity, y = prop_receiving_support)) +
  geom_col(fill = "#D0021B", alpha = 0.5, colour = "black") +
  coord_flip() +
  theme(legend.position = "none") +
  theme_ipsum()

mental_health_factors |> 
  select(ethnicity, ends_with("health_support")) |> 
  filter(waiting_mental_health_support != "Prefer not to say") |> 
  count(ethnicity, waiting_mental_health_support) |> 
  group_by(ethnicity) |> 
  mutate(prop_waiting_support = n / sum(n)) |> 
  ungroup() |> 
  filter(waiting_mental_health_support == "Yes") |> 
  ggplot(aes(x = ethnicity, y = prop_waiting_support)) +
  geom_col(fill = "#D0021B", alpha = 0.5, colour = "black") +
  coord_flip() +
  theme(legend.position = "none") +
  theme_ipsum()
