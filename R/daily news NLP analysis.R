library(tidyverse)
library(tidytext)
library(topicmodels)
library(ldatuning)

# ---- Load and prep data ----
news <- read_lines("data/daily-news-2021-08.txt") |> 
  as_tibble() |> 
  rename(text = value) |> 
  filter(text != "")

news_health <- 
  news |> 
  filter(str_detect(text, "^Health")) |> 
  mutate(text = str_remove(text, "Health inequalities: "))

# Convert text to lower case
news_health <-
  news_health |> 
  mutate(text = str_to_lower(text))

# ---- Unigrams ----
# Tokenize text column to unigrams
health_words <-
  news_health |> 
  unnest_tokens(word, text) |> 
  anti_join(stop_words)

# Inspect top words and remove "junk" words
most_frequent_words <-
  health_words |> 
  count(word, sort = TRUE)

custom_stop_words <- 
  tibble(word = c("bbc", "news", "people", "19", "reports", "sky", "guardian", "independent", "report", "research", "online", "radio"))

health_words <- 
  health_words |> 
  anti_join(custom_stop_words)

# Plot word counts
health_words |> 
  count(word, sort = TRUE) |> 
  filter(n > 2) |> 
  mutate(word = reorder(word, n)) |> 
  ggplot(aes(n, word)) +
  geom_col(fill = "#D0021B", alpha = 0.5, show.legend = FALSE) +
  labs(
    x = "Number of times mentioned",
    y = NULL
  ) +
  theme_classic()

ggsave("output/news-word-counts.png", width = 100, height = 100, units = "mm")

# ---- Bigrams ----
health_bigrams <- 
  news_health |> 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) |> 
  separate(bigram, c("word1", "word2"), sep = " ") |> 
  filter(!word1 %in% c(stop_words$word, custom_stop_words$word)) |>
  filter(!word2 %in% c(stop_words$word, custom_stop_words$word)) |> 
  unite(bigram, word1, word2, sep = " ")

# Plot
health_bigrams |> 
  count(bigram, sort = TRUE) |> 
  filter(n > 1) |>
  mutate(bigram = reorder(bigram, n)) |>
  ggplot(aes(n, bigram)) +
  geom_col(fill = "#D0021B", alpha = 0.5, show.legend = FALSE) +
  labs(
    x = "Number of times mentioned",
    y = NULL
  ) +
  theme_classic()

ggsave("output/news-bigrams.png", width = 100, height = 100, units = "mm")

# ---- 