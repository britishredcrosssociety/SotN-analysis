library(tidyverse)
library(tidytext)
library(topicmodels)
library(ldatuning)
library(viridis)

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

custom_stop_words_health <- 
  tibble(word = c("bbc", "news", "people", "19", "reports", "sky", "guardian", "independent", "report", "research", "online", "radio"))

health_words <- 
  health_words |> 
  anti_join(custom_stop_words_health)

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
  filter(!word1 %in% c(stop_words$word, custom_stop_words_health$word)) |>
  filter(!word2 %in% c(stop_words$word, custom_stop_words_health$word)) |> 
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

# ---- Topic models ----
# Document term matrix
dtm <-
  health_words  |> 
  add_rownames() |> 
  count(rowname, word) |> 
  cast_dtm(rowname, word, n)

# Select number of topics (k) for LDA model using the 'ldatuninig' package.
lda_fit <-
  FindTopicsNumber(
    dtm,
    topics = seq(from = 2, to = 10, by = 1),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs", control = list(seed = 77), mc.cores = 2L, verbose = TRUE
  )
# 
# lda_fit %>% write_rds("analysis/vaccines/data/lda-fit.rds")

# find the extremum to determine optimal k
FindTopicsNumber_plot(lda_fit)

lda <- LDA(dtm, k = 5, control = list(seed = 1234))

# lda %>% write_rds("analysis/vaccines/data/lda-data.rds")

health_topics <- tidy(lda, matrix = "beta")

top_terms <- 
  health_topics |>
  group_by(topic) |>
  top_n(10, beta) |>
  ungroup() |>
  arrange(topic, -beta)

# - plot -
top_terms |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, nrow = 2, ncol = 5, scales = "free") +
  scale_y_reordered() +
  theme_minimal() +
  scale_fill_viridis(discrete = T, begin = .15, end = .85, alpha = .8) + 
  labs(y = NULL,
       title = "Naturally occuring topics related to Health Inequalities",
       subtitle = "The beta value is the probability that any given word belongs to a topic",
       caption = "Colour denotes topic number.")

ggsave("output/news-health-topics.png", width = 250, height = 150, units = "mm")
