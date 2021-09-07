library(tidyverse)
library(tidytext)
library(topicmodels)
library(ldatuning)
library(viridis)
library(ggfittext)

# ---- Load and prep data ----
news <- read_lines("data/daily-news-2021-08.txt") |> 
  as_tibble() |> 
  rename(text = value) |> 
  filter(text != "")

news_health <- 
  news |> 
  filter(str_detect(text, "^Health")) |> 
  mutate(text = str_remove(text, "Health inequalities: ")) |> 
  mutate(text = str_to_lower(text))

news_disasters <- 
  news |> 
  filter(str_detect(text, "^Disasters")) |> 
  mutate(text = str_remove(text, "Disasters and emergencies: ")) |> 
  mutate(text = str_to_lower(text))

news_displacement <- 
  news |> 
  filter(str_detect(text, "^Displacement")) |> 
  mutate(text = str_remove(text, "Displacement and migration: ")) |> 
  mutate(text = str_to_lower(text))

news_climate <- 
  news |> 
  filter(str_detect(text, "^Climate")) |> 
  mutate(text = str_remove(text, "Climate: ")) |> 
  mutate(text = str_remove(text, "Climate change: ")) |> 
  mutate(text = str_to_lower(text))

# ---- How often does each cause get mentioned / have a news item? ---
news |> 
  mutate(first_word = str_extract(text, "\\w+")) |> 
  count(first_word) |> 
  arrange(desc(n)) |> 
  mutate(first_word = reorder(first_word, n)) |> 
  
  ggplot(aes(n, first_word, label = n)) +
  geom_col(fill = "#D0021B", alpha = 0.5, show.legend = FALSE) +
  geom_bar_text() +
  labs(
    x = "Number of times mentioned",
    y = NULL
  ) +
  theme_classic()

ggsave("output/news-cause-mentions.png", width = 100, height = 100, units = "mm")

# ---- Figure out custom stop words ----
#' Count most frequent words
#' @param news A tibble/dataframe containing the text to parse
#' @return Tibble containing word frequencies
word_frequency <- function(news) {
  news |> 
    unnest_tokens(word, text) |> 
    anti_join(stop_words) |> 
    count(word, sort = TRUE)
}

# - Inspect top words and remove "junk" words -
# Health inequalities
common_words_health <- 
  word_frequency(news_health)

View(common_words_health)

# Disasters and emergencies
common_words_disasters <- 
  word_frequency(news_disasters)

View(common_words_disasters)

# Displacement and migration
common_words_displacement <- 
  word_frequency(news_displacement)

View(common_words_displacement)

# - Manually fill these out based on the most common words we want to remove -
custom_stop_words <- 
  tibble(word = c("people", "19", "reports", "report", "research", "online",
                  "sky", "guardian", "independent", "bbc", "news", "radio",
                  "red", "cross", "al", "jazeera"))

# ---- Unigrams ----
#' Plot unigrams
#' @param news A tibble/dataframe containing the text to parse
#' @param custom_stop_words A tibble/dataframe containing custom stop words
#' @param n_to_display Plot words appearing at least this number of times
#' @return ggplot of unigrams
plot_unigram <- function(news, custom_stop_words, n_to_display) {
  # Tokenize text column to unigrams
  news_words <-
    news |> 
    unnest_tokens(word, text) |> 
    anti_join(stop_words) |> 
    anti_join(custom_stop_words)
  
  # Plot word counts
  plt <- 
    news_words |> 
    count(word, sort = TRUE) |> 
    filter(n >= n_to_display) |> 
    mutate(word = reorder(word, n)) |> 
    ggplot(aes(n, word)) +
    geom_col(fill = "#D0021B", alpha = 0.5, show.legend = FALSE) +
    labs(
      x = "Number of times mentioned",
      y = NULL
    ) +
    theme_classic()
  
  plt
}

# - Health inequalities -
plot_unigram(news_health, custom_stop_words, 3)
ggsave("output/news-health-word-counts.png", width = 100, height = 100, units = "mm")

# - Disasters and emergencies -
plot_unigram(news_disasters, custom_stop_words, 2)
ggsave("output/news-disasters-word-counts.png", width = 100, height = 100, units = "mm")

# - Displacement and migration -
plot_unigram(news_displacement, custom_stop_words, 2)
ggsave("output/news-displacement-word-counts.png", width = 100, height = 100, units = "mm")

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
