library(tidyverse)
library(tidytext)
library(topicmodels)
library(ldatuning)
library(viridis)
library(ggfittext)
library(lubridate)

# ---- Load and prep data ----
# Load news from the last three months
# Note that the articles/snippets appear in reverse chronological order (at least when Matt has gathered the data)
read_news <- function(file) {
  # Get date from the filename
  news_month <- str_extract(file, "[0-9]+-[0-9]+") |> ym()
  
  read_lines(file) |> 
    as_tibble() |> 
    rename(text = value) |> 
    filter(text != "") |> 
    mutate(Date = news_month) |> 
    relocate(Date)
}

news <- 
  list.files(path = "data/daily-news/", pattern = "*.txt", full.names = TRUE) |> 
  map_df(~read_news(.))

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
  filter(first_word %in% c("Health", "Disasters", "Displacement")) |> 
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
#' @param cause_name Name of the cause, for the plot title
#' @return ggplot of unigrams
plot_unigram <- function(news, custom_stop_words, n_to_display, cause_name) {
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
      y = NULL,
      title = glue::glue("Most frequently mentioned words in {cause_name}")
    ) +
    theme_classic()
  
  plt
}

# - Health inequalities -
plot_unigram(news_health, custom_stop_words, 7, "Health Inequalities")
ggsave("output/news-health-word-counts.png", width = 200, height = 100, units = "mm")

# - Disasters and emergencies -
plot_unigram(news_disasters, custom_stop_words, 7, "Disasters & Emergencies")
ggsave("output/news-disasters-word-counts.png", width = 200, height = 100, units = "mm")

# - Displacement and migration -
plot_unigram(news_displacement, custom_stop_words, 7, "Displacement & Migration")
ggsave("output/news-displacement-word-counts.png", width = 200, height = 100, units = "mm")

# ---- Bigrams ----
#' Plot bigrams
#' @param news A tibble/dataframe containing the text to parse
#' @param custom_stop_words A tibble/dataframe containing custom stop words
#' @param n_to_display Plot words appearing at least this number of times
#' @param cause_name Name of the cause, for the plot title
#' @return ggplot of bigrams
plot_bigram <- function(news, custom_stop_words, n_to_display, cause_name) {
  bigrams <- 
    news |> 
    unnest_tokens(bigram, text, token = "ngrams", n = 2) |> 
    separate(bigram, c("word1", "word2"), sep = " ") |> 
    filter(!word1 %in% c(stop_words$word, custom_stop_words$word)) |>
    filter(!word2 %in% c(stop_words$word, custom_stop_words$word)) |> 
    unite(bigram, word1, word2, sep = " ")
  
  # Plot
  plt <- 
    bigrams |> 
    count(bigram, sort = TRUE) |> 
    filter(n >= n_to_display) |>
    mutate(bigram = reorder(bigram, n)) |>
    ggplot(aes(n, bigram)) +
    geom_col(fill = "#D0021B", alpha = 0.5, show.legend = FALSE) +
    labs(
      x = "Number of times mentioned",
      y = NULL,
      title = glue::glue("Most frequently mentioned word pairs in {cause_name}")
    ) +
    theme_classic()
  
  plt
}

# - Health inequalities -
plot_bigram(news_health, custom_stop_words, 3, "Health Inequalities")
ggsave("output/news-health-bigrams.png", width = 200, height = 100, units = "mm")

# - Disasters and emergencies -
plot_bigram(news_disasters, custom_stop_words, 3, "Disasters & Emergencies")
ggsave("output/news-disasters-bigrams.png", width = 200, height = 100, units = "mm")

# - Displacement and migration -
plot_bigram(news_displacement, custom_stop_words, 3, "Displacement & Migration")
ggsave("output/news-displacement-bigrams.png", width = 200, height = 100, units = "mm")

# ---- Topic models ----
#' Document term matrices for each cause
#' @param news A tibble/dataframe containing the text to parse
#' @return DocumentTermMatrix
get_dtm <- function(news) {
  news |> 
    unnest_tokens(word, text) |> 
    anti_join(stop_words) |> 
    anti_join(custom_stop_words) |> 
    rownames_to_column() |> 
    count(rowname, word) |> 
    cast_dtm(rowname, word, n)
}

dtm_health <-
  get_dtm(news_health)

dtm_disasters <-
  get_dtm(news_disasters)

dtm_displacement <-
  get_dtm(news_displacement)

# - Work out how many topics (k) to calculate for each cause (have to do this a bit manually) -
# Select number of topics (k) for LDA model using the 'ldatuninig' package.
lda_fit <-
  FindTopicsNumber(
    # Choose one:
    # dtm_health,
    # dtm_disasters,
    dtm_displacement,
    
    topics = seq(from = 2, to = 10, by = 1),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs", control = list(seed = 77), mc.cores = 2L, verbose = TRUE
  )

# find the extremum to determine optimal k
FindTopicsNumber_plot(lda_fit)

#' Plot topics
#' @param dtm The DocumentTermMatrix to use
#' @param k Number of topics to model
#' @param cause_name Name of the cause, for the plot title
#' @return ggplot of topics
plot_topics <- function(dtm, k, cause_name) {
  lda <- LDA(dtm, k = k, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  top_terms <- 
    topics |>
    group_by(topic) |>
    top_n(10, beta) |>
    ungroup() |>
    arrange(topic, -beta)
  
  # - plot -
  plt <- 
    top_terms |>
    mutate(term = reorder_within(term, beta, topic)) |>
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~topic, nrow = 2, ncol = 5, scales = "free") +
    scale_y_reordered() +
    theme_minimal() +
    scale_fill_viridis(discrete = T, begin = .15, end = .85, alpha = .8) + 
    labs(y = NULL,
         title = glue::glue("Naturally occuring topics related to {cause_name}"),
         subtitle = "The beta value is the probability that any given word belongs to a topic",
         caption = "Colour denotes topic number.")
  
  plt
}

# - Health inequalities - 
plot_topics(dtm_health, k = 5, "Health inequalities")
ggsave("output/news-health-topics.png", width = 250, height = 150, units = "mm")

# - Disasters and emergencies - 
plot_topics(dtm_disasters, k = 4, "Disasters and emergencies")
ggsave("output/news-disasters-topics.png", width = 250, height = 150, units = "mm")

# - Displacement and migration - 
plot_topics(dtm_displacement, k = 4, "Displacement and migration")
ggsave("output/news-displacement-topics.png", width = 250, height = 150, units = "mm")

# ---- Further exploration ----
news_health |> 
  filter(str_detect(text, "pregnant"))

news_health |> 
  filter(str_detect(text, "cancer") & str_detect(text, "mental"))

news_disasters |> 
  filter(str_detect(text, "water"))

news_displacement |> 
  filter(str_detect(text, "violen"))

# ---- TF-IDF ----
book_words <- 
  news_health |> 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) |> 
  anti_join(custom_stop_words) |> 
  count(Date, word, sort = TRUE)

total_words <- 
  book_words %>% 
  group_by(Date) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_tf_idf <- 
  book_words |> 
  bind_tf_idf(word, Date, n)

book_tf_idf %>%
  group_by(Date) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Date)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Date, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)
