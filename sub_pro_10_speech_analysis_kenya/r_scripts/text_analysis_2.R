# Paragraph-Level Speech Analysis

# ---------------------------
# INSTALL PACKAGES IF NEEDED
# ---------------------------
# install.packages(c("officer","tidyverse","tidytext","topicmodels","tm","ggplot2","wordcloud","broom"))

library(officer)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)
library(ggplot2)
library(wordcloud)
library(broom)

# ---------------------------
# 1. READ SPEECH FROM WORD
# ---------------------------
doc_path <- "sub_pro_10_speech_analysis_kenya/datasets/ruto_speeches/jamhuri_day/jamhuri_day_celebrations_speech_2024.docx"  # change to your file name
doc <- read_docx(doc_path)

# Extract text and keep only paragraphs
speech_text <- docx_summary(doc) %>%
  filter(content_type == "paragraph") %>%
  pull(text)

# Remove blanks, trim whitespace
speech_text <- speech_text %>%
  str_trim() %>%
  discard(~ .x == "")

# Data frame: each paragraph = one document
speech_df <- tibble(par_id = seq_along(speech_text),
                    text = speech_text)

# ---------------------------
# 2. TOKENIZE WORDS
# ---------------------------
data("stop_words")  # from tidytext

tidy_words <- speech_df %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word,
         !str_detect(word, "^[0-9]+$"))

# ---------------------------
# 3. SENTIMENT OVER TIME
# ---------------------------
# Bing sentiment (positive/negative)
bing <- get_sentiments("bing")

sentiment_by_par <- tidy_words %>%
  inner_join(bing, by = "word") %>%
  count(par_id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment_score = positive - negative)

# Ensure all paragraphs included (fill 0 if no sentiment words)
sentiment_by_par <- speech_df %>%
  select(par_id) %>%
  left_join(sentiment_by_par, by = "par_id") %>%
  replace_na(list(positive = 0, negative = 0, sentiment_score = 0))

# Plot sentiment trajectory
ggplot(sentiment_by_par, aes(x = par_id, y = sentiment_score)) +
  geom_line(color = "darkgreen") +
  geom_point() +
  labs(title = "Sentiment Trajectory Across Paragraphs",
       x = "Paragraph", y = "Sentiment Score (Positive - Negative)") +
  theme_minimal()

# ---------------------------
# 4. TOPIC MODELING (LDA)
# ---------------------------
# Build document-term matrix
dtm <- tidy_words %>%
  count(par_id, word) %>%
  cast_dtm(document = par_id, term = word, value = n)

# Optional: remove sparse terms if too big
dtm <- removeSparseTerms(dtm, sparse = 0.99)

# Run LDA with chosen k topics (try 3–5)
k <- 4
lda_model <- LDA(dtm, k = k, method = "Gibbs",
                 control = list(seed = 1234, burnin = 2000, iter = 2000, thin = 200))

# ---------------------------
# 5. EXTRACT TOPIC TERMS
# ---------------------------
topic_terms <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  arrange(topic, -beta)

print(topic_terms)

# Nicely summarized top terms per topic
topic_terms %>%
  group_by(topic) %>%
  summarise(top_terms = paste(term, collapse = ", ")) %>%
  arrange(topic)

# ---------------------------
# 6. TOPIC DISTRIBUTIONS PER PARAGRAPH
# ---------------------------
topic_gamma <- tidy(lda_model, matrix = "gamma") %>%
  rename(par_id = document) %>%
  mutate(par_id = as.integer(par_id))

# Plot topic proportions across paragraphs
ggplot(topic_gamma, aes(x = par_id, y = gamma, color = factor(topic))) +
  geom_line() +
  labs(title = "Topic Proportions Across Paragraphs",
       x = "Paragraph", y = "Topic Share (γ)", color = "Topic") +
  theme_minimal()

# ---------------------------
# 7. COMBINE SENTIMENT + TOPIC
# ---------------------------
# Get dominant topic per paragraph
dominant_topic <- topic_gamma %>%
  group_by(par_id) %>%
  slice_max(gamma, n = 1) %>%
  ungroup() %>%
  select(par_id, topic)

# Merge with sentiment
combined <- sentiment_by_par %>%
  left_join(dominant_topic, by = "par_id")

# Plot sentiment by dominant topic
ggplot(combined, aes(x = par_id, y = sentiment_score, color = factor(topic))) +
  geom_point(size = 2) +
  geom_line(aes(group = 1), alpha = 0.3) +
  labs(title = "Sentiment Score Per Paragraph, Colored by Dominant Topic",
       x = "Paragraph", y = "Sentiment Score", color = "Topic") +
  theme_minimal()
