# Analyze Speech as One Document

# Install if needed
# install.packages(c("officer","tidyverse","tidytext","tm","wordcloud","topicmodels","broom"))

library(officer)
library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud)
library(topicmodels)
library(broom)

# 1. Load Word doc with officer
doc_path <- "sub_pro_10_speech_analysis_kenya/datasets/ruto_speeches/jamhuri_day/jamhuri_day_celebrations_speech_2024.docx"
doc <- read_docx(doc_path)

# officer extracts structured info
doc_summary <- docx_summary(doc)

# Get only paragraphs
speech_text <- doc_summary %>%
  filter(content_type == "paragraph") %>%
  pull(text)

# Collapse into one big document
speech_text <- paste(speech_text, collapse = " ")

speech_df <- tibble(doc_id = 1, text = speech_text)

# 2. Tokenize words
data("stop_words")

tidy_speech <- speech_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, "^[0-9]+$"))

# 3. Word frequency
word_freq <- tidy_speech %>%
  count(word, sort = TRUE)

head(word_freq, 20)

# Plot top 15 words
word_freq %>%
  slice_max(n, n = 15) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Most Frequent Words in Speech (Full Document)",
       x = "Word", y = "Frequency")

# 4. Wordcloud
set.seed(123)
wordcloud(words = word_freq$word,
          freq = word_freq$n,
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))

# 5. Sentiment (overall)
bing <- get_sentiments("bing")

sentiment_summary <- tidy_speech %>%
  inner_join(bing, by = "word") %>%
  count(sentiment)

print(sentiment_summary)

ggplot(sentiment_summary, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Overall Sentiment of the Speech",
       x = "Sentiment", y = "Word Count")

# 6. Topic modeling workaround: chunk text into ~200-word segments
words <- strsplit(speech_text, "\\s+")[[1]]
chunks <- split(words, ceiling(seq_along(words)/200))
chunk_texts <- map_chr(chunks, paste, collapse = " ")

chunk_df <- tibble(doc_id = seq_along(chunk_texts), text = chunk_texts)

tidy_chunks <- chunk_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Build DTM
dtm <- tidy_chunks %>%
  count(doc_id, word) %>%
  cast_dtm(doc_id, word, n)

# Run LDA (choose k topics, e.g. 3)
lda_model <- LDA(dtm, k = 3, method = "Gibbs", control = list(seed = 1234))

# Extract top terms per topic
topics <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10)

print(topics)
