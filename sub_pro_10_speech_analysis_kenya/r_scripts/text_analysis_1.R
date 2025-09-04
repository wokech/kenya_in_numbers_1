# Install packages if needed
# install.packages(c("pdftools", "tidyverse", "tidytext", "ggplot2", "wordcloud", "tm"))

library(pdftools)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(tm)

# 1. Load PDF
pdf_path <- "sub_pro_10_speech_analysis_kenya/datasets/ruto_speeches/jamhuri_day_celebrations_speech_2022.pdf"

speech_pages <- pdf_text(pdf_path)

# Convert into a data frame with page numbers
speech_df <- tibble(page = seq_along(speech_pages),
                    text = speech_pages)


# Tokenize words
tidy_speech <- speech_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)  # remove stopwords

# 3. Word Frequency Analysis
word_freq <- tidy_speech %>%
  count(word, sort = TRUE)

head(word_freq, 20)

# Plot top 15 words
word_freq %>%
  top_n(15, n) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Most Frequent Words in Jamhuri Day Speech 2022",
       x = "Word", y = "Frequency")

# 4. Wordcloud
set.seed(123)
wordcloud(words = word_freq$word,
          freq = word_freq$n,
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))

# 5. Bigrams (common two-word phrases)
bigrams <- speech_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigram_freq <- bigrams %>%
  count(bigram, sort = TRUE)

head(bigram_freq, 20)

# 6. Sentiment Analysis (using Bing lexicon)
sentiments <- tidy_speech %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment)

ggplot(sentiments, aes(sentiment, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sentiment Analysis of Speech")

# 7. TF-IDF by paragraph (optional for deeper analysis)
speech_paragraphs <- tibble(paragraph = unlist(strsplit(speech_text, "\n\n"))) %>%
  mutate(id = row_number())

tidy_paragraphs <- speech_paragraphs %>%
  unnest_tokens(word, paragraph) %>%
  anti_join(stop_words)

tfidf <- tidy_paragraphs %>%
  count(id, word, sort = TRUE) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))

head(tfidf, 20)

