#' ____________________
#' Title: Text Mining Mark Twain's Novels
#' Author: Yashada Nikam
#' ____________________


library(dplyr)
library(rvest)
library(purrr)
library(tidytext)
library(gutenbergr)
library(ggplot2)

# Download the 5 novels

twain<- gutenberg_download(c(76, 74, 1837, 3176, 86))

tidy_twain <- twain %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#'
#' Remove common unimportant words like "the" and "and" using stop words. 
#' 
#' The result, tidy_twain, contains only the important words, making it easier to analyze
#' 



#'
#' After removing the stop words, here is a list of words starts from the most frequent.
#' 


tidy_twain %>%
  count(word, sort = TRUE)

tidy_twain %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + ggtitle("The Most Common Words in Mark Twain's Novels")

#'
#' "time" appears 1322 times, "tom" appears 1171 times, "king" appears 903 times, and so on.
#'  
#' These word frequencies provide valuable insights into the themes or topics that are most prevalent 
#' 
#' in Mark Twain's writings.
#' 
#' 

#' Sentiment Analysis
#' 
#' 
#' 

#' get_sentiments("bing"): This function retrieves the Bing lexicon, a pre-defined list of words 
#' 
#' labeled as positive or negative based on sentiment analysis.
#' 
#' 



bing_word_counts <- tidy_twain %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts

#' Sentiment Categories

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Words Contribute to sentiment",
       x = NULL) +
  coord_flip()

#'
#' We can observe which words are most frequently associated with positive sentiments 
#' 
#' (like "pretty" and "beautiful") and which words are more commonly connected to negative 
#' 
#' sentiments (like "dead" and "poor").
#' 
#'


library(RColorBrewer)
library(wordcloud)

tidy_twain%>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


library(reshape2)

tidy_twain %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)


#' Relationships between words
#' 
#' 
#' 
#' 

twain_bigrams <- twain %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

twain_bigrams

library(tidyr)

bigrams_separated <- twain_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

#'
#' Now, each token in the data represents a bigram, meaning two words paired together. 
#' 
#' If either of the words in a bigram is a stop word, that word will be removed from the bigram.
#'  
#' After filtering out these stop words, we are interested in identifying the most frequent bigrams 
#' 
#' that remain in the data.
#'
#'
#'
#'

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united


library(igraph)

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()
bigram_graph


library(ggraph)

set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8) + ggtitle("Common Bigrams in Twain's Novels")



#' Through text mining and sentiment analysis of Mark Twain's novels using R, 
#' 
#' we gained valuable insights into his writing style, frequent word usage, and emotional tone.
#'  
#' This project sheds light on the themes and sentiments prevalent in Twain's literary works, 
#' 
#' providing a deeper understanding and appreciation of his contributions to literature.


