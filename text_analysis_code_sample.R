library(igraph)
library(ggraph)
library(ggplot2)
library(udpipe)
library(SnowballC)
library(tidytext)
library(tidyverse)
library(cowplot)
library(nametagger)
library(glue)
library(sentimentr)
library(rvest)

## Analyze sentiment of Brookings opinion pieces ----

# Create Function to get, parse and format Brookings opinion piece ----
brookings.scrape <- function(opinion_title) {
  
  # Remove punctuation and replace space with dash
  rm_punct <- str_replace_all(opinion_title, "[:punct:]\\s", " ")
  rm_punct2 <- tolower(str_replace_all(rm_punct, "[:punct:]", ""))
  rm_space <- str_replace_all(rm_punct2, " ", "-")
  
  
  # Scrape article and format
  brookings_url <- paste0("https://www.brookings.edu/opinions/", rm_space)
  html <- read_html(brookings_url)
  body <- html_nodes(html, "[class='post-body post-body-enhanced'] p") 
  content <- html_text(body[-1], trim = TRUE) 
  
  paste(content, collapse = " \n\n")
}


# Select pieces for scrape. ----
# Both are written by Vanda Felbab-Brown. The first is an overview of president Juan Manuel Santos' administration's national security strategy, and the second is an 
# analysis of anti-urban violence in Medellin. Henceforth, the former will be described as national, and the latter as medellin.

brookings_articles <- c("Reducing Urban Violence: Lessons from Medellin, Colombia", "Colombia's Consolidation: Everything Coming up Orchids?")

# Create for loop to read and format several articles ----
brookings_list <- vector("list", length(brookings_articles))

for (i in seq_along(brookings_articles)) {
  brookings_list[[i]] <- brookings.scrape(brookings_articles[[i]])
  names(brookings_list) <- brookings_articles
}



## What is the overall sentiment? ----

# Process data for analysis ----
brookings_df <- vector("list", length(brookings_list))
brookings_words <- vector("list", length(brookings_list))

for (i in seq_along(brookings_list)) {
  
  brookings_df[[i]] <- tibble(text = brookings_list[[i]])
  brookings_words[[i]] <- unnest_tokens(brookings_df[[i]], word_tokens,  text, token = "words")
}


# Remove stop words ----
no_stop <- vector("list", length(brookings_words))

for (i in seq_along(brookings_words)) {
  
  no_stop[[i]] <- brookings_words[[i]] %>% 
    anti_join(stop_words, by = c("word_tokens" = "word")) %>% 
    rename(text = word_tokens)
}

medellin_no_stop <- no_stop[[1]] 
national_no_stop <- no_stop[[2]]


## Top words and Lemmas ----
brookings_top_words <- vector("list", length(no_stop))
no_stop_parsed <- vector("list", length(no_stop))
brookings_top_lemmas <- vector("list", length(no_stop))

for (i in seq_along(no_stop)) {
  
  # Words
  brookings_top_words[[i]] <- no_stop[[i]] %>% 
    dplyr::count(text, sort = TRUE) %>% 
    head(15)
  
  # lemmas
  no_stop_parsed[[i]] <- udpipe(as.list(no_stop[[i]]), "english") # must use list of words in udpipe
  brookings_top_lemmas[[i]] <- no_stop_parsed[[i]] %>% 
    dplyr::count(lemma, sort = TRUE) %>% 
    head(15)
}

(medellin_top_words <- brookings_top_words[[1]])
(medellin_top_lemmas <- brookings_top_lemmas[[1]])
(national_top_words <- brookings_top_words[[2]])
(national_top_lemmas <- brookings_top_lemmas[[2]])

## Analyze overall sentiment for each article using three sentiment libraries ----

# This nested for loop does sentiment analysis for each article using the three sentiment libraries

sentiment_libraries <- c("nrc", "bing", "afinn")
articles <- c("Medellin", "National")
all_sentiment_tibbles <- vector("list", length(no_stop))
all_sentiment_top <- vector("list", length(no_stop))
all_sentiment_plots <- vector("list", length(no_stop))


for (i in seq_along(no_stop)) {
  
  all_sentiment_tibbles[[i]] <- vector("list", 3)
  all_sentiment_top[[i]] <- vector("list", 3)
  all_sentiment_plots[[i]] <- vector("list", 3)
  
  
  for (j in seq_along(sentiment_libraries)) {
    
    # Join sentiment libraries with word list from each article without stop words
    all_sentiment_tibbles[[i]][[j]] <- no_stop[[i]] %>% 
      left_join(get_sentiments(sentiment_libraries[[j]]), by = c("text" = "word")) %>% 
      rename(word = 1, sentiment = 2)
    
    # Order top sentiments for each article
    all_sentiment_top[[i]][[j]] <- all_sentiment_tibbles[[i]][[j]] %>% 
      dplyr::filter(is.na(sentiment) == F) %>% 
      dplyr::count(sentiment) %>% 
      dplyr::arrange(desc(n)) %>% 
      dplyr::rename(total = n)
    
    
    names(all_sentiment_tibbles) <- c("medellin", "national")
    names(all_sentiment_tibbles[[i]]) <- sentiment_libraries
    
    names(all_sentiment_top) <- c("medellin", "national")
    names(all_sentiment_top[[i]]) <- sentiment_libraries

  }
}


# Plot sentiment comparison between both articles ----
top_medellin <- all_sentiment_top[[1]]
top_national <- all_sentiment_top[[2]]

long_dataframes <- vector("list", length(sentiment_libraries))
sentiment_plots <- vector("list", length(sentiment_libraries))

for (i in seq_along(sentiment_libraries)) {
  
  long_dataframes[[i]] <- top_medellin[[i]] %>% 
    full_join(top_national[[i]], by = "sentiment") %>% 
    rename(medellin = total.x, national = total.y) %>% 
    pivot_longer(cols = 2:3, names_to = "article", values_to = "total")
  
  sentiment_plots[[i]] <- ggplot(long_dataframes[[i]]) + 
    geom_col(aes(x = sentiment, y = total, fill = article), position = "dodge") +
    ggtitle(glue("{toupper(sentiment_libraries[[i]])} Sentiment Analysis for Articles"))
}

plt_sentiment <- plot_grid(plotlist = sentiment_plots, nrow = 3)
plt_sentiment

## Cooccurrence Analysis ----

article_parsed <- vector("list", length(brookings_list))
article_dependencies <- vector("list", length(brookings_list))
top_cooccurrence <- vector("list", length(brookings_list))
corr_cooccurrence <- vector("list", length(brookings_list))


for (i in seq_along(brookings_list)){
  
  article_parsed[[i]] <- udpipe(brookings_list[[i]], "english")
  
  article_dependencies[[i]] <- article_parsed[[i]] %>% 
    cbind_dependencies(type = "parent_rowid", recursive = TRUE) %>% 
    filter(upos %in% c("VERB", "ADJ", "PROPN", "NOUN", "ADV"))
  
  # Top cooccurrences
  top_cooccurrence[[i]] <- head(cooccurrence(x = article_dependencies[[i]], 
                                             term = "lemma",
                                             group = "doc_id"), 30)
  
  # Cooccurrence correlation
  corr_cooccurrence[[i]] <- article_dependencies[[i]] %>% 
    document_term_frequencies(document = "sentence_id", term = "lemma") %>% 
    document_term_matrix() %>% 
    dtm_remove_lowfreq(minfreq = 1) %>% 
    dtm_cor() %>% 
    as_cooccurrence() %>% 
    arrange(desc(cooc)) %>% 
    filter(cooc <= 0.99)
}

(cooc_medelliin <- top_cooccurrence[[1]])
(cooc_national <- top_cooccurrence[[2]])
head(corr_medellin <- corr_cooccurrence[[1]], 20)
head(corr_national <- corr_cooccurrence[[2]], 20)



## What was said about the police, security and violence? ----

# Build function to extract word relations given a certain word

get_word_relations <- function(word) {
  
  list(sentences_medellin = article_dependencies[[1]] %>% 
         filter(token == word) %>% 
         select(sentence),  
       correlations_medellin = head(corr_cooccurrence[[1]] %>% 
         filter(term1 == word | term2 == word), 30),
       sentences_national = article_dependencies[[2]] %>% 
         filter(token == word) %>% 
         select(sentence),  
       correlations_national = head(corr_cooccurrence[[2]] %>% 
         filter(term1 == word | term2 == word), 30)
       )
}

words_of_interest <- c("police", "security", "violence")
word_relations <- vector("list", length(words_of_interest))

for (i in seq_along(words_of_interest)) {
  
  word_relations[[i]] <- get_word_relations(words_of_interest[[i]])
  
  names(word_relations) <- words_of_interest 
}











  









