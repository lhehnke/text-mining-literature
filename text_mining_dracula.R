##------------------------------------------------------------------------------------------##
##                          TEXT MINING: DRACULA (BRAM STOKER)                              ##
##------------------------------------------------------------------------------------------##


## R version 3.3.1 (2016-06-21)


#-------#
# Setup #
#-------#

# Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Load packages
p_load(dplyr, ggplot2, gutenbergr, magrittr, stringr, reshape2, tidyr, tidytext, wordcloud)


#---------------#
# Download text #
#---------------#

# Get corresponding gutenberg_id
stoker <- gutenberg_works(author == "Stoker, Bram")
dracula_id <- stoker[1, "gutenberg_id"]

# Download text
dracula <- gutenberg_download(dracula_id)


#---------------#
# Text cleaning #
#---------------#

# Remove redundant parts
dracula_df <- dracula[162:15482, ]
dracula_df <- dracula_df[, "text"]

# Remove blank lines from text
dracula_df %<>% filter(text != "") 

# Unnest and tokenize text
dracula_tidy <- dracula_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Remove numbers and punctuations
dracula_tidy %<>%
  filter(!grepl("_", word)) %>%
  filter(!grepl("[0-9]", word))


#------------------#
# Word frequencies #
#------------------#

# Find most common words
dracula_wordfreq <- dracula_tidy %>%
  count(word, sort = TRUE)

# Plot words
dracula_tidy %>%
  count(word, sort = TRUE) %>%
  filter(n > 150) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab("Word") + ylab("Frequency") + ggtitle("Most common words in Dracula") +
  coord_flip()


#--------------------#
# Sentiment analysis #
#--------------------#

# Get sentiments (nrc) and join
nrc <- get_sentiments("nrc") 
dracula_nrc <- dracula_tidy %>%
  inner_join(nrc) %>%
  count(word, sort = TRUE)

# Calculate and plot total sentiment scores
nrc_counts <- data.frame(table(nrc$sentiment))

# Plot sentiment scores
ggplot(data = nrc_counts, aes(x = Var1, y = Freq)) +
  geom_bar(aes(fill = Var1), stat = "identity") +
  xlab("Sentiment") + ylab("Count") + ggtitle("Sentiment scores in Dracula") +
  theme(legend.position = "none") 


#------------#
# Wordclouds #
#------------#

# Set color
pal <- brewer.pal(9, "Reds")[-(1:4)] 

# Plot basic wordcloud
dracula_tidy %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, colors = pal, max.words = 80))

# Plot sentiment (bing, i.e., positive/negative) wordcloud
dracula_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("darkred", "red2"), max.words = 80)