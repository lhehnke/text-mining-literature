##----------------------------------------------------------------------------------##
##                             TEXT MINING: THE ROOM                                ##
##----------------------------------------------------------------------------------##


## R version 3.3.1 (2016-06-21)


#-------#
# Setup #
#-------#

# Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Load packages
p_load(dplyr, ggplot2, magrittr, pdftools, reshape2, stringr, tidytext, wordcloud)


#-------------#
# Import data #
#-------------#

# Download pdf
download.file("https://theroomscriptblog.files.wordpress.com/2016/04/the-room-original-script-by-tommy-wiseau.pdf",
              "the-room-original-script-by-tommy-wiseau.pdf")

# Extract text from pdf file
room <- pdf_text("the-room-original-script-by-tommy-wiseau.pdf")


#---------------#
# Text cleaning #
#---------------#

# Separate lines with \n indicating line breaks
room_tidy <- strsplit(room, "\n")

# Remove cover page
room_tidy <- room_tidy[-1]

# Remove page numbers and header
room_tidy <- lapply(room_tidy, function(x) x[-(1:2)])

# Remove footer
room_tidy <- lapply(room_tidy, function(x) x[1:(length(x)-2)])

# Remove information on act and scene
room_tidy <- lapply(room_tidy, function(x) gsub("END SCENE", "", x))
room_tidy <- lapply(room_tidy, function(x) gsub("ACT.*", "", x))
room_tidy <- lapply(room_tidy, function(x) gsub("SCENE.*", "", x))

# Remove punctuation (except for apostrophes) and numbers
room_tidy <- lapply(room_tidy, function(x) gsub("[^[:alpha:][:blank:]']", "", x))

# Remove stage and character directions 
room_tidy <- lapply(room_tidy, function(x) x[!grepl("^[A-Z ']+$", x), drop = FALSE])

# Convert to lowercase
room_tidy <- lapply(room_tidy, function(x) tolower(x))

# Split strings
room_tidy <- lapply(room_tidy, function(x) strsplit(x, " "))

# Turn list to data.frame 
room_df <- data.frame(matrix(unlist(room_tidy), nrow = 10357, byrow = T), stringsAsFactors = FALSE)

# Remove introductory part and last two lines
room_df <- tail(room_df, -102)
room_df <- head(room_df, -2)

# Rename column to match anti_join(stopwords)
colnames(room_df) <- "word"

# Remove blank lines from text
room_df %<>% filter(word != "") 

# Remove stopwords
room_df %<>% anti_join(stop_words)


#------------------#
# Word frequencies #
#------------------#

# Find most common words
room_df_wordfreq <- room_df %>%
  count(word, sort = TRUE)

# Plot words
room_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab("") + ylab("") + ggtitle("Most common words in The Room", subtitle = "Written by Tommy Wiseau") +
  ylim(0, 100) + coord_flip()


#--------------------#
# Sentiment analysis #
#--------------------#

# Get sentiments (nrc) and join
nrc <- get_sentiments("nrc") 
room_nrc <- room_df %>%
  inner_join(nrc) %>%
  count(word, sort = TRUE)

# Calculate and plot total sentiment scores
nrc_counts <- data.frame(table(nrc$sentiment))

# Plot sentiment scores
ggplot(data = nrc_counts, aes(x = Var1, y = Freq)) +
  geom_bar(aes(fill = Var1), stat = "identity") +
  xlab("Sentiment") + ylab("Count") + ggtitle("Total sentiment scores in The Room", subtitle = "Written by Tommy Wiseau") +
  ylim(0, 4000) + theme(legend.position = "none") 


#-------------------------#
# Positive/negative words #
#-------------------------#

# Calculate contributions to positive and negative sentiments (bing) by word 
bing_counts <- room_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Calculate top 10 word contributors
bing_counts_plot <- bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) 

# Plot most common positive and negative words
ggplot(bing_counts_plot, aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  xlab("") + ylab("Count") + 
  ggtitle("Most common positive and negative words in The Room", subtitle = "Written by Tommy Wiseau") +
  coord_flip()


#------------#
# Wordclouds #
#------------#

# Set color
pal <- brewer.pal(9, "Greys")[-(1:4)] 

# Plot traditional word cloud
room_df %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, colors = pal, max.words = 50))

# Plot comparison cloud with positive and negative sentiments (bing)
room_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray70"), max.words = 60)

# Plot previous comparison cloud in ggplot2 colors
## Run > unique(g$data[[1]]["fill"]) after ggplot_build() to extract colors
room_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#00BFC4", "#F8766D"), max.words = 60)