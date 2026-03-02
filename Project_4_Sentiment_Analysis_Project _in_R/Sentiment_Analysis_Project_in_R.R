# --- Step 1: Loading the Text Mining Suite ---

# We use pacman to ensure all specialized NLP libraries are installed
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidytext, 
               janeaustenr, 
               dplyr, 
               stringr, 
               tidyr, 
               ggplot2, 
               wordcloud, 
               reshape2)

# Audit: View the available Jane Austen books
austen_books() %>% group_by(book) %>% summarize(total_lines = n())



# --- Step 2: Tokenization & Cleaning ---

# 1. Group the text by book and add line/chapter tracking
tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\\\divlxc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  
# 2. Tokenize: Break the 'text' column into a 'word' column
  unnest_tokens(word, text)

# 3. Audit: See the first few words of our new tidy dataset
head(tidy_books)



# 4. Remove Stop Words using the built-in 'stop_words' dataset
data(stop_words)
tidy_books <- tidy_books %>%
  anti_join(stop_words)

# 5. Check the most common words remaining (The "Clean" list)
tidy_books %>%
  count(word, sort = TRUE)

# --- Step 3: Mapping Emotions with NRC ---


# 1. Fetch the Bing Lexicon (Stable alternative)
bing_lex <- get_sentiments("bing")

# 2. Filter for 'Positive' words (Similar to finding Joy)
pos_words <- bing_lex %>% 
  filter(sentiment == "positive")

# 3. Join with 'Emma' to see the happiest words
emma_positive <- tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(pos_words) %>%
  count(word, sort = TRUE)

# 4. View your success
head(emma_positive, 10)



# 1. Map all words in the books to Positive or Negative using Bing
jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  # 2. Divide the books into chunks of 80 lines to see the flow
  count(book, index = linenumber %/% 80, sentiment) %>%
  # 3. Reshape the data to calculate 'Net Sentiment'
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# 4. Generate the Visualization
ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x") +
  theme_minimal() +
  labs(title = "Sentiment Trajectory in Jane Austen's Novels",
       subtitle = "Calculated using the Bing Lexicon (Positive - Negative)",
       y = "Net Sentiment", x = "Narrative Progress (80-line chunks)")

#----------Step:05 Comparison Word Cloud--------------
library(wordcloud)
library(reshape2)

# Create a cloud that separates Positive (Green) from Negative (Red)
tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("indianred3", "darkolivegreen4"),
                   max.words = 100)



































