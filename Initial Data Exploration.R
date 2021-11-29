# Import library
library(tidyverse)
library(wordcloud2)
library(tm)
library(tidytext)

# Import the scraped data
dataall5 <- read_delim("5pages_27Nov2021-16.10.csv", delim = ";")
View(dataall5)
colnames(dataall5)
str(dataall5)

# Replace all NaN value to NA
dataall5$salary <- unlist(map(dataall5$salary, 
                              function(x) if(x == "NaN") return(NA) else return(x)))
View(dataall5)

# =============================================================================
# Creating a word cloud for certain description
# Cleaning the data
text <- dataall5$description[1] %>% 
  tolower() %>% 
  gsub("https\\S*", "", .) %>% 
  gsub("@\\S*", "", .) %>% 
  gsub("amp", "", .) %>% 
  gsub("[\r\n]", "", .) %>% 
  gsub("[[:punct:]]", "", .)
text_corpus <- Corpus(VectorSource(text))
text_corpus <- tm_map(text_corpus, removeWords, stopwords())

# Create required data type for word cloud
dtm <- TermDocumentMatrix(text_corpus)
text_matrix <- as.matrix(dtm)
words <- sort(rowSums(text_matrix), decreasing = T)
df <- data.frame(word = names(words), freq = words)

# Word cloud
wordcloud2(df)
# =============================================================================



# Bar chart for location
location <- dataall5 %>% 
  group_by(location) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))

mutate(location, name = fct_reorder(location, n)) %>% 
  ggplot(aes(x = name, y = n)) + 
  geom_bar(stat = "identity") +
  coord_flip() + 
  xlab("") + 
  theme_minimal()

# Check the company
company <- dataall5 %>% 
  group_by(company) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  top_n(10, n)
# Bar chart for the company
company %>% 
  mutate(name = fct_reorder(company, n)) %>% 
  ggplot(aes(x = name, y = n)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  theme_minimal()
