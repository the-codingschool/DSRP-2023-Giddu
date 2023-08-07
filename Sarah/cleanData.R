data <- readRDS("data/USvideos.RDS")

library(dplyr)
small_data <- slice_sample(data, n = 5000)
# Assuming you have a data frame called "df" with a column "description"
library(tm)

# Step 1: Preprocess the text data
corpus <- Corpus(VectorSource(small_data$description))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

# Step 2: Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)

# Step 3: Calculate term frequency
term_freq <- rowSums(as.matrix(tdm))

# Step 4: Identify most common keywords (top 10, for example)
top_keywords <- head(sort(term_freq, decreasing = TRUE), 100)


jimmyVids <- filter(data, grepl("jimmy", tolower(description)))
jamesVids <- filter(data, grepl("james", tolower(description)))



