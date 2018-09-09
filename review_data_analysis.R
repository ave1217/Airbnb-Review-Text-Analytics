library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)

df <- read.csv("reviews_detailed.csv", stringsAsFactors = FALSE)
glimpse(df)

review.corpus <- Corpus(VectorSource(df$comments))
review.corpus <- tm_map(review.corpus, content_transformer(tolower))
review.corpus <- tm_map(review.corpus, removePunctuation)
review.corpus <- tm_map(review.corpus, removeWords, stopwords('english'))
review.corpus <- tm_map(review.corpus, removeWords, c("stay", "great", "place"))
review.corpus <- tm_map(review.corpus, stemDocument)

dtm <- TermDocumentMatrix(review.corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 100)

wordcloud(review.corpus, max.words = 100, random.order = FALSE)

listing.df <- read.csv("listings_detailed.csv", stringsAsFactors = FALSE)
glimpse(listing.df)

# listing id with rating below 90
listing.lowrating <- unique(listing.df[listing.df$review_scores_rating < 90,]$id)
listing.lowrating <- listing.lowrating[!is.na(listing.lowrating)]

# 
review.lowrating.df <- df[df$listing_id %in% listing.lowrating,]
review.low.corpus <- Corpus(VectorSource(review.lowrating.df$comments))
review.low.corpus <- tm_map(review.low.corpus, content_transformer(tolower))
review.low.corpus <- tm_map(review.low.corpus, removePunctuation)
review.low.corpus <- tm_map(review.low.corpus, removeWords, stopwords('english'))
review.low.corpus <- tm_map(review.low.corpus, stemDocument)
wordcloud(review.low.corpus, max.words = 100, random.order = FALSE)

# select popular listings
listing.popular.df <- 
  listing.df %>%
  filter(review_scores_rating >= 95, number_of_reviews > 50)

# description
description.corpus <- Corpus(VectorSource(listing.popular.df$description))
description.corpus <- tm_map(description.corpus, content_transformer(tolower))
description.corpus <- tm_map(description.corpus, removePunctuation)
description.corpus <- tm_map(description.corpus, removeWords, stopwords('english'))
description.corpus <- tm_map(description.corpus, stemDocument)
wordcloud(description.corpus, max.words = 100, random.order = FALSE)

# neighborhood overview
neighborhood.corpus <- Corpus(VectorSource(listing.popular.df$neighborhood_overview))
neighborhood.corpus <- tm_map(neighborhood.corpus, content_transformer(tolower))
neighborhood.corpus <- tm_map(neighborhood.corpus, removePunctuation)
neighborhood.corpus <- tm_map(neighborhood.corpus, removeWords, stopwords('english'))
neighborhood.corpus <- tm_map(neighborhood.corpus, stemDocument)
wordcloud(neighborhood.corpus, max.words = 100, random.order = FALSE)

# transit
transit.corpus <- Corpus(VectorSource(listing.popular.df$transit))
transit.corpus <- tm_map(transit.corpus, content_transformer(tolower))
transit.corpus <- tm_map(transit.corpus, removePunctuation)
transit.corpus <- tm_map(transit.corpus, removeWords, stopwords('english'))
transit.corpus <- tm_map(transit.corpus, stemDocument)
wordcloud(transit.corpus, max.words = 100, random.order = FALSE)

# access
access.corpus <- Corpus(VectorSource(listing.popular.df$access))
access.corpus <- tm_map(access.corpus, content_transformer(tolower))
access.corpus <- tm_map(access.corpus, removePunctuation)
access.corpus <- tm_map(access.corpus, removeWords, stopwords('english'))
access.corpus <- tm_map(access.corpus, stemDocument)
wordcloud(access.corpus, max.words = 100, random.order = FALSE)

# house rule
rule.corpus <- Corpus(VectorSource(listing.popular.df$house_rules))
rule.corpus <- tm_map(rule.corpus, content_transformer(tolower))
rule.corpus <- tm_map(rule.corpus, removePunctuation)
rule.corpus <- tm_map(rule.corpus, removeWords, stopwords('english'))
rule.corpus <- tm_map(rule.corpus, stemDocument)
wordcloud(rule.corpus, max.words = 100, random.order = FALSE)

# interaction
interaction.corpus <- Corpus(VectorSource(listing.popular.df$interaction))
interaction.corpus <- tm_map(interaction.corpus, content_transformer(tolower))
interaction.corpus <- tm_map(interaction.corpus, removePunctuation)
interaction.corpus <- tm_map(interaction.corpus, removeWords, stopwords('english'))
interaction.corpus <- tm_map(interaction.corpus, stemDocument)
wordcloud(interaction.corpus, max.words = 100, random.order = FALSE)

# amenities
# amenities.corpus <- Corpus(VectorSource(listing.popular.df$amenities))
# amenities.corpus <- tm_map(amenities.corpus, content_transformer(tolower))
# amenities.corpus <- tm_map(amenities.corpus, removePunctuation)
# amenities.corpus <- tm_map(amenities.corpus, removeWords, stopwords('english'))
# amenities.corpus <- tm_map(amenities.corpus, stemDocument)
# wordcloud(amenities.corpus, max.words = 100, random.order = FALSE)
