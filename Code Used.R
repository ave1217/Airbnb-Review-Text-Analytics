library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(tidytext)
library(ggplot2)
library(lubridate)
library(ngram)

df <- read.csv("reviews_detailed.csv", stringsAsFactors = FALSE)
glimpse(df)

review.corpus <- Corpus(VectorSource(df$comments))
review.corpus <- tm_map(review.corpus, content_transformer(tolower))
review.corpus <- tm_map(review.corpus, removePunctuation)
review.corpus <- tm_map(review.corpus, removeWords, c("stay", "great", "place"))
review.corpus <- tm_map(review.corpus, stemDocument)

wordcloud(review.corpus, max.words = 100, random.order = FALSE)

listing.df <- read.csv("listings_detailed.csv", stringsAsFactors = FALSE)
wordcount <- function(str) {
  sapply(gregexpr("\\b\\W+\\b", str, perl=TRUE), function(x) sum(x>0) ) + 1 
}
# host_since as date
listing.df <- 
  listing.df %>%
  mutate(host_since = mdy(host_since)) %>%
  mutate(first_review = mdy(first_review)) %>%
  mutate(weeks_till_first_review = ceiling(as.duration((host_since %--% first_review))/dweeks(1))) %>%
  mutate(description_wordcount = wordcount(description)) %>%
  mutate(summary_wordcount = wordcount(summary)) %>%
  mutate(space_wordcount = wordcount(space)) %>%
  mutate(neighborhood_overview_wordcount = wordcount(neighborhood_overview)) %>%
  mutate(transit_wordcount = wordcount(transit)) %>%
  mutate(access_wordcount = wordcount(access)) %>%
  mutate(interaction_wordcount = wordcount(interaction)) %>%
  mutate(host_about_wordcount = wordcount(host_about)) %>%
  mutate(house_rules_wordcount = wordcount(house_rules))

df.to.merge <- 
  listing.df %>%
  select(id, number_of_reviews, review_scores_rating) %>%
  filter(number_of_reviews > 0) %>%
  filter(!is.na(review_scores_rating)) %>%
  rename(listing_id = id)

word.merged <- 
  word.df %>%
  merge(df.to.merge, by = "listing_id") %>%
  count(listing_id, id, review_scores_rating, word) %>%
  ungroup()

word.summary <- 
  word.merged %>%
  group_by(word) %>%
  summarise(businesses = n_distinct(listing_id),
            reviews = n(),
            uses = sum(n),
            average_rating = mean(review_scores_rating)) %>%
  ungroup()

word.high.rating.df <- 
  word.summary %>%
  filter(uses > 100, reviews >= 100, businesses >= 100) %>%
  arrange(desc(average_rating)) %>%
  slice(1:200)

word.high.rating.df

word.low.rating.df <-
  word.summary %>%
  filter(uses > 10, reviews >= 10, businesses >= 10) %>%
  arrange(average_rating) %>%
  slice(1:200)

word.low.rating.df
wordcloud(word.low.rating.df$word, word.low.rating.df$uses, max.words = 100, random.order = FALSE)

word.summary.filtered <-
  word.summary %>%
  filter(uses > 50, reviews >= 50, businesses >= 20)

ggplot(word.summary.filtered, aes(reviews, average_rating)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(word.summary.filtered$average_rating), color = "red", lty = 2) +
  xlab("# of reviews") +
  ylab("Average Rating")

# plot positive
ggplot(word.high.rating.df, aes(reviews, average_rating)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  #geom_hline(yintercept = mean(word.summary.filtered$average_rating), color = "red", lty = 2) +
  xlab("# of reviews") +
  ylab("Average Rating")

# plot negative 
ggplot(word.low.rating.df, aes(reviews, average_rating)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  #geom_hline(yintercept = mean(word.summary.filtered$average_rating), color = "red", lty = 2) +
  xlab("# of reviews") +
  ylab("Average Rating")

# analysis of host description
dscrpt.df <-
  listing.df %>%
  filter(year(host_since) <= 2017) %>%
  select(id, summary, description, space, neighborhood_overview
         , transit, access, interaction, host_about
         , number_of_reviews, review_scores_rating) %>%
  unnest_tokens(word, description) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

dscrpt.summary <- 
  dscrpt.df %>%
  filter(!is.na(review_scores_rating)) %>%
  count(id, review_scores_rating, number_of_reviews, word) %>%
  ungroup() %>%
  group_by(word) %>%
  summarise(listings = n()
            , uses = sum(n)
            , average_rating = mean(review_scores_rating)
            , median_number_reviews = median(number_of_reviews)) %>%
  ungroup()

dscrpt.summary.filtered <- 
  dscrpt.summary %>%
  filter(uses >= 50, listings >= 50)

ggplot(dscrpt.summary.filtered, aes(listings, average_rating)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(dscrpt.summary.filtered$average_rating), color = "red", lty = 2) +
  xlab("# of listings") +
  ylab("Average Rating")

ggplot(dscrpt.summary.filtered, aes(listings, median_number_reviews)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(dscrpt.summary.filtered$median_number_reviews), color = "red", lty = 2) +
  xlab("# of listings") +
  ylab("Median Number of Reviews")