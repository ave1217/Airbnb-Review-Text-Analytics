# sentiment analysis
library(tidytext)
library(ggplot2)
library(lubridate)
library(ngram)

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

plot(listing.df$weeks_till_first_review, listing.df$number_of_reviews)
  

# description_wordcount
plot(listing.df$description_wordcount, log(listing.df$number_of_reviews))
plot(listing.df$description_wordcount, sqrt(listing.df$review_scores_rating))
# summary_wordcount
plot(listing.df$summary_wordcount, log(listing.df$number_of_reviews))
plot(listing.df$summary_wordcount, sqrt(listing.df$review_scores_rating))
# space_wordcount
plot(listing.df$space_wordcount, log(listing.df$number_of_reviews))
plot(listing.df$space_wordcount, sqrt(listing.df$review_scores_rating))
# neighborhood_overview_wordcount
plot(listing.df$neighborhood_overview_wordcount, log(listing.df$number_of_reviews))
plot(listing.df$neighborhood_overview_wordcount, sqrt(listing.df$review_scores_rating))
# transit_wordcount
plot(listing.df$transit_wordcount, log(listing.df$number_of_reviews))
plot(listing.df$transit_wordcount, sqrt(listing.df$review_scores_rating))
# access_wordcount
plot(listing.df$access_wordcount, log(listing.df$number_of_reviews))
plot(listing.df$access_wordcount, sqrt(listing.df$review_scores_rating))
# interaction_wordcount
plot(listing.df$interaction_wordcount, log(listing.df$number_of_reviews))
plot(listing.df$interaction_wordcount, sqrt(listing.df$review_scores_rating))
# host_about_wordcount
plot(listing.df$host_about_wordcount, log(listing.df$number_of_reviews))
plot(listing.df$host_about_wordcount, sqrt(listing.df$review_scores_rating))
# house_rules_wordcount
plot(listing.df$house_rules_wordcount, sqrt(listing.df$number_of_reviews))
plot(listing.df$house_rules_wordcount, log(listing.df$number_of_reviews))
plot(listing.df$house_rules_wordcount, sqrt(listing.df$review_scores_rating))

#------------------------------------------------------------------------------
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

df <- read.csv("reviews_detailed.csv", stringsAsFactors = FALSE)
glimpse(df)

word.df <-
  df %>%
  unnest_tokens(word, comments) %>%
  filter(!word %in% stop_words$word,
       str_detect(word, "^[a-z']+$"))

# review_sentiment <- word.df %>%
#   inner_join(get_sentiments("bing")) %>%
#   count(listing_id, sentiment) %>%
#   spread(sentiment, n, fill = 0) %>%
#   mutate(sentiment = positive - negative)
# 
# summary(review_sentiment$sentiment)

AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn.score = score)

review.sentiment <- word.df %>%
  inner_join(AFINN, by = "word") %>%
  group_by(id) %>%
  summarize(sentiment = mean(afinn_score))

summary(review.sentiment$sentiment)

review.sentimentp.per.lstng <- word.df %>%
  inner_join(AFINN, by = "word") %>%
  group_by(listing_id) %>%
  summarize(sentiment = mean(afinn_score))

summary(review.sentiment.per.lstng$sentiment)

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

#--------------------------------------------------------------------------------

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

# low rated listing description
dscrpt.low.rating.df <-
  dscrpt.summary %>%
  filter(listings >= 10, uses >= 10) %>%
  arrange(average_rating) %>%
  slice(1:100)

ggplot(dscrpt.low.rating.df, aes(listings, average_rating)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  #geom_hline(yintercept = mean(dscrpt.summary.filtered$average_rating), color = "red", lty = 2) +
  xlab("# of listings") +
  ylab("Average Rating")

# high rated listing description
dscrpt.high.rating.df <-
  dscrpt.summary %>%
  filter(listings >= 50, uses >= 50) %>%
  arrange(desc(average_rating)) %>%
  slice(1:100)

ggplot(dscrpt.high.rating.df, aes(listings, average_rating)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  #geom_hline(yintercept = mean(dscrpt.summary.filtered$average_rating), color = "red", lty = 2) +
  xlab("# of listings") +
  ylab("Average Rating")

# low review listing description
dscrpt.low.review.df <-
  dscrpt.summary %>%
  filter(listings >= 10, uses >= 10) %>%
  arrange(median_number_reviews) %>%
  slice(1:100)

ggplot(dscrpt.low.review.df, aes(listings, median_number_reviews)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  #geom_hline(yintercept = mean(dscrpt.summary.filtered$median_number_reviews), color = "red", lty = 2) +
  xlab("# of listings") +
  ylab("Median NUmber of Reviews")

# high review listing description
dscrpt.high.review.df <-
  dscrpt.summary %>%
  filter(listings >= 50, uses >= 50) %>%
  arrange(desc(median_number_reviews)) %>%
  slice(1:100)

ggplot(dscrpt.high.review.df, aes(listings, median_number_reviews)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  #geom_hline(yintercept = mean(dscrpt.summary.filtered$median_number_reviews), color = "red", lty = 2) +
  xlab("# of listings") +
  ylab("Median NUmber of Reviews")
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
listing.info.df <- 
  listing.df %>%
  filter(year(host_since) <= 2017) %>%
  select(id, summary, description, space, neighborhood_overview
         , transit, access, interaction, host_about
         , number_of_reviews, review_scores_rating)

# analysis of host_about
host.df <-
  listing.info.df %>%
  unnest_tokens(word, host_about) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

host.summary <- 
  host.df %>%
  filter(!is.na(review_scores_rating)) %>%
  count(id, review_scores_rating, number_of_reviews, word) %>%
  ungroup() %>%
  group_by(word) %>%
  summarise(listings = n()
            , uses = sum(n)
            , average_rating = mean(review_scores_rating)
            , median_number_reviews = median(number_of_reviews)) %>%
  ungroup()

host.summary.filtered <- 
  host.summary %>%
  filter(uses >= 20, listings >= 20)

ggplot(host.summary.filtered, aes(listings, average_rating)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(dscrpt.summary.filtered$average_rating), color = "red", lty = 2) +
  xlab("# of listings") +
  ylab("Average Rating")

ggplot(host.summary.filtered, aes(listings, median_number_reviews)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(dscrpt.summary.filtered$median_number_reviews), color = "red", lty = 2) +
  xlab("# of listings") +
  ylab("Median Number of Reviews")
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# analysis of interaction
interaction.df <-
  listing.info.df %>%
  unnest_tokens(word, interaction) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

interaction.summary <- 
  interaction.df %>%
  filter(!is.na(review_scores_rating)) %>%
  count(id, review_scores_rating, number_of_reviews, word) %>%
  ungroup() %>%
  group_by(word) %>%
  summarise(listings = n()
            , uses = sum(n)
            , average_rating = mean(review_scores_rating)
            , median_number_reviews = median(number_of_reviews)) %>%
  ungroup()

interaction.summary.filtered <- 
  interaction.summary %>%
  filter(uses >= 10, listings >= 10)

ggplot(interaction.summary.filtered, aes(listings, average_rating)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(dscrpt.summary.filtered$average_rating), color = "red", lty = 2) +
  xlab("# of listings") +
  ylab("Average Rating")

ggplot(interaction.summary.filtered, aes(listings, median_number_reviews)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(dscrpt.summary.filtered$median_number_reviews), color = "red", lty = 2) +
  xlab("# of listings") +
  ylab("Median Number of Reviews")


