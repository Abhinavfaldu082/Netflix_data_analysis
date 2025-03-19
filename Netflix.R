# Load required libraries
install.packages(c("tidyverse", "data.table", "ggplot2", "lubridate", "wordcloud2"))
library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(wordcloud2)

# Load dataset
df <- read.csv("data.csv")

# 1. Handle Missing Values & Duplicates
# Remove duplicate rows
df <- df[!duplicated(df), ]

# Count missing values
colSums(is.na(df))

# Drop 'availableCountries' if mostly missing
if ("availableCountries" %in% colnames(df)) {
  df <- df %>% select(-availableCountries)
}

# Impute missing numeric values with median
df$imdbAverageRating[is.na(df$imdbAverageRating)] <- median(df$imdbAverageRating, na.rm = TRUE)
df$imdbNumVotes[is.na(df$imdbNumVotes)] <- median(df$imdbNumVotes, na.rm = TRUE)

# 2. Analyze Distribution of Ratings & Genres
# Histogram of IMDb ratings
ggplot(df, aes(x=imdbAverageRating)) +
  geom_histogram(bins=30, fill="blue", alpha=0.7) +
  theme_minimal() +
  ggtitle("Distribution of IMDb Ratings")

# Count of movies/series per genre
genre_counts <- df %>% separate_rows(genres, sep=',') %>% count(genres, sort=TRUE)
ggplot(genre_counts, aes(x=reorder(genres, n), y=n)) +
  geom_bar(stat="identity", fill="steelblue") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Number of Movies/Series per Genre")

# Word cloud for genres
wordcloud2(data = genre_counts)

# 3. Explore Trends Over Time (Movies vs. Series)
ggplot(df, aes(x=releaseYear, fill=type)) +
  geom_histogram(binwidth=1, alpha=0.7, position="dodge") +
  theme_minimal() +
  ggtitle("Movies vs. Series Over Time")

# 4. Detect Popular & Highly Rated Titles
# Top-rated titles
top_rated <- df %>% filter(imdbAverageRating > 8.5 & imdbNumVotes > 5000)
print(top_rated[, c("title", "imdbAverageRating", "imdbNumVotes")])

# Most popular movies/series by vote count
most_popular <- df %>% arrange(desc(imdbNumVotes)) %>% head(10)
print(most_popular[, c("title", "imdbAverageRating", "imdbNumVotes")])

# 5. Investigate availableCountries Column
# If column was not dropped due to missing values:
if("availableCountries" %in% colnames(df)) {
  country_counts <- df %>% separate_rows(availableCountries, sep=',') %>% count(availableCountries, sort=TRUE)
  ggplot(country_counts, aes(x=reorder(availableCountries, n), y=n)) +
    geom_bar(stat="identity", fill="darkred") +
    coord_flip() +
    theme_minimal() +
    ggtitle("Content Availability by Country")
}

# 6. Temporal Analysis
# Monthly and yearly release trend
df$releaseYear <- as.integer(df$releaseYear)
df <- df %>% filter(releaseYear > 1900 & releaseYear <= year(Sys.Date()))
ggplot(df, aes(x=releaseYear)) +
  geom_density(fill="green", alpha=0.5) +
  theme_minimal() +
  ggtitle("Density Plot of Release Years")

# 7. Relationship Between Rating & Votes
ggplot(df, aes(x=imdbNumVotes, y=imdbAverageRating)) +
  geom_point(alpha=0.5, color="purple") +
  scale_x_log10() +
  theme_minimal() +
  ggtitle("Relationship Between IMDb Votes & Ratings")

# 8. Top Production Years for Movies & Series
top_years <- df %>% count(releaseYear, sort=TRUE) %>% head(10)
ggplot(top_years, aes(x=reorder(releaseYear, n), y=n)) +
  geom_bar(stat="identity", fill="darkorange") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Top 10 Production Years")

# 9. IMDb Rating Trends Over Time
rating_trend <- df %>% group_by(releaseYear) %>% summarize(avg_rating = mean(imdbAverageRating, na.rm = TRUE))
ggplot(rating_trend, aes(x=releaseYear, y=avg_rating)) +
  geom_line(color="blue", size=1) +
  theme_minimal() +
  ggtitle("IMDb Rating Trends Over Time")

summary(df)
write.csv(df, "cleaned_data.csv", row.names = FALSE)
