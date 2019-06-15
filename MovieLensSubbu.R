# Subbu TEst file

## Data Loading and Preparation

#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

edx <- edx %>% mutate(year_release = stringi::stri_extract(title, regex = "(\\d{4})", comments = TRUE ) %>% as.numeric())

edx <- edx %>% mutate(year_eval = year(as_datetime(timestamp)))


validation <- validation %>% 
  mutate(year_release = stringi::stri_extract(title, regex = "(\\d{4})", comments = TRUE ) %>% 
           as.numeric())

validation <- validation %>% mutate(year_eval = year(as_datetime(timestamp)))

edx <- edx %>% select(-C(timestamp,title))
 
validation  <- validation  %>% select(-c(timestamp, title))

mu_edx_rating <- mean(edx$rating)
model_baseline <- RMSE(validation$rating, mu_edx_rating)

movieId_avgs_norm <- edx %>% group_by(movieId) %>% summarize(b_m = mean(rating - mu_edx_rating)) 

predicted_movieId_norm <- validation %>% left_join(movieId_avgs_norm, by='movieId') %>% mutate(rating = mu_edx_rating + b_m)

model_movieId_rmse <- RMSE(predicted_movieId_norm$rating , validation$rating) 


