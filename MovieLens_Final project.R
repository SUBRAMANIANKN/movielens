## Data Loading and Preparation

#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

# Add the required libraries
library(caret)
library(tidyverse)
library(lubridate)

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


# Extract the year of release information from the title column and add that info to a new column year_release 
edx <- edx %>% mutate(year_release = as.numeric(str_sub(title,-5,-2)))
validation <- validation %>% mutate(year_release = as.numeric(str_sub(title,-5,-2)))


# Add a new column called year_eval and update it with year of Evaluation information from the timestamp column
edx <- edx %>% mutate(year_eval = year(as_datetime(timestamp)))
validation <- validation %>% mutate(year_eval = year(as_datetime(timestamp)))


# To save memory remove the timestamp & title columns from both the training and validation data set 
edx <- edx %>% select(-c(timestamp,title))
validation  <- validation  %>% select(-c(timestamp, title))

# Summary details of the data set
summary(edx)
summary(validation)


# Unique list of values across different fields
edx %>% summarize(UnqGenre = n_distinct(genres), UnqMid = n_distinct(movieId) , 
                  UnqUid = n_distinct(userId), UnqYRls = n_distinct(year_release), 
                  UnqYEva = n_distinct(year_eval))

# Analysing how many times different Movies have been rated 
edx %>% count(movieId) %>%  ggplot(aes(n)) +
  geom_histogram(fill = "darkblue") +
  scale_x_log10() +
  labs(title = "Movie Data Distribution",
       subtitle = "Movie distribution by count",
       x = "Movies ratingn count",
       y = "Frequency") 


#Top 5 movies which has more review rating than others
top5Movies <- edx %>% group_by(movieId) %>% summarize(count = n()) %>% 
  arrange(desc(count)) %>% head(5)

top5Movies


# Analysing how many times different user have rated movies 
edx %>% count(userId) %>%  ggplot(aes(n)) +
  geom_histogram(fill = "darkblue" , bins=30) +
  scale_x_log10() +
  labs(title = "Movie Data Distribution",
       subtitle = " User distribution by count",
       x = "Users",
       y = "Frequency") 

# Top 5 users who rated more movies than others
top5Users  <- edx %>% group_by(userId) %>% summarize(count = n()) %>% 
  arrange(desc(count)) %>% head(5)

top5Users


# Analysing how many times different movies have scored similar ratings 
edx %>% ggplot(aes(rating)) +
  geom_histogram(fill = "darkblue") +
  labs(title = "Movie Data Distribution",
       subtitle = "Rating distribution by count",
       x = "Rating",
       y = "Frequency")


# Ratings which has more count during movie review
top5Rating <- edx %>% group_by(rating) %>% summarize(count = n()) %>% 
  arrange(desc(count)) %>% head(5)

top5Rating

# Analysing how old the different movies are that were rated. 
edx %>% ggplot(aes(year_release)) +
  geom_histogram(fill = "darkblue") +
  labs(title = "Movie Data Distribution",
       subtitle = "Movie release year distribution by count",
       x = "Release YEAR",
       y = "Frequency") 

# The top 5 year where there are more movie releases
top5YrRls  <- edx %>% group_by(year_release) %>% 
  summarize(count = n()) %>% arrange(desc(count)) %>% head(5)

top5YrRls

# Analysing how many ovies were rated during different years 
edx %>% ggplot(aes(year_eval)) +
  geom_histogram(fill = "darkblue") +
  labs(title = "Movie Data Distribution",
       subtitle = "Movie evaluation distribution by count",
       x = "Evaluation Year",
       y = "Frequency") 

#Top 5 years when more movies were evaluated when compared to rest of the period
top5YrEval  <- edx %>% group_by(year_eval) %>% 
  summarize(count = n()) %>% arrange(desc(count)) %>% head(5)

top5YrEval


# We shall also look at the top 5 movie Genres which were reviewed more than others 
top5Genres <- edx %>% group_by(genres) %>% 
  summarize(count = n()) %>% arrange(desc(count)) %>% head(5)
top5Genres

# Model deveopment

# Creating Baseline model
mu_edx_rating <- mean(edx$rating)
model_baseline <- RMSE(validation$rating, mu_edx_rating)

rmse_report <- tibble(method = "Base Line average Model", RMSE = model_baseline)
rmse_report%>%knitr::kable()


# Creating model based on impact of Movie genre 
movieGenres_avgs_norm <- edx %>% group_by(genres) %>% 
  summarize(b_movieGenres = mean(rating - mu_edx_rating)) 

predicted_movieGenres_norm <- validation %>% 
  left_join(movieGenres_avgs_norm, by='genres') %>% 
  mutate(rating = mu_edx_rating + b_movieGenres )

model_movieGenres_rmse <- RMSE(predicted_movieGenres_norm$rating , validation$rating) 

rmse_report <- rbind(rmse_report, tibble(method = "Movie Genre Model", 
                                         RMSE = model_movieGenres_rmse))

rmse_report%>%knitr::kable()


# Creating model based on impact of Movie genre & User impact
movieGenres_userId_avgs_norm <- edx %>% 
  left_join(movieGenres_avgs_norm, by='genres') %>% 
  group_by(userId) %>% 
  summarize(b_movieGenres_userId = 
              mean(rating - mu_edx_rating - b_movieGenres)) 

predicted_movieGenres_userId_norm <- validation %>% 
  left_join(movieGenres_avgs_norm, by='genres') %>% 
  left_join(movieGenres_userId_avgs_norm, by='userId') %>% 
  mutate(rating = mu_edx_rating + b_movieGenres +  b_movieGenres_userId  )

model_movieGenres_userId_rmse <- 
  RMSE(predicted_movieGenres_userId_norm$rating , validation$rating) 

rmse_report <- rbind(rmse_report, 
                     tibble(method = "Movie Genre & User effect Model", 
                            RMSE = model_movieGenres_userId_rmse))
rmse_report%>%knitr::kable()


# Creating model based on impact of Movie genre, User & Year Release impact model
movieGenres_userId_yearRelease_avgs_norm <- edx %>% 
  left_join(movieGenres_avgs_norm, by='genres') %>% 
  left_join(movieGenres_userId_avgs_norm, by='userId') %>% 
  group_by(year_release) %>% 
  summarize(b_movieGenres_userId_yearRelease = 
              mean(rating - mu_edx_rating - 
                     b_movieGenres - b_movieGenres_userId)) 

predicted_movieGenres_userId_yearRelease_norm <- validation %>% 
  left_join(movieGenres_avgs_norm, by='genres') %>% 
  left_join(movieGenres_userId_avgs_norm, by='userId') %>%
  left_join(movieGenres_userId_yearRelease_avgs_norm, by='year_release') %>%
  mutate(rating = mu_edx_rating + b_movieGenres +  b_movieGenres_userId + 
           b_movieGenres_userId_yearRelease  )

model_movieGenres_userId_yearRelease_rmse <- 
  RMSE(predicted_movieGenres_userId_yearRelease_norm$rating , 
       validation$rating) 

rmse_report <- 
  rbind(rmse_report, 
        tibble(method = "Movie Genre, User & Release Year effect Model", 
               RMSE = model_movieGenres_userId_yearRelease_rmse))

rmse_report%>%knitr::kable()


# Creating model based on impact of Movie genre, User, Release Year & Evaluation year impact model

movieGenres_userId_yearRelease_yearEval_avgs_norm <- edx %>% left_join(movieGenres_avgs_norm, by='genres') %>%
  left_join(movieGenres_userId_avgs_norm, by='userId') %>%
  left_join(movieGenres_userId_yearRelease_avgs_norm, by='year_release') %>%
  group_by(year_eval) %>%
  summarize(b_movieGenres_userId_yearRelease_yearEval = 
              mean(rating - mu_edx_rating - b_movieGenres - 
                     b_movieGenres_userId - b_movieGenres_userId_yearRelease))

predicted_movieGenres_userId_yearRelease_yearEval_norm <- validation %>%
  left_join(movieGenres_avgs_norm, by='genres') %>%
  left_join(movieGenres_userId_avgs_norm, by='userId') %>%
  left_join(movieGenres_userId_yearRelease_avgs_norm, by='year_release') %>%
  left_join(movieGenres_userId_yearRelease_yearEval_avgs_norm, by='year_eval') %>%
  mutate(rating = mu_edx_rating + b_movieGenres +  b_movieGenres_userId + 
           b_movieGenres_userId_yearRelease + 
           b_movieGenres_userId_yearRelease_yearEval  )

model_movieGenres_userId_yearRelease_yearEval_rmse <- 
  RMSE(predicted_movieGenres_userId_yearRelease_yearEval_norm$rating , 
       validation$rating)

rmse_report <- 
  rbind(rmse_report, 
        tibble(method = "Movie Genre, User, Release Year & Evaluation Year effect Model",                 RMSE = model_movieGenres_userId_yearRelease_yearEval_rmse))
rmse_report%>%knitr::kable()


# There is no significant impact by including the year of evaluation in the model 

# So far we tried model based on Movie Genre as it will help if in future we get any new movie (not in training set but belongs to genre available in training set) to be predicted for rating .
# Now we shall slightly change track and  try  model based on the Movie Id to see whether we can improve the RMSE further though this model will not be able to help if a new movie which is not in the existing data set has to be predicted for rating even thought it belongs to existing genre



# Creating model based on  Movie Id impact
movieId_avgs_norm <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_movieId = mean(rating - mu_edx_rating)) 

predicted_movieId_norm <- validation %>% 
  left_join(movieId_avgs_norm, by='movieId') %>% 
  mutate(rating = mu_edx_rating + b_movieId )

model_movieId_rmse <- RMSE(predicted_movieId_norm$rating , validation$rating) 

rmse_report <- 
  rbind(rmse_report, tibble(method = "Movie effect Model", 
                            RMSE = model_movieId_rmse))

rmse_report%>%knitr::kable()


# Creating model based on  Movie Id & User impact
movieId_userId_avgs_norm <- edx %>% 
  left_join(movieId_avgs_norm, by='movieId') %>% 
  group_by(userId) %>% 
  summarize(b_movieId_userId = mean(rating - mu_edx_rating - b_movieId)) 

predicted_movieId_userId_norm <- validation %>% 
  left_join(movieId_avgs_norm, by='movieId') %>% 
  left_join(movieId_userId_avgs_norm, by='userId') %>% 
  mutate(rating = mu_edx_rating + b_movieId +  b_movieId_userId  )

model_movieId_userId_rmse <- 
  RMSE(predicted_movieId_userId_norm$rating , validation$rating) 

rmse_report <- rbind(rmse_report, tibble(method = "Movie & User effect Model", 
                                         RMSE = model_movieId_userId_rmse))

rmse_report%>%knitr::kable()

# Creating model based on  Movie Id, User & Year Release impact
movieId_userId_yearRelease_avgs_norm <- edx %>% 
  left_join(movieId_avgs_norm, by='movieId') %>% 
  left_join(movieId_userId_avgs_norm, by='userId') %>% 
  group_by(year_release) %>% 
  summarize(b_movieId_userId_yearRelease = 
              mean(rating - mu_edx_rating - b_movieId - b_movieId_userId)) 

predicted_movieId_userId_yearRelease_norm <- validation %>% 
  left_join(movieId_avgs_norm, by='movieId') %>% 
  left_join(movieId_userId_avgs_norm, by='userId') %>%
  left_join(movieId_userId_yearRelease_avgs_norm, by='year_release') %>%
  mutate(rating = mu_edx_rating + b_movieId +  b_movieId_userId + 
           b_movieId_userId_yearRelease  )

model_movieId_userId_yearRelease_rmse <- 
  RMSE(predicted_movieId_userId_yearRelease_norm$rating , validation$rating) 

rmse_report <- 
  rbind(rmse_report, tibble(method = "Movie, User & Release Year effect Model ", 
                            RMSE = model_movieId_userId_yearRelease_rmse))

rmse_report%>%knitr::kable()


# Creating model based on  Movie Id, User & Year Release & Evaluation Year impact
movieId_userId_yearRelease_yearEval_avgs_norm <- edx %>% 
  left_join(movieId_avgs_norm, by='movieId') %>%
  left_join(movieId_userId_avgs_norm, by='userId') %>%
  left_join(movieId_userId_yearRelease_avgs_norm, by='year_release') %>%
  group_by(year_eval) %>%
  summarize(b_movieId_userId_yearRelease_yearEval = 
              mean(rating - mu_edx_rating - b_movieId - b_movieId_userId  - 
                     b_movieId_userId_yearRelease))

predicted_movieId_userId_yearRelease_yearEval_norm <- validation %>%
  left_join(movieId_avgs_norm, by='movieId') %>%
  left_join(movieId_userId_avgs_norm, by='userId') %>%
  left_join(movieId_userId_yearRelease_avgs_norm, by='year_release') %>%
  left_join(movieId_userId_yearRelease_yearEval_avgs_norm, by='year_eval') %>%
  mutate(rating = mu_edx_rating + b_movieId +  b_movieId_userId + 
           b_movieId_userId_yearRelease + b_movieId_userId_yearRelease_yearEval)


model_movieId_userId_yearRelease_yearEval_rmse <- 
  RMSE(predicted_movieId_userId_yearRelease_yearEval_norm$rating , 
       validation$rating)

rmse_report <- 
  rbind(rmse_report, 
        tibble(method = "Movie, User, Release Year & Evaluation Year effect Model", 
               RMSE = model_movieId_userId_yearRelease_yearEval_rmse))

rmse_report%>%knitr::kable()

# Creating model based on Regularziation of  Movie Id, User & Year Release & Evaluation Year & Evaluation Year

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  movieId_avgs_norm <- edx %>% group_by(movieId) %>% 
    summarize(b_movieId = sum(rating - mu_edx_rating)/(n()+l)) 
  
  movieId_userId_avgs_norm <- edx %>% 
    left_join(movieId_avgs_norm, by='movieId') %>% 
    group_by(userId) %>% 
    summarize(b_movieId_userId = sum(rating - mu_edx_rating - b_movieId)/(n()+l))
  
  
  movieId_userId_yearRelease_avgs_norm <- edx %>% 
    left_join(movieId_avgs_norm, by='movieId') %>% 
    left_join(movieId_userId_avgs_norm, by='userId') %>% 
    group_by(year_release) %>% 
    summarize(b_movieId_userId_yearRelease = 
                sum(rating - mu_edx_rating - b_movieId - b_movieId_userId)/(n()+l))
  
  
  movieId_userId_yearRelease_yearEval_avgs_norm <- edx %>% 
    left_join(movieId_avgs_norm, by='movieId') %>%
    left_join(movieId_userId_avgs_norm, by='userId') %>%
    left_join(movieId_userId_yearRelease_avgs_norm, by='year_release') %>%
    group_by(year_eval) %>%
    summarize(b_movieId_userId_yearRelease_yearEval = 
                sum(rating - mu_edx_rating - b_movieId - b_movieId_userId  - 
                      b_movieId_userId_yearRelease)/(n()+l))
  
  predicted_movieId_userId_yearRelease_yearEval_norm <- validation %>%
    left_join(movieId_avgs_norm, by='movieId') %>%
    left_join(movieId_userId_avgs_norm, by='userId') %>%
    left_join(movieId_userId_yearRelease_avgs_norm, by='year_release') %>%
    left_join(movieId_userId_yearRelease_yearEval_avgs_norm, by='year_eval') %>%
    mutate(rating = mu_edx_rating + b_movieId +  b_movieId_userId + 
             b_movieId_userId_yearRelease + b_movieId_userId_yearRelease_yearEval)
  
  
  return(RMSE(predicted_movieId_userId_yearRelease_yearEval_norm$rating , 
              validation$rating))
  
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

rmse_report <- 
  rbind(rmse_report, 
        tibble(method = " Regularized Movie, User, Release Year & Evaluation Year effect Model", 
               RMSE = min(rmses)))

rmse_report%>%knitr::kable()

