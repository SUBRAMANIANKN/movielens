# Subbu TEst file

## Data Loading and Preparation

#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

library(caret)
library(tidyverse)
library(lubridate)
library(anytime)

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







#edx <- edx %>% mutate(year_release = stringi::stri_extract(title, regex = "(\\d{4})", comments = TRUE ) %>% as.numeric())
edx2 <- edx
validation2 <- validation

edx <- edx %>% mutate(year_release = as.numeric(str_sub(title,-5,-2)))
edx <- edx %>% mutate(year_eval = year(as_datetime(timestamp)))




validation <- validation %>% mutate(year_release = as.numeric(str_sub(title,-5,-2)))
validation <- validation %>% mutate(year_eval = year(as_datetime(timestamp)))




edx <- edx %>% select(-c(timestamp,title))
validation  <- validation  %>% select(-c(timestamp, title))

mu_edx_rating <- mean(edx$rating)
model_baseline <- RMSE(validation$rating, mu_edx_rating)


rmse_report <- tibble(method = "Base Line average", RMSE = model_baseline)
rmse_report%>%knitr::kable()


movieId_avgs_norm <- edx %>% group_by(movieId) %>% summarize(b_movieId = mean(rating - mu_edx_rating)) 
predicted_movieId_norm <- validation %>% left_join(movieId_avgs_norm, by='movieId') %>% mutate(rating = mu_edx_rating + b_movieId )
model_movieId_rmse <- RMSE(predicted_movieId_norm$rating , validation$rating) 

rmse_report <- rbind(rmse_report, tibble(method = "MOvie effect", RMSE = model_movieId_rmse))
rmse_report%>%knitr::kable()



movieId_userId_avgs_norm <- edx %>% left_join(movieId_avgs_norm, by='movieId') %>% 
                                    group_by(userId) %>% 
                                    summarize(b_movieId_userId = mean(rating - mu_edx_rating - b_movieId)) 

predicted_movieId_userId_norm <- validation %>% left_join(movieId_avgs_norm, by='movieId') %>% 
     left_join(movieId_userId_avgs_norm, by='userId') %>% mutate(rating = mu_edx_rating + b_movieId +  b_movieId_userId  )


model_movieId_userId_rmse <- RMSE(predicted_movieId_userId_norm$rating , validation$rating) 


rmse_report <- rbind(rmse_report, tibble(method = "MOvie & USER effect", RMSE = model_movieId_userId_rmse))
rmse_report%>%knitr::kable()


movieGenres_avgs_norm <- edx %>% group_by(genres) %>% summarize(b_movieGenres = mean(rating - mu_edx_rating)) 
predicted_movieGenres_norm <- validation %>% left_join(movieGenres_avgs_norm, by='genres') %>% mutate(rating = mu_edx_rating + b_movieGenres )
model_movieGenres_rmse <- RMSE(predicted_movieGenres_norm$rating , validation$rating) 


rmse_report <- rbind(rmse_report, tibble(method = "MOvie Genre effect", RMSE = model_movieGenres_rmse))
rmse_report%>%knitr::kable()


movieGenres_userId_avgs_norm <- edx %>% left_join(movieGenres_avgs_norm, by='genres') %>% 
  group_by(userId) %>% 
  summarize(b_movieGenres_userId = mean(rating - mu_edx_rating - b_movieGenres)) 

predicted_movieGenres_userId_norm <- validation %>% left_join(movieGenres_avgs_norm, by='genres') %>% 
  left_join(movieGenres_userId_avgs_norm, by='userId') %>% mutate(rating = mu_edx_rating + b_movieGenres +  b_movieGenres_userId  )


model_movieGenres_userId_rmse <- RMSE(predicted_movieGenres_userId_norm$rating , validation$rating) 



rmse_report <- rbind(rmse_report, tibble(method = "MOvieGenres & User effect", RMSE = model_movieGenres_userId_rmse))
rmse_report%>%knitr::kable()









movieGenres_userId_yearRelease_avgs_norm <- edx %>% left_join(movieGenres_avgs_norm, by='genres') %>% 
  left_join(movieGenres_userId_avgs_norm, by='userId') %>% 
  group_by(year_release) %>% 
  summarize(b_movieGenres_userId_yearRelease = mean(rating - mu_edx_rating - b_movieGenres - b_movieGenres_userId)) 

predicted_movieGenres_userId_yearRelease_norm <- validation %>% 
  left_join(movieGenres_avgs_norm, by='genres') %>% 
  left_join(movieGenres_userId_avgs_norm, by='userId') %>%
  left_join(movieGenres_userId_yearRelease_avgs_norm, by='year_release') %>%
  mutate(rating = mu_edx_rating + b_movieGenres +  b_movieGenres_userId + b_movieGenres_userId_yearRelease  )


model_movieGenres_userId_yearRelease_rmse <- RMSE(predicted_movieGenres_userId_yearRelease_norm$rating , validation$rating) 



rmse_report <- rbind(rmse_report, tibble(method = "MOvieGenres, User & Year Release effect", RMSE = model_movieGenres_userId_yearRelease_rmse))
rmse_report%>%knitr::kable()




movieGenres_userId_yearRelease_movieId_avgs_norm <- edx %>% left_join(movieGenres_avgs_norm, by='genres') %>% 
  left_join(movieGenres_userId_avgs_norm, by='userId') %>% 
  left_join(movieGenres_userId_yearRelease_avgs_norm, by='year_release') %>% 
  group_by(movieId) %>% 
  summarize(b_movieGenres_userId_yearRelease_movieId = mean(rating - mu_edx_rating - b_movieGenres - b_movieGenres_userId - b_movieGenres_userId_yearRelease)) 

predicted_movieGenres_userId_yearRelease_movieId_norm <- validation %>% 
  left_join(movieGenres_avgs_norm, by='genres') %>% 
  left_join(movieGenres_userId_avgs_norm, by='userId') %>%
  left_join(movieGenres_userId_yearRelease_avgs_norm, by='year_release') %>%
  left_join(movieGenres_userId_yearRelease_movieId_avgs_norm, by='movieId') %>%
  mutate(rating = mu_edx_rating + b_movieGenres +  b_movieGenres_userId + b_movieGenres_userId_yearRelease + b_movieGenres_userId_yearRelease_movieId  )


model_movieGenres_userId_yearRelease_movieId_rmse <- RMSE(predicted_movieGenres_userId_yearRelease_movieId_norm$rating , validation$rating) 


rmse_report <- rbind(rmse_report, tibble(method = "MOvieGenres, User , Year Release & Movie effect", RMSE = model_movieGenres_userId_yearRelease_movieId_rmse))
rmse_report%>%knitr::kable()

rmse_report <- tibble(method = "Base Line average", RMSE = model_baseline)
rmse_report%>%knitr::kable()



  ggplot(edx, aes(x=year_release, y=year_eval )) +
  geom_histogram(binwidth = 1, color="black") + geom_point(color='red')
  

facet_grid(year_eval~.)



head(edx)

top5Genres <- edx %>% group_by(genres) %>% summarize(count = n()) %>% arrange(desc(count)) %>% head(5)

# edx %>% filter(year_release < 1500)
# head(edx)
# 
# tmp <- edx %>%
#   filter(year_release > 1930) %>%
#   filter(genres %in% c(top5Genres$genres)) 
#   
# 
# edx %>%
#   na.omit() %>%
#   filter(year_release > 1930) %>%
#   filter(genres %in% c(head(top5Genres$genres,3))) %>%
#   ggplot(aes(x = year_release, y = rating)) +
#  geom_histogram(binwidth = 1, color="black") +
# #   geom_line(aes(color=genres)) +
#   scale_fill_brewer(palette = "Paired") + 
#   facet_grid(genres~.)
# 
# edx %>% group_by(genres) %>% sort(count(genres))


edx %>% summarize(UnqGenre = n_distinct(genres), UnqMid = n_distinct(movieId) , UnqUid = n_distinct(userId), UnqYRls = n_distinct(year_release), UnqYEva = n_distinct(year_eval))


