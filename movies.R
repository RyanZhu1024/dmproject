# Loading the sufficient dataset and removing the NA values

library(MASS)
movies = read.csv("movies.csv", na.strings = "?")
movies = na.omit(movies)
attach(movies)
totalrows = dim(movies)[1]
imdb_rating = rep("bad", totalrows)

# Classificying the predictor based on imdb_score: 
# If less than 6 then classifying it as bad movie
# If between 6 and 8 then classifying it as average movie
# If between 8 and 9 then classifying it as good movie
# If greater than 9 then classifyng it as best movie

imdb_rating[imdb_score<6] = "bad"
imdb_rating[(imdb_score>=6) & (imdb_score<8)] = "average"
imdb_rating[(imdb_score>=8) & (imdb_score<9)] = "good"
imdb_rating[imdb_score>=9] = "best"

# Making a new data set for movie

moviesdataset = data.frame(movies, imdb_rating)
fix(moviesdataset)
summary(moviesdataset)

# Sampling the data set into 70:30 ratio for Training and Testing purpose

percent70data = round(0.70*totalrows)
percent30data = totalrows - percent70data
train = 1:percent70data
test = -train
moviesTrain = moviesdataset[train, ]
moviesTest = moviesdataset[-train, ]
dim(moviesTrain)
dim(moviesTest)

# Plots with all predictors

attach(moviesdataset)
plot(imdb_score, num_critic_for_reviews)
plot(imdb_score, director_facebook_likes)
plot(imdb_score, gross)
plot(imdb_score, num_voted_users)
plot(imdb_score, facenumber_in_poster)
plot(imdb_score, num_user_for_reviews)
plot(country, imdb_score)
plot(imdb_score, movie_facebook_likes)
# plot(imdb_score, budget); Impacting not at all
# plot(imdb_score, cast_total_facebook_likes); Impacting very few
# plot(imdb_score, duration); Impacting not at all
# plot(imdb_score, actor_2_facebook_likes); Impacting very few
# plot(imdb_score, actor_1_facebook_likes); Impacting not at all
# plot(imdb_score, actor_3_facebook_likes); Impacting not at all
# plot(color, imdb_score); Impacting not at all
# plot(director_name, imdb_score); Impacting not at all

# As our target has been classified into more than 2 levels so we are using a stabilized statistical learning method, LDA

require(ISLR)
require(class)
require(boot)
dim(moviesdataset)
lda.fit = lda(imdb_rating~num_critic_for_reviews + director_facebook_likes + gross + num_voted_users + facenumber_in_poster + num_user_for_reviews + country + movie_facebook_likes, data=moviesTrain)
