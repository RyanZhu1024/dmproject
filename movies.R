# Loading the sufficient dataset and removing the NA values

library(MASS)
library(ISLR)
require(ISLR)
library(class)
require(class)
require(boot)
movies = read.csv("./movies.csv", na.strings = "?")
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
plot(imdb_score, budget)                    # Impacting not at all
plot(imdb_score, cast_total_facebook_likes) # Impacting very few
plot(imdb_score, duration)                  # Impacting not at all
plot(imdb_score, actor_2_facebook_likes)    # Impacting very few
plot(imdb_score, actor_1_facebook_likes)    # Impacting not at all
plot(imdb_score, actor_3_facebook_likes)    # Impacting not at all
plot(color, imdb_score)                     # Impacting not at all
plot(director_name, imdb_score)             # Impacting not at all

# We are trying with Multiple Linear Regression Model based on the above plots for determination of predictors to come up with the predicted imdb score

dim(moviesdataset)
glm.fit1 = lm(imdb_score~num_critic_for_reviews + actor_1_facebook_likes + actor_2_facebook_likes + director_facebook_likes + gross + num_voted_users + facenumber_in_poster + num_user_for_reviews + country + movie_facebook_likes, data=moviesTrain)
summary(glm.fit1)
glm.fit2 = lm(imdb_score~num_critic_for_reviews + director_facebook_likes + gross + num_voted_users + facenumber_in_poster + num_user_for_reviews + country + movie_facebook_likes, data=moviesTrain)
summary(glm.fit2)
glm.fit3 = lm(imdb_score~num_critic_for_reviews + director_facebook_likes + gross + num_voted_users + facenumber_in_poster + num_user_for_reviews + movie_facebook_likes, data=moviesTrain)
summary(glm.fit3)
glm.fit4 = lm(imdb_score~num_critic_for_reviews + director_facebook_likes + gross + num_voted_users + facenumber_in_poster + num_user_for_reviews, data=moviesTrain)
summary(glm.fit4)
glm.fit5 = lm(imdb_score~num_critic_for_reviews + movie_facebook_likes*director_facebook_likes + gross + num_voted_users + facenumber_in_poster + num_user_for_reviews, data=moviesTrain)
summary(glm.fit5)

# Running the best fitted models (Multiple Linear Regression statistical learning method) on test data set; We found glm.fit4 and glm.fit5 models are better ones which can be predicted on Test Data set

glm.predict4conf = confint(glm.fit4)
glm.predict4conf
glm.predict5conf = confint(glm.fit5)
glm.predict5conf
glm.predict4 = predict(glm.fit4, moviesTest)
summary(glm.predict4)
glm.predict5 = predict(glm.fit5, moviesTest)
summary(glm.predict5)
glm.prob4 = rep("bad", nrow(moviesTest))
glm.prob4[(predict(glm.fit4) < 6)] = "bad"
glm.prob4[(predict(glm.fit4) >= 6) & (predict(glm.fit4) < 8)] = "average"
glm.prob4[(predict(glm.fit4) >= 8) & (predict(glm.fit4) < 9)] = "good"
glm.prob4[(predict(glm.fit4) >= 9)] = "best"
mean(glm.prob4 != moviesTest$imdb_rating)
glm.prob5 = rep("bad", nrow(moviesTest))
glm.prob5[(predict(glm.fit5) < 6)] = "bad"
glm.prob5[(predict(glm.fit5) >= 6) & (predict(glm.fit5) < 8)] = "average"
glm.prob5[(predict(glm.fit5) >= 8) & (predict(glm.fit5) < 9)] = "good"
glm.prob5[(predict(glm.fit5) >= 9)] = "best"
mean(glm.prob5 != moviesTest$imdb_rating)

# As of now, model 4 (glm.fit4) from Multiple Linear Regression makes more fit on testing data with 38.78% test error rate while model 5 (glm.fit5) makes more than 40% test error rate
# Adjusted R square value for model 4 was little low than model 5; RSS was high for model 4 compared to model 5

# We are not using the Logistic Regression models as we are classifying our target based on 4 levels [bad, average, good and best]
# Using the LDA model now with only those predictors which are highly associated with the target variable; We are using the predictors of glm.fit4 and glm.fit5 models from the earlier stage

attach(moviesTrain)
lda.fit4 = lda(imdb_rating~num_critic_for_reviews + director_facebook_likes + gross + num_voted_users + facenumber_in_poster + num_user_for_reviews, data=moviesTrain)
lda.fit4
lda.fit5 = lda(imdb_rating~num_critic_for_reviews + movie_facebook_likes*director_facebook_likes + gross + num_voted_users + facenumber_in_poster + num_user_for_reviews, data=moviesTrain)
lda.fit5

# Use the above fitted LDA model on test data set

lda.predict4 = predict(lda.fit4, moviesTest)
table(lda.predict4$class, moviesTest$imdb_rating)
mean(lda.predict4$class != moviesTest$imdb_rating)
lda.predict5 = predict(lda.fit5, moviesTest)
table(lda.predict5$class, moviesTest$imdb_rating)
mean(lda.predict5$class != moviesTest$imdb_rating)

# We can clearly see from LDA models that model 4 which we have used in Multiple Linear Regression model performed well on testing data set as it provided 27% test error rate while model 5 provided 28% test error rate

# Using the KNN model now with only those predictors which are highly associated with the target variable; We are using the predictors of glm.fit4 and glm.fit5 models from the very earlier stage
predictors=cbind(num_critic_for_reviews,director_facebook_likes,gross,num_voted_users,facenumber_in_poster,num_user_for_reviews)
knn.pred1 = knn(predictors[train,],predictors[!train,],imdb_rating[train],k=10)
table(knn.pred1,imdb_rating[!train])
mean(knn.pred1==imdb_rating[!train])
