# Loading the sufficient dataset and removing the NA values

library(MASS)
library(ISLR)
require(ISLR)
library(class)
require(class)
require(boot)
require(tree)
movies = read.csv("./movies.csv", na.strings = "?")
movies = na.omit(movies)
attach(movies)
totalrows = dim(movies)[1]
imdb_rating = rep("bad", totalrows)

# Classifying the predictor based on imdb_score: 
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
test = (percent70data+1):totalrows
moviesTrain = moviesdataset[train, ]
moviesTest = moviesdataset[test, ]
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
par(mfrow=c(2,2))
dim(moviesdataset)
glm.fit1 = lm(imdb_score~num_critic_for_reviews + actor_1_facebook_likes + actor_2_facebook_likes + director_facebook_likes + gross + num_voted_users + facenumber_in_poster + num_user_for_reviews + country + movie_facebook_likes, data=moviesTrain)
summary(glm.fit1)
plot(glm.fit1)
glm.fit2 = lm(imdb_score~num_critic_for_reviews + director_facebook_likes + gross + num_voted_users + facenumber_in_poster + num_user_for_reviews + country + movie_facebook_likes, data=moviesTrain)
summary(glm.fit2)
plot(glm.fit2)
glm.fit3 = lm(imdb_score~num_critic_for_reviews + director_facebook_likes + gross + num_voted_users + facenumber_in_poster + num_user_for_reviews + movie_facebook_likes, data=moviesTrain)
summary(glm.fit3)
plot(glm.fit3)
glm.fit4 = lm(imdb_score~num_critic_for_reviews + director_facebook_likes + gross + num_voted_users + facenumber_in_poster + num_user_for_reviews, data=moviesTrain)
summary(glm.fit4)
plot(glm.fit4)
glm.fit5 = lm(imdb_score~num_critic_for_reviews + movie_facebook_likes*director_facebook_likes + gross + num_voted_users + facenumber_in_poster + num_user_for_reviews, data=moviesTrain)
summary(glm.fit5)
plot(glm.fit5)

# Having cross validation on the above predicted Linear Regression models having high R square

glm.fit3CV = glm(imdb_score~num_critic_for_reviews + director_facebook_likes + gross + num_voted_users + facenumber_in_poster + num_user_for_reviews + movie_facebook_likes, data=moviesTrain)
glm.fit4CV = glm(imdb_score~num_critic_for_reviews + director_facebook_likes + gross + num_voted_users + facenumber_in_poster + num_user_for_reviews, data=moviesTrain)
glm.fit5CV = glm(imdb_score~num_critic_for_reviews + movie_facebook_likes*director_facebook_likes + gross + num_voted_users + facenumber_in_poster + num_user_for_reviews, data=moviesTrain)
cv.glm(moviesTrain, glm.fit3CV)$delta[1]
cv.glm(moviesTrain, glm.fit4CV)$delta[1]
cv.glm(moviesTrain, glm.fit5CV)$delta[1]
cv.glm(moviesTrain, glm.fit3CV, K=5)$delta[1]
cv.glm(moviesTrain, glm.fit4CV, K=5)$delta[1]
cv.glm(moviesTrain, glm.fit5CV, K=5)$delta[1]
cv.glm(moviesTrain, glm.fit3CV, K=10)$delta[1]
cv.glm(moviesTrain, glm.fit4CV, K=10)$delta[1]
cv.glm(moviesTrain, glm.fit5CV, K=10)$delta[1]

# Running the best fitted models (Multiple Linear Regression statistical learning method) on test data set; We found glm.fit4 and glm.fit5 models are better ones which can be predicted on Test Data set

glm.predict3conf = confint(glm.fit3)
glm.predict3conf
glm.predict4conf = confint(glm.fit4)
glm.predict4conf
glm.predict5conf = confint(glm.fit5)
glm.predict5conf
glm.predict3 = predict(glm.fit3, moviesTest)
summary(glm.predict3)
glm.predict4 = predict(glm.fit4, moviesTest)
summary(glm.predict4)
glm.predict5 = predict(glm.fit5, moviesTest)
summary(glm.predict5)
glm.prob3 = rep("bad", nrow(moviesTest))
glm.prob3[(predict(glm.fit3) < 6)] = "bad"
glm.prob3[(predict(glm.fit3) >= 6) & (predict(glm.fit3) < 8)] = "average"
glm.prob3[(predict(glm.fit3) >= 8) & (predict(glm.fit3) < 9)] = "good"
glm.prob3[(predict(glm.fit3) >= 9)] = "best"
mean(glm.prob3 != moviesTest$imdb_rating)
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
lda.fit3 = lda(imdb_rating~num_critic_for_reviews + director_facebook_likes + gross + num_voted_users + facenumber_in_poster + num_user_for_reviews + movie_facebook_likes, data=moviesTrain)
lda.fit3
plot(lda.fit3)
lda.fit4 = lda(imdb_rating~num_critic_for_reviews + director_facebook_likes + gross + num_voted_users + facenumber_in_poster + num_user_for_reviews, data=moviesTrain)
lda.fit4
plot(lda.fit4)
lda.fit5 = lda(imdb_rating~num_critic_for_reviews + movie_facebook_likes*director_facebook_likes + gross + num_voted_users + facenumber_in_poster + num_user_for_reviews, data=moviesTrain)
lda.fit5
plot(lda.fit5)
# Use the above fitted LDA model on test data set

lda.predict3 = predict(lda.fit3, moviesTest)
table(lda.predict3$class, moviesTest$imdb_rating)
mean(lda.predict3$class != moviesTest$imdb_rating)
lda.predict4 = predict(lda.fit4, moviesTest)
table(lda.predict4$class, moviesTest$imdb_rating)
mean(lda.predict4$class != moviesTest$imdb_rating)
lda.predict5 = predict(lda.fit5, moviesTest)
table(lda.predict5$class, moviesTest$imdb_rating)
mean(lda.predict5$class != moviesTest$imdb_rating)

# We can clearly see from LDA models that model 4 which we have used in Multiple Linear Regression model performed well on testing data set as it provided 27% test error rate while model 5 provided 28% test error rate

# Using the KNN model now with only those predictors which are highly associated with the target variable; We are using the predictors of glm.fit4 and glm.fit5 models from the very earlier stage

set.seed(1)
attach(moviesdataset)
knntrain = as.matrix(moviesdataset[train, ])
knntest = as.matrix(moviesdataset[test, ])
predictors = cbind(num_critic_for_reviews,director_facebook_likes,gross,num_voted_users,facenumber_in_poster,num_user_for_reviews)
p = imdb_rating[train]
knn.pred1 = knn(predictors[train,],predictors[test,],p,k=5)
table(knn.pred1,imdb_rating[test])
mean(knn.pred1!=imdb_rating[test])
knn.pred1 = knn(predictors[train,],predictors[test,],p,k=10)
table(knn.pred1,imdb_rating[test])
mean(knn.pred1!=imdb_rating[test])
knn.pred1 = knn(predictors[train,],predictors[test,],p,k=15)
table(knn.pred1,imdb_rating[test])
mean(knn.pred1!=imdb_rating[test])
knn.pred1 = knn(predictors[train,],predictors[test,],p,k=20)
table(knn.pred1,imdb_rating[test])
mean(knn.pred1!=imdb_rating[test])
predictors = cbind(num_critic_for_reviews,movie_facebook_likes,director_facebook_likes,gross,num_voted_users,facenumber_in_poster,num_user_for_reviews)
p = imdb_rating[train]
knn.pred1 = knn(predictors[train,],predictors[test,],p,k=5)
table(knn.pred1,imdb_rating[test])
mean(knn.pred1!=imdb_rating[test])
knn.pred1 = knn(predictors[train,],predictors[test,],p,k=10)
table(knn.pred1,imdb_rating[test])
mean(knn.pred1!=imdb_rating[test])
knn.pred1 = knn(predictors[train,],predictors[test,],p,k=15)
table(knn.pred1,imdb_rating[test])
mean(knn.pred1!=imdb_rating[test])
knn.pred1 = knn(predictors[train,],predictors[test,],p,k=20)
table(knn.pred1,imdb_rating[test])
mean(knn.pred1!=imdb_rating[test])

# K=20 performs better result on both the models which provides lower test error rate

# Using Tree model now on the above data set

set.seed(1)
tree.movies=tree(imdb_rating~num_critic_for_reviews + director_facebook_likes + gross + num_voted_users + facenumber_in_poster + num_user_for_reviews, moviesdataset[train,])
plot(tree.movies);text(tree.movies,pretty=0)
tree.pred=predict(tree.movies,moviesdataset[-train,],type="class")
with(moviesdataset[-train,],table(tree.pred,imdb_rating))
(501+3+35+1+42+10)/1140
# Very high error rate from the tree model
# Trying to use cross validation to get the best sequence of subtrees and then pruning the tree to fit on the best subtree 
cv.movies=cv.tree(tree.movies,FUN=prune.misclass)
cv.movies
plot(cv.movies)
prune.movies=prune.misclass(tree.movies,best=4) 
#Even tried with 5 and 6 as suggested by cv.movies but results are same
plot(prune.movies);text(prune.movies,pretty=0)
summary(prune.movies)
tree.pred=predict(prune.movies,moviesdataset[-train,],type="class")
with(moviesdataset[-train,],table(tree.pred,imdb_rating))
