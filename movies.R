# Loading the sufficient dataset and removing the NA values

library(MASS)
rm(moviesdataset)
rm(imdb_rating)
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
moviesTrain = movies[train, ]
moviesTest = movies[-train, ]
dim(moviesTrain)
dim(moviesTest)

# As our target has been classified into more than 2 levels so we are using a stabilized statistical learning method, LDA

