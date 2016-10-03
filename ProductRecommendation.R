#CLEAR WORKSPACE
rm(list = ls(all = TRUE))

# Get the working dir and set if it is not the one
getwd()
if(getwd()!="D:\\DataScience_Workshop")
    setwd("D:\\DataScience_Workshop")
getwd() 


# Load Library
library(recommenderlab)
library(ggplot2)

#Loading the data
data(MovieLense)
summary(getRatings(MovieLense))

#normalized ratings
qplot(getRatings(normalize(MovieLense, method="Z-score")), main="Histogram of normalized ratings", xlab="Rating")
summary(getRatings(normalize(MovieLense, method="Z-score")))

#no. of movies rated by an average user
qplot(rowCounts(MovieLense), binwidth=10, main="Movies rated on Average", xlab="# of users", ylab="# of movies rated")

#Mean rating of each movie
qplot(colMeans(MovieLense), binwidth=0.1, main="Mean ratings of Movies", xlab="Rating", ylab="# of movies")

recommenderRegistry$get_entries(dataType="realRatingMatrix")

# Scheme
scheme <- evaluationScheme(MovieLense, method="split", train=0.9, k=1, goodRating=4,given=10)

# Algorithms
algorithms <- list(
  "random items" = list(name="RANDOM", param=list(normalize = "Z-score")),
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score", method="Cosine", nn=50, minRating=3))
)
# run algorithms, predict next n movies
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15))

# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")

