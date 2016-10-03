#CLEAR WORKSPACE
rm(list = ls(all = TRUE))

#Data Read
data = read.csv("dataset.csv", stringsAsFactors=FALSE,quote ="")

#Data Preprocessing
library(tm)
library(SnowballC)

#Building the Corpus
corpus = Corpus(VectorSource(data$Name))

#Convert to Lower Case
corpus = tm_map(corpus, tolower)

#Remove Punctuation
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))

#Stemming the Document
corpus = tm_map(corpus, stemDocument)

corpus[[1]]$content

#Sparse Matrix
dtm = DocumentTermMatrix(corpus)
dtm
?findFreqTerms
findFreqTerms(dtm, lowfreq=20)
?removeSparseTerms
sparse = removeSparseTerms(dtm, 0.995)
dataSparse = as.data.frame(as.matrix(sparse))
colnames(dataSparse) = make.names(colnames(dataSparse))
dataSparse$Label = as.factor(data$Label)

#Data Split
library(caTools)
set.seed(123)
split = sample.split(dataSparse$Label, SplitRatio = 0.8)
trainData = subset(dataSparse, split==TRUE)
testData = subset(dataSparse, split==FALSE)

#------------------------------DECISION TREE-----------------------------------
#Decision Tree Model
library(rpart)
library(rpart.plot)

?rpart
#Train the Model
dataCART = rpart(Label ~ ., data=trainData, method="class")
prp(dataCART)

#Prediction on Test Data
predictCART = predict(dataCART, newdata=testData, type="class")

#Evaluation - Decision Tree
install.packages("caret")
library(caret)
install.packages("pbkrtest")
library(pbkrtest)
confusionMatrix(predictCART,testData$Label)

#------------------------------RANDOM FOREST-----------------------------------

#Random Forest Model
library(randomForest)

#Train the Model
dataRF <- randomForest(Label ~ .,data=trainData, ntree=30)

#Prediction on Test Data
predictRF = predict(dataRF, newdata=testData, type="class")

#Evaluation - Random Forest
confusionMatrix(predictRF,testData$Label)

#------------------------------SVM-----------------------------------

#Support Vector Machine Model
library(e1071)

#Train the Model
dataSVM <- svm(Label ~ .,data=trainData)

#Prediction on Test Data
predictSVM = predict(dataSVM, newdata=testData, type="class")

#Evaluation - Support Vector Machine
confusionMatrix(predictSVM,testData$Label)


