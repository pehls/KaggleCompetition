library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(tm)
library(SnowballC)
library(neuralnet)
setwd("../input") # For Kaggle kernel

train <- read.csv("train.csv")
test <- read.csv("test.csv")

train$comment_text<-as.character(train$comment_text)
train$comment_text <- sapply(train$comment_text, function(x) iconv(enc2utf8(x), sub = "byte"))
corpus <- VCorpus(VectorSource(train$comment_text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c(stopwords("english")))
corpus <- tm_map(corpus, stemDocument)
frequencies = DocumentTermMatrix(corpus)
sparse = removeSparseTerms(frequencies, 0.9)
trainSparse <- as.data.frame(as.matrix(sparse))
colnames(trainSparse) <- make.names(colnames(trainSparse))

test$comment_text<-as.character(test$comment_text)
test$comment_text <- sapply(test$comment_text, function(x) iconv(enc2utf8(x), sub = "byte"))
testCorpus <- VCorpus(VectorSource(test$comment_text))
testCorpus <- tm_map(testCorpus, content_transformer(tolower))
testCorpus <- tm_map(testCorpus, removePunctuation)
testCorpus <- tm_map(testCorpus, removeNumbers)
testCorpus <- tm_map(testCorpus, removeWords, c(stopwords("english")))
testCorpus <- tm_map(testCorpus, stemDocument)
tfrequencies = DocumentTermMatrix(testCorpus)
tsparse = removeSparseTerms(tfrequencies, 0.9)
testSparse <- as.data.frame(as.matrix(tsparse))
colnames(testSparse) <- make.names(colnames(testSparse))

istrain <- setdiff(colnames(trainSparse), colnames(testSparse))

trainSparse <- trainSparse[,!(names(trainSparse) %in% istrain)]

istest <- setdiff( colnames(testSparse), colnames(trainSparse))

testSparse <- testSparse[,!(names(testSparse) %in% istest)]

trainSparse  <-  sapply(trainSparse, function(x) {return ((x - min(x)) / (max(x) - min (x)) )})
testSparse  <-  sapply(testSparse, function(x) {return ((x - min(x)) / (max(x) - min (x)) )})

trainSparse$toxic <- train$toxic

net <- neuralnet(toxic ~ formula ,
                 data = trainSparse,
                 hidden = 8,
                 threshold = 0.01,
                 linear.output = F,
                 act.fct = 'logistic',
                 err.fct = 'sse')

net.pred <- compute(net, formula)
results <- data.frame(actual <- trainSparse$toxic, predict <- net.pred$net.result)
roundedResults <- sapply(results, round, digits=0)
roundedResults <- as.data.frame(roundedResults)
accuracy <- table(roundedResults)
accuracy.percent <- (accuracy[1,1] + accuracy[2,2])/(accuracy[1,1] + accuracy[2,2] + accuracy[1,2] + accuracy[2,1])
accuracy.percent
rmse <- sum((normalizedTrain$player.win - net.pred$net.result)^2/nrow(normalizedTrain))
rmse

plot(net, rep="best")