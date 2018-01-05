library(tm)
library(SnowballC)

test$comment_text<-as.character(test$comment_text)
test$comment_text <- sapply(test$comment_text, function(x) iconv(enc2utf8(x), sub = "byte"))

testCorpus <- VCorpus(VectorSource(test$comment_text))
testCorpus <- tm_map(testCorpus, content_transformer(tolower))
testCorpus <- tm_map(testCorpus, removePunctuation)
testCorpus <- tm_map(testCorpus, removeNumbers)

testCorpus <- tm_map(testCorpus, removeWords, c(stopwords("english")))
testCorpus <- tm_map(testCorpus, stemDocument)

tfrequencies = DocumentTermMatrix(testCorpus)


tsparse = removeSparseTerms(tfrequencies, 0.996)

testSparse <- as.data.frame(as.matrix(tsparse))

colnames(testSparse) <- make.names(colnames(testSparse))
testSparse <- cbind(testSparse, istrain)

############ setando diferenças entre os dois dataframes para treinar a rede e prever##############
###################################################################################################
train <- setdiff(colnames(trainSparse), colnames(testSparse))
istrain <- as.data.frame(t(train))
istrain[1:226998,] = 0
dl_from_dropbox("istrain.csv","phd2nf9ggujwc1r")
testSparse <- cbind(testSparse, istrain)
dl_from_dropbox("istest.csv","82cj98ab9zccg3e")
istest <- setdiff( colnames(testSparse), colnames(trainSparse))
istest <- as.data.frame(t(istest))
istest[1:95851,] = 0

trainSparse <- cbind(trainSparse, istest)
###################################################################################################
################### treinando Decision Trees para aplicar no dataframe de teste ###################

library(caret)
library(rpart)
library(rpart.plot)
library(rdrop2)
library(RCurl)
dl_from_dropbox <- function(x, key) {
  require(RCurl)
  bin <- getBinaryURL(paste0("https://dl.dropboxusercontent.com/s/", key, "/", x),
                      ssl.verifypeer = FALSE)
  con <- file(x, open = "wb")
  writeBin(bin, con)
  close(con)
  message(noquote(paste(x, "read into", getwd())))
}
dl_from_dropbox("token.rds","169we6hudfy83u8")
token <- readRDS("token.rds")
drop_acc(dtoken = token)

dl_from_dropbox("trainSparse.csv", "v5zgwqafq9i8amw")
dl_from_dropbox("testSparse.csv","i2vabp8e9c6dygz")
dl_from_dropbox("train.csv","e99twlocb155w46")
dl_from_dropbox("test.csv","pfa79pglqxlij88")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

trainSparse <- read.csv("trainSparse.csv")
testSparse <- read.csv("testSparse.csv")

trainSparse$toxic <- train$toxic
#trainSparse$severe_toxic <- train$severe_toxic
#trainSparse$obscene <- train$obscene
#trainSparse$threat <- train$threat
#trainSparse$insult <- train$insult
#trainSparse$identity_hate <- train$identity_hate

traint <- rpart(toxic~., data=trainSparse, method='class', cp=0.00008)

####################################################################################

trainSparse$severe_toxic <- train$severe_toxic
#trainSparse$obscene <- train$obscene
#trainSparse$threat <- train$threat
#trainSparse$insult <- train$insult
#trainSparse$identity_hate <- train$identity_hate

trainst <- rpart(severe_toxic~., data=trainSparse, method='class', cp=0.00008)

####################################################################################

trainSparse$obscene <- train$obscene
#trainSparse$threat <- train$threat
#trainSparse$insult <- train$insult
#trainSparse$identity_hate <- train$identity_hate

traino <- rpart(obscene~., data=trainSparse, method='class', cp=0.00008)

####################################################################################

trainSparse$threat <- train$threat
#trainSparse$insult <- train$insult
#trainSparse$identity_hate <- train$identity_hate

trainth <- rpart(threat~., data=trainSparse, method='class', cp=0.00008)

####################################################################################

trainSparse$insult <- train$insult
#trainSparse$identity_hate <- train$identity_hate

traini <- rpart(insult~., data=trainSparse, method='class', cp=0.00008)

####################################################################################

trainSparse$identity_hate <- train$identity_hate

trainih <- rpart(identity_hate~., data=trainSparse, method='class', cp=0.00008)

####################################################################################
####################################################################################
########## Agora vamos tentar prever o output dos dados de teste ###################
####################################################################################
####################################################################################
####################################################################################
testSparse$toxic <- 0
#testSparse$severe_toxic <- 0
#testSparse$obscene <- 0
#testSparse$threat <- 0
#testSparse$insult <- 0
#testSparse$identity_hate <- 0
predictcv <- predict(traint, newdata=testSparse, type="class")
####################################################################################
testSparse$toxic <- as.numeric(predictcv)
####################################################################################
####################################################################################
testSparse$severe_toxic <- 0
#testSparse$obscene <- 0
#testSparse$threat <- 0
#testSparse$insult <- 0
#testSparse$identity_hate <- 0
predictcv <- predict(trainst, newdata=testSparse, type="class")
####################################################################################
testSparse$severe_toxic <- as.numeric(predictcv)
####################################################################################
####################################################################################
testSparse$obscene <- 0
#testSparse$threat <- 0
#testSparse$insult <- 0
#testSparse$identity_hate <- 0
predictcv <- predict(traino, newdata=testSparse, type="class")
####################################################################################
testSparse$obscene <- as.numeric(predictcv)
####################################################################################
testSparse$threat <- 0
#testSparse$insult <- 0
#testSparse$identity_hate <- 0
predictcv <- predict(trainth, newdata=testSparse, type="class")
####################################################################################
testSparse$threat <- as.numeric(predictcv)
####################################################################################
testSparse$insult <- 0
#testSparse$identity_hate <- 0
predictcv <- predict(traini, newdata=testSparse, type="class")
####################################################################################
testSparse$insult <- as.numeric(predictcv)
####################################################################################
testSparse$identity_hate <- 0
predictcv <- predict(trainih, newdata=testSparse, type="class")
####################################################################################
testSparse$identity_hate <- as.numeric(predictcv)
####################################################################################
result <- NULL
result$id <- NULL
result$toxic <- NULL
result$severe_toxic <- NULL
result$obscene <- NULL
result$threat <- NULL
result$insult <- NULL
result$identity_hate <- NULL

result$id <- test$id
result$toxic <- testSparse$toxic
result$severe_toxic <- testSparse$severe_toxic
result$obscene <- testSparse$obscene
result$threat <- testSparse$threat
result$insult <- testSparse$insult
result$identity_hate <- testSparse$identity_hate
write.csv(result, "result.csv")
drop_upload("result.csv")
