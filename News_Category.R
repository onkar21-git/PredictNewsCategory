setwd("C:\\Personal\\study\\DataScience\\Hackathons\\PredictNewsCategory\\PredictNewsCategory_Project")
library("readxl")
News1 = read_excel("Data_Train.xlsx")
News2 = read_excel("Data_Test.xlsx")
News2$SECTION = NA
#News_train = read_excel("Data_Train.xlsx")
News_train = rbind(News1,News2)

# Cleaning the texts
# install.packages('tm')
# install.packages('SnowballC')
library(tm)
library(SnowballC)
corpus = VCorpus(VectorSource(News_train$STORY))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords()) #remove useless words
corpus = tm_map(corpus, stemDocument) #ex. chop, chopping, chopped are converted into chop
corpus = tm_map(corpus, stripWhitespace)


# Creating the Bag of Words model
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.975)
dataset = as.data.frame(as.matrix(dtm))
dataset$SECTION = News_train$SECTION
dataset$SECTION = as.factor(dataset$SECTION)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
#set.seed(123)
#split = sample.split(dataset$SECTION, SplitRatio = 0.8)
#training_set = subset(dataset, split == TRUE)
#test_set = subset(dataset, split == FALSE)

training_set = dataset[1:7628,]
test_set = dataset[7629:10376,]

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-419],
                          y = training_set$SECTION,
                          ntree = 200)

# Predicting the results of train set
y_pred = predict(classifier, newdata = training_set[-419])
View(y_pred)

# Making the Confusion Matrix
cm = table(training_set$SECTION, y_pred)
cm
sum(diag(cm))/sum(cm) #calculting accuracy

# Predicting the results of test set
y_pred_test = predict(classifier, newdata = test_set[-419])
View(y_pred_test)
write.csv(y_pred_test,"output.csv")


#Naive Bays------------------------------------------------------------------
library(e1071)
snb=naiveBayes(training_set$SECTION~.,training_set)
snb.pred=predict(snb,training_set[-419])
cm2 = table(training_set[,419], snb.pred)
cm2
sum(diag(cm2))/sum(cm2) #calculting accuracy
# Predicting the results of test set
y_pred_test_N = predict(snb,test_set[-419])
View(y_pred_test_N)
write.csv(y_pred_test_N,"output_Naive.csv")

