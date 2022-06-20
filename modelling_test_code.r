## Including needed libraries
library(XML)
library(tm)
library(splitstackshape)
library(caret)
library(tidytext)
library(dplyr)
library(ggplot2)
library(groupdata2)

start.time <- Sys.time()

# Preparing parameters
n <- 100      # Number of words in the vocabulary. Usually used 1000 or 10000
k <- 10        # Number of folds in cross-validation. Usually used 10
r <- 3        # Number of repeats in cross-validation. Usually used 3
path_training <- "/home/lorena-pr/Escritorio/SocialMedia/pan22-author-profiling-training-2022-03-29/en"
path_test <- 	"/home/lorena-pr/Escritorio/SocialMedia/pan22-author-profiling-training-2022-03-29/en"
lang <- "en"

# Auxiliar functions
# * freq_terms: Given a text, it extracts the n most frequent terms
# * Plot: Given a set of pairs term, frequency, it plots the distribution
# * GenerateVocabulary: Given a corpus (training set), obtains the n most frequent words
# * GenerateBoW: Given a corpus (training or test), and a vocabulary, obtains the bow representation

freq_terms <- function(corpus.preprocessed, n = 1000) {
  #https://dk81.github.io/dkmathstats_site/rtext-freq-words.html
  corpus.text <- data_frame(Text = corpus.preprocessed)
  corpus.words <- corpus.text %>% unnest_tokens(output = word, input = Text)
  corpus.wordcounts <- corpus.words %>% count(word, sort = TRUE)
  corpus.frequentterms <- corpus.wordcounts[1:n,]
  names(corpus.frequentterms) <- c("WORD", "FREQ")
  
  return (corpus.frequentterms)
}

Plot <- function(wordcounts) {
  wordcounts %>% 
    filter(FREQ > 70) %>% 
    mutate(WORD = reorder(WORD, FREQ)) %>% 
    ggplot(aes(WORD, FREQ)) + 
    geom_col() +
    coord_flip() +
    labs(x = "Word \n", y = "\n Count ", title = "Frequent Words \n") +
    geom_text(aes(label = FREQ), hjust = 1.2, colour = "white", fontface = "bold") +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
          axis.title.y = element_text(face="bold", colour="darkblue", size = 12))
}

# GenerateVocabulary: Given a corpus (training set), obtains the n most frequent words
GenerateVocabulary <- function(path, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", verbose = TRUE) {
  setwd(path)
  
  # Reading corpus list of files
  files = list.files(pattern="*.xml")
  
  # Reading files contents and concatenating into the corpus.raw variable
  corpus.raw <- NULL
  i <- 0
  for (file in files) {
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
    i <- i + 1
    if (verbose) print(paste(i, " ", file))
  }
  
  # Preprocessing the corpus
  corpus.preprocessed <- corpus.raw
  
  if (lowcase) {
    if (verbose) print("Tolower...")
    corpus.preprocessed <- tolower(corpus.preprocessed)
  }
  
  if (punctuations) {
    if (verbose) print("Removing punctuations...")
    corpus.preprocessed <- removePunctuation(corpus.preprocessed)
  }
  
  if (numbers) {
    if (verbose) print("Removing numbers...")
    corpus.preprocessed <- removeNumbers(corpus.preprocessed)
  }
  
  if (whitespaces) {
    if (verbose) print("Stripping whitestpaces...")
    corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
  }
  
  if (swlang!="")	{
    if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
  }
  
  if (swlist!="") {
    if (verbose) print("Removing provided stopwords...")
    corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
  }
  
  # Generating the vocabulary as the n most frequent terms
  if (verbose) print("Generating frequency terms")
  
  corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
  
  if (verbose) Plot(corpus.frequentterms)
  
  return (corpus.frequentterms)
}

# GenerateBoW: Given a corpus (training or test), and a vocabulary, obtains the bow representation
GenerateBoW <- function(path, vocabulary, n = 100000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", verbose = TRUE) {
  setwd(path)
  
  # Reading the truth file
  truth <- read.csv("truth.txt", sep=":", header=FALSE)
  truth <- truth[,c(1,4)]
  colnames(truth) <- c("author", "class")
  
  i <- 0
  bow <- NULL
  # Reading the list of files in the corpus
  files = list.files(pattern="*.xml")
  for (file in files) {
    # Obtaining truth information for the current author
    author <- gsub(".xml", "", file)
    class <- truth[truth$author==author,"class"]
    
    if (class=="I") {
      class = "ironic"
    } else {
      class = "normal"
    }
    
    # Reading contents for the current author
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    txtdata <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
    
    # Preprocessing the text
    if (lowcase) {
      txtdata <- tolower(txtdata)
    }
    
    if (punctuations) {
      txtdata <- removePunctuation(txtdata)
    }
    
    if (numbers) {
      txtdata <- removeNumbers(txtdata)
    }
    
    if (whitespaces) {
      txtdata <- stripWhitespace(txtdata)
    }
    
    # Building the vector space model. For each word in the vocabulary, it obtains the frequency of occurrence in the current author.
    line <- author
    freq <- freq_terms(txtdata, n)
    for (word in vocabulary$WORD) {
      thefreq <- 0
      #if (length(freq[freq$WORD==word,"FREQ"])>0) {
      if (is.na(freq[freq$WORD==word, "FREQ"]$FREQ[1])) {
        0
      } else {
        thefreq <- freq[freq$WORD==word,"FREQ"]$FREQ[1]
      }
      
      line <- paste(line, ",", thefreq, sep="")
    }
    
    line <- paste(class, ",", line, sep="")
    
    # New row in the vector space model matrix
    bow <- rbind(bow, line)
    i <- i + 1
    
    if (verbose) {
      print(paste(i, author, class))
    }
  }
  
  return (bow)
}

# GENERATE VOCABULARY
vocabulary <- GenerateVocabulary(path_training, n, swlang=lang)

# GENERATING THE BOW FOR THE TRAINING SET
bow_training <- GenerateBoW(path_training, vocabulary)

# PREPARING THE VECTOR SPACE MODEL FOR THE TRAINING SET
training <- concat.split(bow_training, "V1", ",")
training <- cbind(training[,2], training[,4:ncol(training)])
names(training)[1] <- "theclass"

# TRAINING MODELS 
## DF 1 - BAG OF WORDS

# splitting training dataset to test models
set.seed(2221)

# split option 1
# id <- sample(1:420,420, replace=FALSE)
# train_set <-slice(training,id)
# test_set <-data.frame(train_set)

# split option 2
# id <- createDataPartition(training, p=0.7,list=FALSE,times=1)
# train_set <-training[id,]
# test_sest <- training[-id,]

# split option 3 - balance theClass
partitions <- partition(data = training, p = 0.7, cat_col = "theclass")
train_set <- partitions[[1]]
table(train_set$theclass)

test_set <- partitions[[2]]
table(test_set$theclass)
test_set_res <- as.factor(test_set$theclass)
test_set_val <- test_set[,-1]

# Learning a Machine Learning Algorithm and evaluating it with k-fold cross-validation
train_control <- trainControl( method="repeatedcv", number = k , repeats = r)

## Support Vector Machine
print("SVM")

set.seed(01234)
model.SVM <- train( theclass~., data= train_set, trControl = train_control, method = "svmLinear")
print(model.SVM)

## General Linear Model
print("GLM")

set.seed(0123)
model.GLM <- train(theclass~., data = train_set,
                   method = 'glm',
                   trControl = train_control,
                   family = 'binomial')
print(model.GLM)

# Árboles de decisión
set.seed(0123)
model.tree = train(theclass ~ ., 
              data=train_set, 
              method="rpart",
              trControl = train_control)
print(model.tree)
summary(model.tree)
 
plot(model.tree$finalModel, uniform=TRUE, main="Classification Tree")

# Random Forest
print("RF")
set.seed(01234)
model.rf <- train(theclass ~ .,
            data = train_set,
            method = 'rf',
            trControl = train_control)
print(model.rf)

#--------
# Predicting and evaluating the prediction
pred_SVM <- predict(model.SVM, test_set_val)
confusionMatrix(pred_SVM, test_set_res)

pred.GLM <- predict(model.GLM, test_set_val)
confusionMatrix(pred.GLM, test_set_res)

pred_tree <- predict(model.tree, test_set_val)
confusionMatrix(pred_tree, test_set_res)

pred.rf <- predict(model.rf, test_set_val)
confusionMatrix(pred.rf, test_set_res)

##-----------------------------
## DF 2 - BAG OF WORDS SELECTING THE RELEVANT CHARACTERS FROM TREES
training2<-select(training,"theclass","V1_0004", "V1_0102", "V1_0700", "V1_0817", "V1_0375", "V1_0486", "V1_0167", "V1_0914", "V1_0007", "V1_0006", "V1_0017", "V1_0152")

# split
partitions <- partition(data = training2, p = 0.7, cat_col = "theclass")
train_set <- partitions[[1]]
table(train_set$theclass)

test_set <- partitions[[2]]
table(test_set$theclass)
test_set_res <- as.factor(test_set$theclass)
test_set_val <- test_set[,-1]

# Testing different models
## Support Vector Machine
set.seed(01234)
model.SVM.2 <- train( theclass~., data= training2, trControl = train_control, method = "svmLinear")
print(model.SVM.2)

## General Linear Model
set.seed(0123)
model.GLM.2 <- train(theclass~., data = train_set,
                   method = 'glm',
                   trControl = train_control,
                   family = 'binomial')
print(model.GLM.2)

# Árboles de decisión
model.tree.2 = train(theclass ~ ., 
                   data=training2, 
                   method="rpart",
                   trControl = train_control)
print(model.tree.2)

# Random Forest
set.seed(01234)
model.rf.2 <- train(theclass ~ .,
                  data = train_set,
                  method = 'rf',
                  trControl = train_control)
print(model.rf.2)

# Predicting and evaluating the prediction
pred_SVM <- predict(model.SVM.2, test_set_val)
confusionMatrix(pred_SVM, test_set_res)

pred.GLM <- predict(model.GLM.2, test_set_val)
confusionMatrix(pred.GLM, test_set_res)

pred_tree <- predict(model.tree.2, test_set_val)
confusionMatrix(pred_tree, test_set_res)

pred.rf <- predict(model.rf.2, test_set_val)
confusionMatrix(pred.rf, test_set_res)


# Convert theClass in numeric to perform a Linear Regression
training2.1 <-training2
training2.1$theclass <- as.numeric(as.factor(training2.1$theclass))

# split
partitions <- partition(data = training2.1, p = 0.7, cat_col = "theclass")
train_set <- partitions[[1]]
table(train_set$theclass)
 
test_set <- partitions[[2]]
table(test_set$theclass)
test_set_res <- as.factor(test_set$theclass)
test_set_val <- test_set[,-1]
 
set.seed(0123)
model.lm.2 <- train(theclass~.,
                   data = train_set,
                   method  = "lm",
                   trControl = train_control, 
                   tuneGrid  = expand.grid(intercept = FALSE))
print(model.lm.2)

# Predicting and evaluating the prediction
pred.lm <- predict(model.lm.2, test_set_val)

for (i in 1:length(pred.lm)) {
  if (pred.lm[[i]]<=1.5) {
    pred.lm[i] <- 1
  } else{
    pred.lm[i] <- 2
}
}
pred.lm <- as.factor(pred.lm)
confusionMatrix(pred.lm, test_set_res)

##-----------------------------
## DF 3 - SELECTING THE MOST CORRELATED CHARACTERS
print("df3")
training3<-training
training3$theclass <- as.numeric(as.factor(training3$theclass))

classcorrelation<-cor(training3)
classcorrelationKV <-rownames(classcorrelation)
classcorrelationKV <- data_frame(classcorrelationKV,classcorrelation[,1])
classcorrelationKV = classcorrelationKV[-c(1),]
names(classcorrelationKV) <-c('key','value')
more_corr <- classcorrelationKV[which(abs(classcorrelationKV$value) >=0.2),1]
training4 <-select(training, "theclass",more_corr$key)

# split
partitions <- partition(data = training4, p = 0.7, cat_col = "theclass")
train_set <- partitions[[1]]
table(train_set$theclass)

test_set <- partitions[[2]]
table(test_set$theclass)
test_set_res <- as.factor(test_set$theclass)
test_set_val <- test_set[,-1]

# Testing different models
## Support Vector Machine
set.seed(01234)
model.SVM.3 <- train( theclass~., data= train_set, trControl = train_control, method = "svmLinear")
print(model.SVM.3)

## General Linear Model
set.seed(0123)
model.GLM.3 <- train(theclass~., data = train_set,
                     method = 'glm',
                     trControl = train_control,
                     family = 'binomial')
print(model.GLM.3)

# Árboles de decisión
model.tree.3 = train(theclass ~ ., 
                     data=train_set, 
                     method="rpart",
                     trControl = train_control)
print(model.tree.3)

# Random Forest
set.seed(01234)
model.rf.3 <- train(theclass ~ .,
                    data = train_set,
                    method = 'rf',
                    trControl = train_control)
print(model.rf.3)

set.seed(0123)
model.lm.3 <- train(theclass~.,
                    data = train_set,
                    method  = "lm",
                    trControl = train_control, 
                    tuneGrid  = expand.grid(intercept = FALSE))
print(model.lm.3)


# Predicting and evaluating the prediction
pred_SVM <- predict(model.SVM.3, test_set_val)
for (i in 1:length(pred_SVM)) {
  if (pred_SVM[[i]]<=1.5) {
    pred_SVM[i] <- 1
  } else{
    pred_SVM[i] <- 2
  }
}
pred_SVM <- as.factor(pred_SVM)
confusionMatrix(pred_SVM, test_set_res)

vec <- vector()
pred.GLM <- predict(model.GLM.3, test_set_val)
for (i in 1:length(pred.GLM)) {
  if (pred.GLM[[i]]=="ironic") {
    vec[i] <- 1
  } else{
    vec[i] <- 2
  }
}
pred.GLM <- as.factor(vec)
confusionMatrix(pred.GLM, test_set_res)

pred_tree <- predict(model.tree.3, test_set_val)
for (i in 1:length(pred_tree)) {
  if (pred_tree[[i]]<=1.5) {
    pred_tree[i] <- 1
  } else{
    pred_tree[i] <- 2
  }
}
pred_tree <- as.factor(pred_tree)
confusionMatrix(pred_tree, test_set_res)

pred.rf <- predict(model.rf.3, test_set_val)
for (i in 1:length(pred.rf)) {
  if (pred.rf[[i]]<=1.5) {
    pred.rf[i] <- 1
  } else{
    pred.rf[i] <- 2
  }
}
pred.rf <- as.factor(pred.rf)
confusionMatrix(pred.rf, test_set_res)

pred.lm <- predict(model.lm.3, test_set_val)
for (i in 1:length(pred.lm)) {
  if (pred.lm[[i]]<=1.5) {
    pred.lm[i] <- 1
  } else{
    pred.lm[i] <- 2
  }
}
pred.lm <- as.factor(pred.lm)
confusionMatrix(pred.lm, test_set_res)
sink()

##-----------------------------
# Normalizar y pasar red neuronal
# trainingNorm <-scale(training3[,c(2)],center=T,scale=T)
# parametros <- train(theclass~., data=datos.train, method="nnet", trace=F)

##-----------------------------
# Mejor modelo
# rf <- train(theclass ~ .,
#             data = trainingFinal,
#             method = 'rf',
#             metric = 'Accuracy',
#             tuneLength  = 15, 
#             trControl = train_control)
# print(rf)

# Mejor ajuste (mtry = 69)
#mtry<-69
#tunegrid<- expand.grid(.mtry=mtry)

#rf1 <- train(theclass ~ .,
#            data = trainingFinal,
#            method = 'rf',
#            metric = 'Accuracy',
#            tuneGrid = tunegrid, 
#            trControl = train_control)
#print(rf1)

# plot(rf1, uniform=TRUE, main="Classification Tree")
# text(rf1,  all=TRUE, cex=0.8)

end.time <- Sys.time()
time.taken <- end.time - start.time

print(time.taken)