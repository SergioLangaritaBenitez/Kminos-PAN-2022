# Including needed libraries
library(XML)
library(tm)
library(splitstackshape)
library(caret)
library(tidytext)
library(dplyr)
library(ggplot2)

start.time <- Sys.time()

# Preparing parameters
n <- 100      # Number of words in the vocabulary. Usually used 1000 or 10000
k <- 2        # Number of folds in cross-validation. Usually used 10
r <- 1        # Number of repeats in cross-validation. Usually used 3
path_training <- "/home/sergio/Escritorio/Master/16.-Text Mining en Social Media/2021-2022TextMiningenSocialMedia/pan22-author-profiling-training-2022-03-29/en"	# Your training path
path_test <- "/home/sergio/Escritorio/Master/16.-Text Mining en Social Media/2021-2022TextMiningenSocialMedia/pan22-author-profiling-training-2022-03-29/en"			# Your test path
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

# Learning a SVM and evaluating it with k-fold cross-validation
 train_control <- trainControl( method="repeatedcv", number = k , repeats = r)
# model_SVM <- train( theclass~., data= training, trControl = train_control, method = "svmLinear")
# print(model_SVM)


# Learning a SVM with the whole training set and without evaluating it
#train_control <- trainControl(method="none")
#model_SVM_gender <- train( theclass~., data= training_gender, trControl = train_control, method = "svmLinear")

# GENERATING THE BOW FOR THE TEST SET
#bow_test <- GenerateBoW(path_test, vocabulary)

# Preparing the vector space model and truth for the test set
#test <- concat.split(bow_test, "V1", ",")
#truth <- as.factor(unlist(test[,2]))
#test <- test[,4:ncol(test)]

# Predicting and evaluating the prediction
#pred_SVM <- predict(model_SVM, test)

#confusionMatrix(pred_SVM, truth)


set.seed(2221)
#id <- createDataPartition(training, p=0.7,list=FALSE,times=1)
id <- sample(1:420,336, replace=FALSE) 
trainingtable<-slice(training,id)
test<-slice(training,-id)




docuFREQ<- trainingtable%>%summarise(across(where(is.numeric),~ sum(.x,na.rm=TRUE)))

trainingtable2 <-trainingtable[,2:ncol(trainingtable)]

datasetFinal<-trainingtable2%>%
  mutate_if(is.numeric,funs(./sum(.)))

columna<-trainingtable$theclass
columna <- as.numeric(trainingtable$theclass)
columna<-replace(columna,columna=="2",0)
datasetFinal$theclass<-as.factor(columna)


docuFREQ2 <- test%>%summarise(across(where(is.numeric),~ sum(.x,na.rm=TRUE)))

testtable <-test[,2:ncol(test)]

testFinal<-testtable%>%
  mutate_if(is.numeric,funs(./sum(.)))

 columna<-test$theclass
 columna <- as.numeric(test$theclass)
 columna<-replace(columna,columna=="2",0)
 columna <-as.factor(columna)
 columna <-as.data.frame(columna)
# testFinal$theclass<-columna
# 


 nnet <- train(theclass~., data=datasetFinal, method="nnet", trace=F)
 print(nnet)
#testFinal<-subset(testFinal, select=-c("theclass"))
 test.model<-predict(nnet,testFinal)
 #test.round<-as.factor(round(test.model,digits=0))
 #test.round<-replace(test.round,test.round=="2",0)
 test.round <-as.data.frame(test.model)
 confusionMatrix(test.round$test.model,columna$columna)




# 
# # trainingFinal<-training[id,]
# # test<- training[-id,]
# 
# rf <- train(theclass ~ .,
#             data = datasetFinal,
#             method = 'rf',
#             metric = 'Accuracy',
#             tuneLength  = 15, 
#             trControl = train_control)
# print(rf)
#  mtry<-30
#  tunegrid<- expand.grid(.mtry=mtry)
# 
# rf1 <- train(theclass ~ .,
#               data = datasetFinal,
#               method = 'rf',
#               metric = 'Accuracy',
#               tuneGrid = tunegrid, 
#               trControl = train_control)
#  print(rf1)
# 
# # plot(rf1, uniform=TRUE, main="Classification Tree")
# # text(rf1,  all=TRUE, cex=0.8)
# 
# test.model<-predict(rf1,testFinal)
# 
# #test.round<-as.factor(round(test.model,digits=0))
# # test.round<-replace(test.round,test.round=="2",0)
# test.round <-as.data.frame(test.model)
# confusionMatrix(test.round$test.model,columna$columna)
# 
# 





end.time <- Sys.time()
time.taken <- end.time - start.time

print(time.taken)



