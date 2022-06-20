## Including needed libraries
library(XML)
library(tm)
library(splitstackshape)
library(caret)
library(tidytext)
library(dplyr)
library(ggplot2)

start.time <- Sys.time()

# Preparing parameters
n <- 1000      # Number of words in the vocabulary. Usually used 1000 or 10000
k <- 10      # Number of folds in cross-validation. Usually used 10
r <- 3        # Number of repeats in cross-validation. Usually used 3
setwd("/home/sergio/Escritorio/Master/")
path_training <- "/home/sergio/Escritorio/Master/16.-Text Mining en Social Media/2021-2022TextMiningenSocialMedia/pan22-author-profiling-training-2022-03-29/en"	# Your training path
path_test <- "/home/sergio/Escritorio/Master/16.-Text Mining en Social Media/pan22-author-profiling-test-2022-04-22-without_truth/en"			# Your test path
resultados <-"/home/sergio/Escritorio/Master/16.-Text Mining en Social Media/Kminos-PAN-2022/resultados/"
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
######################################3
GenerateBoWTest <- function(path, vocabulary, n = 100000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", verbose = TRUE) {
  setwd(path)
  
  i <- 0
  bow <- NULL
  # Reading the list of files in the corpus
  files = list.files(pattern="*.xml")
  for (file in files) {
    # Obtaining truth information for the current author
    author <- gsub(".xml", "", file)

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

    # New row in the vector space model matrix
    bow <- rbind(bow, line)
    i <- i + 1
    
    if (verbose) {
      print(paste(i, author))
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

# TRAINING THE MODEL
train_control <- trainControl( method="repeatedcv", number = k , repeats = r)

rf <- train(theclass ~ .,
            data = training,
            method = 'rf',
            metric = 'Accuracy',
            tuneLength  = 15, 
            trControl = train_control)
print(rf)

# MODELO RF-1
#mtry<-69
#tunegrid<- expand.grid(.mtry=mtry)
#rf1 <- train(theclass ~ .,
#            data = training,
#            method = 'rf',
#            metric = 'Accuracy',
#            tuneGrid = tunegrid, 
#            trControl = train_control)
#print(rf1)

#plot(rf1, uniform=TRUE, main="Classification Tree")
#text(rf1,  all=TRUE, cex=0.8)

# GENERATING THE BOW FOR THE TEST SET
#vocabulary.test <- GenerateVocabulary(path_test, n, swlang=lang)
bow_test <- GenerateBoWTest(path_test, vocabulary)

# PREDICTING OVER THE TEST
test <- concat.split(bow_test, "V1", ",")
test <- cbind(test2[,2], test2[,3:ncol(test2)])
test.model<-predict(rf,test)
test.modelizado.elbueno <- data.frame(test.model)


final <- cbind(test2[,1],test.modelizado.elbueno[ ,1])
final$resultado[final$"V2" == 'ironic'] <- "I"
final$resultado[final$"V2" == 'normal'] <- "NI"
final <- cbind(final[,1],final[,3])

##---------- WRITTING THE RESULTS IN .XML -----------##
setwd(path_test)

for (i in 1:nrow(final)) {
   # Obtaining truth information for the current author
   author <-final$V1_0001[i]
   print(author)
   var<-final[i,2]
   path_resultados <- paste(resultados,author,".xml", sep="")
   texto <- paste("<author id=\"",author,"\" lang=\"",lang,"\" type=\"",var,"\" />",sep="")
   print(texto)
   print(path_resultados)
   con <- file(path_resultados, open="w")
   documento <- writeLines(text = texto, con = con)
   close(con)
 }

 
end.time <- Sys.time()
time.taken <- end.time - start.time
 
print(time.taken)
