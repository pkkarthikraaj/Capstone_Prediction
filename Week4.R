library(tm)
set.seed(123456789)
# load unigram and bigrams
load("oneGram.RData"); load("biGram.RData");
# clean ngram vector
oneGram <- oneGram[grep("^[a-z]+$", oneGram, perl = TRUE)]
biGram <- biGram[grep("^[a-z]+ [a-z]+$", biGram, perl = TRUE)]

oneGram <- data.frame(table(oneGram))
biGram <- data.frame(table(biGram))
sanitizeDataFrame <- function(df) {
  names(df) <- c("Term", "Count")
  df$Term <- as.character(df$Term)
  df <- df[order(df$Count, decreasing = TRUE), ]
  df
}
oneGram <- sanitizeDataFrame(oneGram);
format(object.size(oneGram), units = "MB") # [1] "7.2 Mb"
biGram <- sanitizeDataFrame(biGram)
format(object.size(biGram), units = "MB") # [1] "81 Mb"

oneGram <- oneGram[oneGram$Count!=1,]
library(dplyr)
oneGramDf <- sample_n(oneGram, nrow(oneGram) / 50)
biGramDf <- sample_n(biGram, nrow(biGram) / 16 )
format(object.size(biGramDf), units = "MB")

oneGramDfFinal <- oneGramDf
oneGramDfFinal_Tes <- oneGramDf
lengthOfOneGram <- nrow(oneGramDfFinal)

transitionMatrix <- matrix(data = rep(0, as.numeric(lengthOfOneGram) * as.numeric(lengthOfOneGram)),
                           nrow = as.numeric(lengthOfOneGram), ncol = as.numeric(lengthOfOneGram),
                           dimnames = list(oneGramDfFinal$Term, oneGramDfFinal$Term))


oneGramDfFinal$Probability <- oneGramDfFinal$Count / sum(oneGramDfFinal$Count)
lowestProbability <- min(oneGramDfFinal$Probability)

allPossibleNextWords <- dimnames(transitionMatrix)[[1]]

for (term in oneGramDfFinal[, "Term"]) {
  filteredBiGram <- biGramDf[grep(paste("^", term, " ", sep=""), biGramDf$Term, perl = TRUE), ]
  # get following words from Bi Gram
  nextWords <- c()
  for (phrase in filteredBiGram$Term) {
    splitedPhrase <- strsplit(phrase, " ")[[1]]
    nextWord <- splitedPhrase[2]
    nextWords <- c(nextWords, nextWord)
    print(nextWords)
  }
  # cross validate with oneGramDfFinal (e.g. how many nextWord found in oneGramDfFinal ?)
  predictableOneGramDf <- oneGramDfFinal[oneGramDfFinal$Term %in% nextWords, ]
  # all nextWords are not found ins one gram
  if (nrow(predictableOneGramDf) == 0) {
    equalProbability <- 1 / lengthOfOneGram
    for (name in allPossibleNextWords) {
      transitionMatrix[term, name] <- equalProbability
    }
    # some nextWords are foudn in one gram
  } else {
    # compute initial probabilities
    sumOfProbabilityForNotPredictables <- (lengthOfOneGram - nrow(predictableOneGramDf)) * lowestProbability
    sumOfRemainingProbabilityForPredictables <- 1 - sumOfProbabilityForNotPredictables
    # compute predictable probabilities
    predictableOneGramDf$Probability <- 
      (predictableOneGramDf$Count / sum(predictableOneGramDf$Count)) * sumOfRemainingProbabilityForPredictables
    # catch if suspecious
    summ <- sum(predictableOneGramDf$Probability)
    if (all.equal(summ, sumOfRemainingProbabilityForPredictables)) {
      warning(paste("[", term, "] Probability must always equal to 1. (", 
                    summ, " <> ", sumOfRemainingProbabilityForPredictables, ")", sep = ""))
    }
    # update transition matrix
    for (name in allPossibleNextWords) {
      if (name %in% predictableOneGramDf$Term) {
        transitionMatrix[term, name] <-
          predictableOneGramDf[predictableOneGramDf$Term == name, "Probability"]
      } 
      #else {
      #  transitionMatrix[term, name]  <- lowestProbability
      #}
    }
    summ <- sum(transitionMatrix[term, ])
    if (all.equal(summ, 1)) {
      warning(paste("[", term, "] Row did not sum to 1. (", summ, ")", sep = ""))
    }
  }
}

warnings()

save(transitionMatrix, file = "transitionMatrix.RData")
write.table(transitionMatrix, "transitionMatrix.txt")

load("./transitionMatrix.RData")

library("markovchain")
markovChainModel <- new("markovchain", transitionMatrix = transitionMatrix)
head(markovChainModel,1)

predictFollowingWord <- function(model, input, numberOfOutcome = 10) {
  inputString <- input
  inputStringParts <- strsplit(inputString, " ")[[1]]
  inputStringLength <- length(inputStringParts)
  dictionary <- states(model)
  
  getRandomIndex <- function (len) (len * runif(1)) + 1
  getRandomWord <- function (len, dictionary) dictionary[getRandomIndex(len)]
  
  currentState <- NULL
  nextState <- NULL
  cache <- list()
  cache$stateHistory <- c()
  
  
  currentState <- inputStringParts[10]
  print("i1")
  print(currentState)
  if (!currentState %in% dictionary)
    currentState <- getRandomWord(inputStringLength, dictionary)
  cache$stateHistory  <- c(cache$stateHistory, currentState)
  print("i2")
  print(currentState)
  print("i3")
  print(cache$stateHistory)
  
  remainingInputStringParts <- inputStringParts[2:inputStringLength]
  
  for (remainingInputString in remainingInputStringParts) {
    nextState <- remainingInputString
    
    if (!nextState %in% ?conditionalDistribution) {
      nextPossibilities <- conditionalDistribution(markovChainModel, currentState)
      
      nextStates <- dictionary[which.max(nextPossibilities)]
      print("i6")
      print(nextStates)
      if (length(nextStates) > 0) 
        nextState <- nextStates[getRandomIndex(length(nextStates))]
      else
        warning("Unable to find next state in model")
    }
    currentState <- nextState
    print("i4")
    print(currentState)
    cache$stateHistory  <- c(cache$stateHistory, currentState)
  }
  
  cache$conditionalProbabilities <- 
    sort(conditionalDistribution(markovChainModel, currentState),
         decreasing = TRUE)[1:numberOfOutcome]
  
  cache
}

preprocessInputText <- function(inputText) {
  corpus <- Corpus(VectorSource(inputText))
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  # corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stemDocument) # E.g. running and run may have different linguistic context
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, PlainTextDocument)
  print(corpus)
  return(as.character(corpus[[1]]))
}

week4 <- function() {
  questions <- c(
    "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
    "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
    "I'd give anything to see arctic monkeys this",
    "Talking to your mom has the same effect as a hug and helps reduce your",
    "When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
    "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
    "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
    "Every inch of you is perfect from the bottom to the",
    "I'm thankful my childhood was filled with imagination and bruises from playing",
    "I like how the same people are in almost all of Adam Sandler's")
  for (i in 1:length(questions)) {
    predictions <- predictFollowingWord(markovChainModel, preprocessInputText(questions[i]))
    print(predictions)
    # answer <- paste("Q", i, ": ", , sep = "")
    # print(answer)
  }
}

week4()