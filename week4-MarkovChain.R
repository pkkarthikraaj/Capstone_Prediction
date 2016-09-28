source("./Week3-Construct_Corpus.R")
library("markovchain")

predictFollowingWord <- function(model, input, numberOfOutcome) {
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
  
  currentState <- inputStringParts[1]
  # print(paste("first word:", currentState))
  if (!currentState %in% dictionary) 
    currentState <- getRandomWord(inputStringLength, dictionary)
  
  # print(paste("check dictionary:", currentState))
  cache$stateHistory  <- c(cache$stateHistory, currentState)
  
  remainingInputStringParts <- inputStringParts[2:inputStringLength]
  
  for (remainingInputString in remainingInputStringParts) {
    nextState <- remainingInputString
    # print(paste("next word:", nextState))
    if (!nextState %in% dictionary) {
      nextPossibilities <- conditionalDistribution(model, currentState)
      nextStates <- dictionary[which.max(nextPossibilities)]
      if (length(nextStates) > 0) 
        nextState <- nextStates[getRandomIndex(length(nextStates))]
      else
        warning("Unable to find next state in model")
    }
    
    currentState <- nextState
    
    cache$stateHistory  <- c(cache$stateHistory, currentState)
  }
  
  cache$conditionalProbabilities <- 
    sort(conditionalDistribution(model, currentState),
         decreasing = TRUE)[1:numberOfOutcome]
  
  cache
}

preprocessInputText <- function(inputText) {
  corpus <- Corpus(VectorSource(inputText))
  corpus <- transformCorpus(corpus)
  return(as.character(corpus[[1]]))
}

test <- function() {
  library(markovchain)
  load("transitionMatrix.RData");
  markovChainModel <- new("markovchain", transitionMatrix = transitionMatrix)
  # save(markovChainModel, file = "markovChainModel")
  predictedWords <- predictFollowingWord(markovChainModel, preprocessInputText("jokingly wished the two could"), 4)
  colnames(t(as.matrix(predictedWords$conditionalProbabilities)))
}
