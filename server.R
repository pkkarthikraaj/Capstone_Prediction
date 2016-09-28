library(shiny)

source("./week4-MarkovChain.R")

# load("./markovChainModel.RData")
# markovModel <- markovChainModel
# rm(markovChainModel)

load("./markovChainModelReference.RData")
markovModel <- markovChainModelReference
rm(markovChainModelReference)

isValid <- function(input) {
  if (length(input) == 0) FALSE
  else if (length(input[grep("^\\W+$", input, perl = TRUE)])) FALSE
  else if (length(input[grep("^\\d+$", input, perl = TRUE)])) FALSE
  else if (length(input) == 1 && input[1] == "") FALSE
  else if (length(input) == 1 && input[1] != "") TRUE
  else FALSE
}

predictionModelHandler <- function(model, input, numToPredict) {
  if (isValid(input)) {
    print(paste(input, " [", numToPredict, "]", collapse = ""))
    predictedWords <- predictFollowingWord(model, preprocessText(input), numToPredict)
    predictedWordsMatrix <- t(as.matrix(predictedWords$conditionalProbabilities))
    return(paste(colnames(predictedWordsMatrix), collapse = ", "))
  } else {
    return("<Please use a valid input>")
  }
}

shinyServer(
  function(input, output) {    
    print("Request received!")
    
    reactiveInputHandler1 <- reactive({
      if (isValid(input$inputText)) return(paste("\"", input$inputText, "\"", sep = ""))
      else return("<Please use a valid input>")
    })
    
    output$inputText <- renderText(reactiveInputHandler1())
    
    reactiveInputHandler2 <- reactive({
      if (isValid(input$inputText)) return(paste("\"", preprocessText(input$inputText), "\"", sep = ""))
      else return("<Please use a valid input>")
    })
    
    output$preprocessedInputText <- renderText(reactiveInputHandler2())
    
    reactiveInputHandler3 <- reactive({
      predictionModelHandler(markovModel, preprocessText(input$inputText), input$numToPredict)
    })
    
    output$predictedWords <- renderText(reactiveInputHandler3())
  })