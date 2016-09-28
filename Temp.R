library(slam)
library(reshape2)
library(tm)
library(ggplot2)
library(wordcloud)
library(RWeka)

set.seed(55669)

filePathSep <- "\\"
fileNameSep <- "."
swiftKeyDirectory <- "C:/Karthik/SData_Science/Data_Prediction_Capstone/SCapstone"
finalDirectory <- paste(swiftKeyDirectory, "final", sep = filePathSep)
outputDirectory <- paste(swiftKeyDirectory, "output", sep = filePathSep) 
localesAvail <- c("de_DE", "en_US", "fi_FI", "ru_RU")
locales <- localesAvail[2]
contexts <- c("blogs", "news", "twitter")
fileExt <- "txt"

enUsOutputDirectory <- paste(outputDirectory, locales, sep = filePathSep)

makeCorpus <- function(d) {
  dirSource <- DirSource(directory = d, encoding = "UTF-8")
  ovid <- VCorpus(dirSource, readerControl = list(language = "eng"))
  on.exit(close(dirSource))
  ovid
}

ovid <- makeCorpus(enUsOutputDirectory)

transformCorpus <- function(corpus) {
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  # corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, PlainTextDocument)
  corpus
}

ovid <- transformCorpus(ovid)

tagDocumentWithId <- function(corpus) {
  for(i in c(1 : length(corpus))) {
    DublinCore(corpus[[i]], "id") <- i
  }
  corpus
}

ovid <- tagDocumentWithId(ovid)

gramTokenizer <- function(n) {
  NGramTokenizer(ovid, Weka_control(min = n, max = n, delimiters = " \\r\\n\\t.,;:\"()?!"))
}

oneGram <- gramTokenizer(1)
save(oneGram, file="oneGram.RData")
biGram <- gramTokenizer(2)
save(biGram, file="biGram.RData")
triGram <- gramTokenizer(3)
save(triGram, file="triGram.RData")
fourGram <- gramTokenizer(4)
save(fourGram, file="fourGram.RData")

