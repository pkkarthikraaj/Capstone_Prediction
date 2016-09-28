library(tm);

makeCorpus <- function(d) {
  vectorSource <- c()
  files <- list.files(d)
  for (file in files) {
    fileFullPath <- paste(d, file, sep = filePathSep)
    vectorSource <- c(vectorSource, readLines(fileFullPath))
  }
  ovid <- Corpus(VectorSource(vectorSource))
  ovid
}

removeNonAscii <- content_transformer(function(input) { 
  iconv(input, "latin1", "ASCII", sub="")
})

removeExplicitPunctuation <- content_transformer(function(input) {
  output <- gsub("[[:punct:]]", " ", input)
  output
})

removeLeadingEndingWhiteSpaces <- content_transformer(function(input) {
  output <- gsub("^\\s+", "", input)
  output <- gsub("\\s+$", "", output)
  output
})

transformCorpus <- function(corpus) {
  corpus <- tm_map(corpus, removeNonAscii)
  corpus <- tm_map(corpus, removeExplicitPunctuation)
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  # corpus <- tm_map(corpus, removeWords, stopwords("english"))
  # corpus <- tm_map(corpus, stemDocument, mc.cores = 3) # E.g. running and run may have different linguistic context
  profanityRdata <- "profanity.RData"
  if (file.exists(profanityRdata)) {
    if (!exists("profanity")) {
      load(profanityRdata)
    }
    corpus <- tm_map(corpus, removeWords, profanity)
  }
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, PlainTextDocument)
  corpus <- tm_map(corpus, removeLeadingEndingWhiteSpaces)
  corpus
}

preprocessText <- function(text) {
  as.character(transformCorpus(Corpus(VectorSource(text)))[[1]])
}

tagDocumentWithId <- function(corpus) {
  for(i in c(1 : length(corpus))) {
    DublinCore(corpus[[i]], "id") <- i
  }
  corpus
}