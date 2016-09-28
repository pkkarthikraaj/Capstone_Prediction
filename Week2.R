library(slam)
library(reshape2)
library(NLP)
library(tm)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(rJava)
library(RWeka)

set.seed(55669)

filePathSep <- "\\"
fileNameSep <- "."
swiftKeyDirectory <- "C:/Karthik/SData_Science/Capstone_Project/SCapstone"
finalDirectory <- paste(swiftKeyDirectory, "final", sep = filePathSep)
outputDirectory <- paste(swiftKeyDirectory, "output", sep = filePathSep) 
localesAvail <- c("de_DE", "en_US", "fi_FI", "ru_RU")
locales <- localesAvail[2]
contexts <- c("blogs", "news", "twitter")
fileExt <- "txt"

#file_news <- "C:/Karthik/SData_Science/Capstone_Project/SCapstone/final/en_US/en_US.news.txt"
#ent <- readLines('final/en_US/en_US.twitter.txt', encoding = 'UTF-8')
#enn <- readLines('final/en_US/en_US.news.txt', encoding = 'UTF-8')
#enb <- readLines('final/en_US/en_US.blogs.txt', encoding = 'UTF-8')
#newent <- sample(ent, length(ent) * 0.01)


getFileInfo <- function(directory) {
  df <- data.frame(name = c(), size = c())
  for (locale in locales) {
    for (context in contexts) {
      fileName <- paste(locale, context, fileExt, sep = fileNameSep)
      fullQualifiedFileName <- paste(directory, locale, fileName, sep = filePathSep)
      if (file.exists(fullQualifiedFileName) == TRUE) {
        fInfo <- file.info(fullQualifiedFileName)
        fileSizeInMb <- paste(round(fInfo$size / 1024 / 1024, 2), "MB")
        df <- rbind(df, data.frame(name = fileName, size = fileSizeInMb))
      } else {
        stop("File not found!") 
      }
    }
  }
  df
}

makeFqnOutputFilePath <- function(locale, context) {
  localeDirectory <- paste(outputDirectory, locale, sep = filePathSep)
  dir.create(localeDirectory, showWarnings = FALSE, recursive = TRUE)
  fileName <- paste(locale, context, fileExt, sep = fileNameSep)
  fqnOutputFileName <- paste(localeDirectory, fileName, sep = filePathSep)
  fqnOutputFileName
}


makeReducedData <- function(fileName, factor = 0.01) {
  connection <- file(fileName, "rb")
  contents <- readLines(connection, encoding = "UTF-8", skipNul = TRUE)
  length(contents) 
  newContents <- sample(contents, length(contents) * factor)
  on.exit(close(connection))
  newContents
}

writeDataToFile <- function(fileName, data, printFileName = FALSE) {
  write(data, file = fileName) # over write file
  if(printFileName == TRUE) print(fileName)
}

makeSampleFiles <- function() {
  for (locale in locales) {
    for (context in contexts) {
      fileName <- paste(locale, context, fileExt, sep = fileNameSep)
      fullQualifiedFileName <- paste(finalDirectory, locale, fileName, sep = filePathSep)
      if (file.exists(fullQualifiedFileName) == TRUE) {
        writeDataToFile(
          makeFqnOutputFilePath(locale, context), 
          makeReducedData(fullQualifiedFileName))
      } else {
        stop("File not found!") 
      }
    }
  }
}

makeSampleFiles()

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

Sys.time()
documentTermMatrix <- DocumentTermMatrix(ovid) # This will take a while
Sys.time()


termDocumentMatrix <- as.TermDocumentMatrix(documentTermMatrix)
termDocumentMatrix2 <- removeSparseTerms(termDocumentMatrix, 0.1)
tx <- as.matrix(termDocumentMatrix)

termDocumentMatrix3 <- as.matrix(termDocumentMatrix2)
termDocumentMatrix4 <- melt(termDocumentMatrix3, value.name = "Count")
termDocumentMatrix5 <- aggregate(Count ~ Terms, data = termDocumentMatrix4, sum)
termDocumentMatrix6 <- termDocumentMatrix5[order(termDocumentMatrix5$Count, decreasing = TRUE), ]
termDocumentMatrix6$Terms <- as.character(termDocumentMatrix6$Terms)

wordcloud(termDocumentMatrix6$Terms, termDocumentMatrix6$Count, 
          random.order = FALSE, rot.per = 0.35,
          max.words = 150, colors = brewer.pal(6, "Dark2"))

gramTokenizer <- function(n) {
  NGramTokenizer(ovid, Weka_control(min = n, max = n, delimiters = " \\r\\n\\t.,;:\"()?!"))
}

oneGram <- gramTokenizer(1)
biGram <- gramTokenizer(2)
triGram <- gramTokenizer(3)
fourGram <- gramTokenizer(4)
fiveGram <- gramTokenizer(5)
save(fiveGram, file = "fiveGram.RData")

oneGramDf <- data.frame(table(oneGram))
biGramDf <- data.frame(table(biGram))
triGramDf <- data.frame(table(triGram))
fourGramDf <- data.frame(table(fourGram))
fiveGramDf <- data.frame(table(fiveGram))
save(fiveGram, file = "fiveGram.RData")


sanitizeGramDf <- function(df) {
  newDf <- data.frame(Term = as.character(df[, 1]), Count = df[, 2])
  newDf$Term <- as.character(newDf$Term)
  newDf
}

oneGramDf <- sanitizeGramDf(oneGramDf)
biGramDf <- sanitizeGramDf(biGramDf)
triGramDf <- sanitizeGramDf(triGramDf)
fourGramDf <- sanitizeGramDf(fourGramDf)


sortGramDf <- function(df) {
  df[order(df$Count, decreasing = TRUE), ]
}

oneGramDf <- sortGramDf(oneGramDf)
biGramDf <- sortGramDf(biGramDf)
triGramDf <- sortGramDf(triGramDf)
fourGramDf <- sortGramDf(fourGramDf)

reductionRows <- c(1: 30)
oneGramDfReduced <- oneGramDf[reductionRows, ]
biGramDfReduced <- biGramDf[reductionRows, ]
triGramDfReduced <- triGramDf[reductionRows, ]


plotNgram <- function(df, titleLabel, xLabel, yLabel) {
  plot1 <- ggplot(df, aes(x = reorder(Term, -Count), y = Count))
  plot1 <- plot1 + geom_bar(stat = "identity")
  plot1 <- plot1 + ggtitle(titleLabel)
  plot1 <- plot1 + labs(x = xLabel, y = yLabel)
  plot1 <- plot1 + theme(axis.text.x = element_text(angle = 45, size = 14, hjust = 1), 
                         plot.title = element_text(size = 20, face = "bold"))
  plot1
}


plotNgram(oneGramDfReduced, "Top 30 1-Gram", "1-Gram", "Count of 1-Gram")
plotNgram(biGramDfReduced, "Top 30 2-Grams", "2-Grams", "Count of 2-Grams")
plotNgram(triGramDfReduced, "Top 30 3-Grams", "3-Grams", "Count of 3-Grams")



getLastWord <- function (txt, seperator = " ") {
  txtElems <- strsplit(txt, seperator)[[1]]
  txtElems[length(txtElems)]
}

getLastWords <- function(txts) {
  numOfTxt <- length(txts)
  lastWords <- vector(length = numOfTxt)
  for(i in c(1:numOfTxt)) {
    lastWords[i] <- getLastWord(txts[i])
  }
  lastWords
}

getLastWord(biGramDfReduced)
