options(java.parameters = "-Xmx8192m" )
options(mc.cores = 3)
library(ggplot2);
library(slam)

filePathSep <- "\\"
fileNameSep <- "."
swiftKeyDirectory <- "C:/Karthik/SData_Science/Data_Prediction_Capstone/SCapstone"
finalDirectory <- paste(swiftKeyDirectory, "final", sep = filePathSep)
outputDirectory <- paste(swiftKeyDirectory, "output", sep = filePathSep) 
localesAvail <- c("de_DE", "en_US", "fi_FI", "ru_RU")
locales <- localesAvail[2]
contexts <- c("blogs", "news", "twitter")
fileExt <- "txt"
set.seed(55669)

source("./Week3_Sample_Data.R")
makeSampleFiles(0.01) # 50%

source("./Week3-Construct_Corpus.R")
enUsOutputDirectory <- paste(outputDirectory, locales, sep = filePathSep)
ovid <- makeCorpus(enUsOutputDirectory)
ovid <- transformCorpus(ovid)
# ovid <- tagDocumentWithId(ovid)
save(ovid, file="corpus.RData")
rm(ovid)

source("./Week3-nGramMaker.R")
load("corpus.RData")
ngrams <- makeNGrams(ovid)
oneGram <- ngrams[[1]]; biGram <- ngrams[[2]]; triGram <- ngrams[[3]];
# fourGram <- ngrams[[4]]; fiveGram <- ngrams[[5]]
rm(ngrams); gc()
save(oneGram, file = "oneGram.RData")
save(biGram, file = "biGram.RData")
save(triGram, file = "triGram.RData")
# save(fourGram, file = "fourGram.RData")
# save(fiveGram, file = "fiveGram.RData")
rm(oneGram); rm(biGram); rm(triGram); 
# rm(fourGram); rm(fiveGram); 
gc()


tagDocumentWithId <- function(corpus) {
  for(i in c(1 : length(corpus))) {
    DublinCore(corpus[[i]], "id") <- i
  }
  corpus
}

ovid <- tagDocumentWithId(corpus)
head(corpus)



makeCorpus1 <- function(d) {
  dirSource <- DirSource(directory = d, encoding = "UTF-8")
  ovid <- VCorpus(dirSource, readerControl = list(language = "eng"))
  on.exit(close(dirSource))
  ovid
}

ovid <- makeCorpus1(enUsOutputDirectory)



