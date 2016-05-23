library(tm)
library(RColorBrewer)
library(broom)
library(formattable)
library(ggplot2)
library(ggthemes)
library(ggvis)
library(kernlab)
library(plotly)
library(plyr)
library(dplyr)
library(qdap)
library(RWeka)
library(wordcloud)
library(rJava)
library(openNLP)
library(topicmodels)
library(SnowballC)
library(quanteda)
library(wordnet)
library(tidyr)
library(gridExtra)
library(slam)
library(RTextTools)
library(Hmisc)
library(reshape)
library(gplots)
library(lattice)
library(plotrix)
library(GGally)
library(RWeka)
library(Matrix)
library(qlcMatrix)
library(svs)
library(Rstem)
library(sentiment)

require(devtools)
install_url("http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1/tar/gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
install_url("http://cran.r-project.org/src/contrib/languageR_1.4.1..tar.gz")


########################################################
# SECTION: READ IN FILES AND CREATE WORD/SENTECE CORPUSES
#########################################################

# Tokenize by word and by sentence.
clintonCorpus <- textfile("Clinton/*.txt", docvarsfrom = "filenames", docvarnames = c("Candidate", "Month", "Day", "Year"))
clinton <- corpus(texts(clintonCorpus))
clinton.words <- tokenize(toLower(clinton), what = "word", removePunct = TRUE)
clinton.sent <- tokenize(toLower(clinton), what = "sentence")
clinton.sent <- gsub("'", "", clinton.sent)

sandersCorpus <- textfile("Sanders/*.txt", docvarsfrom = "filenames", docvarnames = c("Candidate", "Month", "Day", "Year"), cache = FALSE)
sanders <- corpus(texts(sandersCorpus))
sanders.words <- tokenize(toLower(sanders), what="word", removePunct = TRUE)
sanders.sent <- tokenize(toLower(sanders), what="sentence")

trumpCorpus <- textfile("Trump/*.txt", docvarsfrom = "filenames", docvarnames = c("Candidate", "Month", "Day", "Year"), cache = FALSE)
trump <- corpus(texts(trumpCorpus))
trump.words <- tokenize(toLower(trump), what="word", removePunct = TRUE)
trump.sent <- tokenize(toLower(trump), what="sentence")

# Read in stopwords files
myDict <- scan(file = "stopwords.txt", what="character", sep = ",", strip.white=TRUE)
myStop <- scan(file = "myStopWords.csv", what = "character", sep = ",", strip.white = TRUE)
# Create document frequency matrix for each candidate
clinton.dfm <- dfm(clinton.words, ignoredFeatures = c(stopwords("english"), myDict, myStop, "york", "steve", "senator", "thank"))

sanders.dfm <- dfm(sanders.words, ignoredFeatures = c(stopwords("english"), myDict, myStop, "X1", "X5"))
topfeatures(sanders.dfm, 30)

trump.dfm <- dfm(trump.words, ignoredFeatures = c(stopwords("english"), myDict, myStop, "mr"))
topfeatures(trump.dfm, 30)

# Create word clouds for democrats and republicans
pal <- brewer.pal(8, "Set1")
png("dem.png", width = 640, height = 480)
par(mfrow=c(1,2), mar=c(4,4,2,1), oma=c(0,0,1,0))
plot(clinton.dfm, max.words =30, colors = pal, bty="o", scale = c(4, .5),
     xlab="Hillary Clinton's Speeches")
plot(sanders.dfm, max.words = 30, colors = pal, bty="o", scale = c(4, .5),
     xlab="Bernie Sanders' Speeches")
mtext("Democratic Candidates", outer=TRUE)
dev.off()

png("rep.png", width = 640, height = 480)
par(mfrow=c(1,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
plot(kasich.dfm, max.words = 50, colors = pal, bty="o", scale = c(6, .3),
     main="John Kasich's Speeches")
plot(trump.dfm, max.words = 50, colors = pal, bty="o", scale = c(6, .3),
     main="Donald Trump's Speeches")
mtext("Republican Candidates", outer = TRUE)
dev.off()

