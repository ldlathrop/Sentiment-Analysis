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

#################################################
# SECTION: MOST FREQUENT WORDS
#################################################

# Create a most common words named vector for each candidate
clTop <- topfeatures(clinton.dfm, 100)
saTop <- topfeatures(sanders.dfm, 100)
trTop <- topfeatures(trump.dfm, 100)
head(clTop)

# Turn most common words into datasets for each candidate
# Then combine all datasets into one
clTop <- data.frame(as.list(clTop))
names(clTop)
clTop <- gather(clTop, Words, Frequency, israel:agenda)
clTop.df <- arrange(clTop, Words)
clTop.m <- as.matrix(arrange(clTop, Words))
write.csv(clTop, file = "cltop.csv")
head(read.csv("cltop.csv"), 10)

saTop <- data.frame(as.list(saTop))
names(saTop)
saTop <- gather(saTop, Words, Frequency, wall:wealthy)
saTop <- arrange(saTop, Words)
write.csv(saTop, file="satop.csv")

trTop <- data.frame(as.list(trTop))
names(trTop)
trTop <- gather(trTop, Words, Frequency, israel:assets)
trTop <- gather(trTop, Trump.Words, Trump.Frequency, people:weapons)
trTop <- arrange(trTop, Words)
write.csv(trTop, file = "trtop.csv")



# Bind the datasets together
allTop <- cbind(clTop, saTop, trTop)

# Create graphic table using the 'formattable' package
formattable(allTop, list(
  Clinton.Frequency = color_bar("coral", 0.2),
  Sanders.Frequency = color_bar("aquamarine", 0.2),
  Trump.Frequency = color_bar("deeppink", 0.2)
))




