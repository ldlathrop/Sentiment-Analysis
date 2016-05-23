##################################################
# SECTION: SENTIMENT ANALYSIS - DICTIONARY-BASED
##################################################

# This is based on Jeffrey Breen's excellent tutorial at http://jeffreybreen.wordpress.com/2011/07/04/twitter-text-mining-r-slides/
hu.liu.pos = scan('opinion-lexicon-English/positive.txt', what = 'character',comment.char=';') #load +ve sentiment word list
hu.liu.neg = scan('opinion-lexicon-English/negative.txt',what = 'character',comment.char= ';') #load -ve sentiment word list
pos.words = c(hu.liu.pos)
neg.words = c(hu.liu.neg)

# Function to score sentiment of each sentence
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}
require(plyr)
clinton.list <- as.list(c(clinton.sent$Clinton_03_21_16.txt, clinton.sent$Clinton_04_12_16.txt, clinton.sent$Clinton_04_26_16.txt, clinton.sent$Clinton_04_27_16.txt))
sanders.list <- as.list(c(sanders.sent$Sanders_10_24_15.txt, sanders.sent$Sanders_11_19_15.txt, sanders.sent$Sanders_01_05_16.txt))
trump.list <- as.list(c(trump.sent$Trump_06_16_15.txt, trump.sent$Trump_03_21_16.txt))

clinton.scores <- score.sentiment(clinton.list,pos.words,neg.words,.progress='text') # get scores for the text 
sanders.scores <- score.sentiment(sanders.list,pos.words,neg.words,.progress='text')
trump.scores <- score.sentiment(trump.list,pos.words,neg.words,.progress='text')

# Extract highly positive/highly negative sentences
cl.pos.sent <- subset(clinton.scores, clinton.scores$score >= 2) # get sentences with only very +ve scores
cl.neg.sent <- subset(clinton.scores, clinton.scores$score <= -2) # get sentences with only very -ve scores

sa.pos.sent <- subset(sanders.scores, sanders.scores$score >= 2) # get sentences with only very +ve scores
sa.neg.sent <- subset(sanders.scores, sanders.scores$score <= -2) # get sentences with only very -ve scores

tr.pos.sent <- subset(trump.scores, trump.scores$score >= 2) # get sentences with only very +ve scores
tr.neg.sent <- subset(trump.scores, trump.scores$score <= -2) # get sentences with only very -ve scores

# Function to calculate each score's percentage of the total sentences
options(digits = 2)
scoreFun <- function(x){
  tbl <- data.frame(table(x))
  colnames(tbl) <- c("Score", "Count")
  tbl$Percentage <- tbl$Count / sum(tbl$Count) * 100
  round(tbl$Percentage, 2)
  return(tbl)
}


# Plotly drawing of relative scores between candidates
clPct <- scoreFun(clinton.scores$score)
clPct$hovertext <- paste("Percentage of total",
                         round(clPct$Percentage,2), sep = "<br>")
clPct

saPct <- scoreFun(sanders.scores$score)
saPct$hovertext <- paste("Percentage of total",
                         round(saPct$Percentage,2), sep = "<br>")
saPct

trPct <- scoreFun(trump.scores$score)
trPct$hovertext <- paste("Percentage of total",
                         round(trPct$Percentage,2), sep = "<br>")
trPct

# Define xaxis and yaxis and create plot
d <- subplot(
  plot_ly(clPct, x = Score, y=Percentage, xaxis="x1", yaxis="y1",
          text = hovertext, hoverinfo = 'text', mode = "line"),
  plot_ly(saPct, x = Score, y=Percentage, xaxis="x2", yaxis="y2",
          text = hovertext, hoverinfo = 'text', mode = "line"),
  plot_ly(trPct, x = Score, y=Percentage, xaxis="x3", yaxis="y3",
          text = hovertext, hoverinfo = 'text', mode = "line"),
  margin = 0.05,
  nrows=3
) %>% layout(d, title="Negative & Positive Statements",
             xaxis=list(title="", range=c(-15, 15)),
             xaxis2=list(title="", range=c(-15,15)),
             xaxis3=list(title="Score", range=c(-15,15)),
             yaxis=list(title="Clinton", range=c(0,50)),
             yaxis2=list(title="Sanders", range=c(0,50)),
             yaxis3=list(title="Trump", range=c(0,50)),
             showlegend = FALSE)
d

# Extract sentences that are highly positive/highly negative
clPct$Score <- as.numeric(levels(clPct$Score)[clPct$Score])
cl.pos <- subset(clPct, clPct$Score >= 2) # get sentences with only very +ve scores
cl.neg <- subset(clPct, clPct$Score <= -2) # get sentences with only very -ve scores

saPct$Score <- as.numeric(levels(saPct$Score)[saPct$Score])
sa.pos <- subset(saPct, saPct$Score >= 2) # get sentences with only very +ve scores
sa.neg <- subset(saPct, saPct$Score <= -2) # get sentences with only very -ve scores

trPct$Score <- as.numeric(levels(trPct$Score)[trPct$Score])
tr.pos <- subset(trPct, trPct$Score >= 2) # get sentences with only very +ve scores
tr.neg <- subset(trPct, trPct$Score <= -2) # get sentences with only very -ve scores

# Print percentage totals for each candidate
sum(cl.pos$Percentage)
sum(cl.neg$Percentage)
sum(sa.pos$Percentage)
sum(sa.neg$Percentage)
sum(tr.pos$Percentage)
sum(tr.neg$Percentage)

# Function to find actual pos & neg phrases of each sentence
hu.liu.pos = scan('opinion-lexicon-English/positive.txt', what = 'character',comment.char=';') #load +ve sentiment word list
hu.liu.neg = scan('opinion-lexicon-English/negative.txt',what = 'character',comment.char= ';') #load -ve sentiment word list
pos.words = c(hu.liu.pos)
neg.words = c(hu.liu.neg)

words <- matrix(ncol = 2,nrow=8)

words = (str_split(unlist(sent$Sentences)," "))

tmp <- data.frame()
tmn <- data.frame()

for (i in 1:nrow(sent)) {
  for (j in 1:length(words)) {
    for (k in 1:length(pos.words)){
      if (words[[i]][j] == pos.words[k]) {
        print(paste(i,words[[i]][j],1))
        tmn <- cbind(i,words[[i]][j],1)
        tmp <- rbind(tmp,tmn)
      }
    }
    for (m in 1:length(neg.words)){
      if (words[[i]][j] == neg.words[m]) { 
        print(paste(i,words[[i]][j],-1))
        tmn <- cbind(i,words[[i]][j],-1)
        tmp <- rbind(tmp,tmn)
      }
    }  
  }
}

View(tmp)

##########################################################
# SECTION: SENTIMENT ANALYSIS - SYNTACTIC METHOD

##########################################################
### --- Part-of-Speech tagging and syntactic parsing with R
##########################################################
### --- R script "Part-of-Speech tagging and syntactic parsing with R"
### --- Author: Martin Schweinberger
### --- This script aims at an automated approach to POS tagging
### --- a sample corpus.
##########################################################
### --- Prepare data
# Remove all lists from the current workspace
rm(list=ls(all=T))
# Install packages we need or which may be useful
# (to activate just delete the #)
#install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
#install.packages("openNLP")
#install.packages("NLP")
### additional packages
#install.packages("tm")
#install.packages("stringr")
#install.packages("gsubfn")
#install.packages("plyr")
# load packages
library(NLP)
library(openNLP)
library(openNLPmodels.en)
### load additional packages
library(tm)
library(stringr)
library(gsubfn)
library(plyr)
# to install openNLPmodels, please download an install the packages/models direktly from
# http://datacube.wu.ac.at/. To install these packages/models, simply enter
#install.packages("foo", repos = "http://datacube.wu.ac.at/", type = "source")
# into your R console. E.g. enter:
#install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
# to install the file "openNLPmodels.en_1.5-1.tar.gz"
##########################################################
# specify path of corpus
#pathname <- "./Clinton"
###############################################################
###                   START
###############################################################
# Prepare for loading corpus
# Choose the files you would like to use
#corpus.files = list.files(path = pathname, pattern = NULL, all.files = T,
#                          full.names = T, recursive = T, ignore.case = T, include.dirs = T)
###############################################################
# Load and unlist corpus
#corpus.tmp <- lapply(clinton.sent, function(x) {
#  scan(x, what = "char", sep = "\t", quiet = T) }  )
# Paste all elements of the corpus together
#corpus.tmp <- lapply(clinton.sent, function(x){
#  x <- paste(x, collapse = " ")  }  )
# Clean corpus
#corpus.tmp <- lapply(clinton.sent, function(x) {
#  x <- enc2utf8(x)  }  )
#corpus.tmp <- gsub(" {2,}", " ", corpus.tmp)
#corpus.tmp <- str_trim(corpus.tmp, side = "both") # remove spaces at beginning and end of strings
# inspect result
#str(corpus.tmp)
#>chr [1:3] "This is the first sentence in the first file of the test corpus. This is a second sentence in the test corpus but I am too lazy"| __truncated__ ...


# convert corpus files into strings
#Corpus <- lapply(corpus.tmp, function(x){
#  x <- as.String(x)  }  )
###############################################################
# Start actual PoS-tagging
# apply annotators to Corpus
# Corpus.tagged <- lapply(Corpus, function(x){
#  sent_token_annotator <- Maxent_Sent_Token_Annotator()
#  word_token_annotator <- Maxent_Word_Token_Annotator()
#  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
#  y1 <- NLP::annotate(x, list(sent_token_annotator, word_token_annotator))
#  y2 <- NLP::annotate(x, pos_tag_annotator, y1)
#  y3 <- annotate(x, Maxent_POS_Tag_Annotator(probs = TRUE), y1)
#  y2w <- subset(y2, type == "word")
#  tags <- sapply(y2w$features, '[[', "POS")
#  r1 <- sprintf("%s/%s", x[y2w], tags)
#  r2 <- paste(r1, collapse = " ")
#  return(r2)  }  )

# inspect results
#Corpus.tagged

###############################################################
### --- a function which POS tags corpus files
###############################################################
POStag <- function(file = file){
  require("NLP")
  require("openNLP")
  require("openNLPmodels.en")
  corpus.tmp <- lapply(file, function(x){
    x <- paste(x, collapse = " ")  }  )
  corpus.tmp <- lapply(corpus.tmp, function(x) {
    x <- enc2utf8(x)  }  )
  corpus.tmp <- gsub(" {2,}", " ", corpus.tmp)
  corpus.tmp <- str_trim(corpus.tmp, side = "both") 
  Corpus <- lapply(corpus.tmp, function(x){
    x <- as.String(x)  }  )
  sent_token_annotator <- openNLP::Maxent_Sent_Token_Annotator()
  word_token_annotator <- openNLP::Maxent_Word_Token_Annotator()
  pos_tag_annotator <- openNLP::Maxent_POS_Tag_Annotator() 
  lapply(Corpus, function(x){
    y1 <- NLP::annotate(x, list(sent_token_annotator, word_token_annotator))
    y2<- NLP::annotate(x, pos_tag_annotator, y1)
    y2w <- subset(y2, type == "word")
    tags <- sapply(y2w$features, '[[', "POS")
    r1 <- sprintf("%s/%s", x[y2w], tags)
    r2 <- paste(r1, collapse = " ")
    return(r2)  }  )
}

# test the function
library(stringr)
clinton.POS <- POStag(file = clinton.sent)
sanders.POS <- POStag(file = sanders.sent)
trump.POS <- POStag(file = trump.sent)


J <- "JJ"
N <- "N[A-Z]"
R <- "R[A-Z]"
V <- "V[A-Z]"

cl.POS.un <- unlist(clinton.POS)
cl.POS.df <- read.table(textConnection(gsub(" ", " \n ", cl.POS.un)), sep="/", quote = "", comment = '', encoding="latin1", fill = TRUE, stringsAsFactors=FALSE)

sa.POS.un <- unlist(sanders.POS)
sa.POS.df <- read.table(textConnection(gsub(" ", "\n", sa.POS.un)), sep="/", quote = "", comment = '', encoding="latin1", fill = TRUE, stringsAsFactors=FALSE)
head(sa.df, 20)
tr.POS.un <- unlist(trump.POS)
tr.POS.df <- read.table(textConnection(gsub(" ", "\n", tr.POS.un)), sep="/", quote = "", comment = '', encoding="latin1", fill = TRUE, stringsAsFactors=FALSE)
tail(tr.POS.df, 20)

cl.POS.df$adjnn <- grepl("J|N", cl.POS.df$V2)
summary(cl.POS.df$adjnn)
cl.POS.df$adj.nn.pair <- c(NA, cl.POS.df$adjnn[1:(nrow(cl.POS.df)-1)] & cl.POS.df$adjnn[2:nrow(cl.POS.df)])
cl.POS.df$advadj <- grepl("R|J|!N", cl.POS.df$V2)
summary(cl.POS.df$advadj)
cl.POS.df$adv.adj.pair <- c(NA, cl.POS.df$advadj[1:(nrow(cl.POS.df)-1)] & cl.POS.df$advadj[2:nrow(cl.POS.df)])
cl.POS.df$adjadj <- grepl("J|J|!N", cl.POS.df$V2)
cl.POS.df$adj.adj.pair <- c(NA, cl.POS.df$adjadj[1:(nrow(cl.POS.df)-1)] & cl.POS.df$adjadj[2:nrow(cl.POS.df)])
cl.POS.df$nnadj <- grepl("N|J|!N", cl.POS.df$V2)
cl.POS.df$nn.adj.pair <- c(NA, cl.POS.df$nnadj[1:(nrow(cl.POS.df)-1)] & cl.POS.df$nnadj[2:nrow(cl.POS.df)])
cl.POS.df$advvb <- grepl("R|V", cl.POS.df$V2)
cl.POS.df$adv.vb.pair <- c(NA, cl.POS.df$advvb[1:(nrow(cl.POS.df)-1)] & cl.POS.df$advvb[2:nrow(cl.POS.df)])

cl.JN <- grep("J|N", cl.POS.df$V2, perl = TRUE, value = TRUE)
pairs <- function(x, y) {
  
  ngram <- c(x[i], x[i-1])   # the ngram consists of the word on line `i` and the word above line `i`
  for(i in 1:(nrow(y)-1)) {
    grep("TRUE", y, perl = TRUE, value = FALSE) #if 'y' == TRUE
  }
  return(paste(ngram, sep = " ", collapse = FALSE)) #return the ngram as one item
}

cl.JN <- pairs(x=cl.POS.df$V1, y=cl.POS.df$adj.nn.pair)

# Function from SO
pairs <- function(x) {
  
  J <- "JJ"      #adjectives
  N <- "N[A-Z]"   #any noun form
  R <- "R[A-Z]"   #any adverb form
  V <- "V[A-Z]"   #any verb form
  
  ngram = c(x[i,1], x[i+1,1])
  pair = rep("FALSE", length(x))
  for(i in 1:(nrow(x)-2)) {
    this.pos = x[i,2]
    next.pos = x[i+1,2]
    next.next.pos = x[i+2,2]
    if(this.pos == J && next.pos == N) {    #i.e., if the first word = J and the next = N
      pair[i] <- "JJ|NN"     #insert this into the 'pair' variable
    } else if (this.pos == R && next.pos == J && next.next.pos != N) {
      pair[i] <- "RB|JJ"
    } else if  (this.pos == J && next.pos == J && next.next.pos != N) {
      pair[i] <- "JJ|JJ"
    } else if (this.pos == N && next.pos == J && next.next.pos != N) {
      pair[i] <- "NN|JJ"
    } else if (this.pos == R && next.pos == V) {
      pair[i] <- "RB|VB"
    } else {
      pair[i] <- "FALSE"
    }
    
    return(as.list(c(ngrams=ngram, pairs=pair)))
  }
  
  data.frame(pair(x))
  ## then deal with the last two elements, for which you can't check what's up next
  
}

# trying something different
pairs <- function(x) {
  
  J <- "JJ"      #adjectives
  N <- "N[A-Z]"   #any noun form
  R <- "R[A-Z]"   #any adverb form
  V <- "V[A-Z]"   #any verb form
  
  ngram = c(x[i,1], x[i+1,1])
  pair = rep("FALSE", length(x))
  for(i in 1:(nrow(x)-2)) {
    j = x[i, 2]
    pair <- grep("J|N", j, perl = TRUE, value = TRUE)
    pair <- grep("R|J|!N", j, perl = TRUE, value = TRUE)
    pair <- grep("J|J|!N", j, perl = TRUE, value = TRUE)
    pair <- grep("N|J|!N", j, perl = TRUE, value = TRUE)
    pair <- grep("R|V", j, perl = TRUE, value = TRUE)
    
  }
  
  return(as.list(c(ngrams=ngram, pairs=pair)))
}


## then deal with the last two elements, for which you can't check what's up next


cl.pairs <- pairs(cl.POS.df)
sl.pairs <- pairs(sa.df)

cl.tag.Split = strsplit(cl.POS.un," ")
cl.tag.Split
cl.tag.df <- data.frame(t(strsplit(cl.tag.Split[[1]], "/")))
head(cl.tag.df, 20)

qq = 0
tag = 0

for (i in 1:length(cl.tag.Split[[1]])){
  qq[i] <-strsplit(cl.tag.Split[[1]][i],'/')
  tag[i] = qq[i][[1]][2]
}

index = 0

k = 0

cl.index <- for (i in 1:(length(cl.tag.Split[[1]])-1)) {
  
  if ((tag[i] == "JJ" && tag[i+1] == "NN") |
      (tag[i] == "JJ" && tag[i+1] == "NNS") |
      (tag[i] == "RB" && tag[i+1] == "JJ") |
      (tag[i] == "RBR"&& tag[i+1] == "JJ") |
      (tag[i] == "RBS" && tag[i+1] == "JJ") |
      (tag[i] == "JJ" && tag[i+1] == "JJ") | 
      (tag[i] == "NN" && tag[i+1] == "JJ") |
      (tag[i] == "NNS" && tag[i+1] == "JJ") |
      (tag[i] == "RB" && tag[i+1] == "VB") |
      (tag[i] == "RB" && tag[i+1] == "VBD") |
      (tag[i] == "RB" && tag[i+1] == "VBN") |
      (tag[i] == "RB" && tag[i+1] == "VBG") |
      (tag[i] == "RBR" && tag[i+1] == "VB") |
      (tag[i] == "RBR" && tag[i+1] == "VBD") |
      (tag[i] == "RBR" && tag[i+1] == "VBN") |
      (tag[i] == "RBR" && tag[i+1] == "VBG") |
      (tag[i] == "RBS" && tag[i+1] == "VB") |
      (tag[i] == "RBS" && tag[i+1] == "VBD") |
      (tag[i] == "RBS" && tag[i+1] == "VBN") |
      (tag[i] == "RBS" && tag[i+1] == "VBG") 
}  
k = k +1
index[k] = i



cl.index

