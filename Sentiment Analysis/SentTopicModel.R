#############################################
# SECTION: TOPIC MODELING
#############################################

# Convert dfms for use with 'tm' package
clinton.tm <- convert(clinton.dfm, to="tm")
sanders.tm <- convert(sanders.dfm, to="tm")
trump.tm <- convert(trump.dfm, to="tm")


# Now create subset based on tweets with certain words, such as the high frequency words identified in the text mining. eg. science
clPeople <- subset(clinton.scores, regexpr("people", clinton.scores$text) > 0)   # extract tweets containing only 'scien'
# plot histogram for this token, 
ggplot(clPeople, aes(x = score)) + geom_histogram(binwidth = 1) + xlab("Sentiment score for the token 'scien'") + ylab("Frequency") + theme_bw()  + element_text(axis.title.x = theme_text(vjust = -0.5, size = 14)) + element_text(axis.title.y = theme_text(size = 14, angle = 90, vjust = -0.25))
# repeat this block with different high frequency words

cl.tm.sparse <- removeSparseTerms(clinton.tm, sparse=0.95)  # I found I had to iterate over this to ensure the tdm doesn't get too small... for example: 0.990 nrow=88, 0.989, nrow=67, 0.985, nrow=37, 0.98 nrow=23, 0.95 nrow=6
cl.tdm.sp.df <- as.data.frame(inspect(cl.tm.sparse)) # convert document term matrix to data frame
nrow(cl.tdm.sp.df) # check to see how many words we're left with after removing sparse terms
# this analysis is based on http://www.statmethods.net/advstats/cluster.html 
# scale and transpose data for cluster analysis
cl.tdm.sp.df.sc.t <- t(scale(cl.tdm.sp.df))
require(pvclust)
fit <- pvclust(cl.tdm.sp.df.sc.t, method.hclust = "average", method.dist = "correlation", nboot = 10) # this method may take a few hours the bootstraping, you can reduce the nboot value for a quicker result
plot(fit, cex = 1.5, cex.pv = 1.2, col.pv = c(1,0,0), main="", xlab="", sub="")  # draw the dendrogram

require(slam)
a.tdm.sp.t <- t(a.tdm.sp) # transpose document term matrix, necessary for the next steps using mean term frequency-inverse document frequency (tf-idf) to select the vocabulary for topic modeling
summary(col_sums(a.tdm.sp.t)) # check median...
term_tfidf <- tapply(a.tdm.sp.t$v/row_sums(a.tdm.sp.t)[a.tdm.sp.t$i], a.tdm.sp.t$j,mean) * log2(nDocs(a.tdm.sp.t)/col_sums(a.tdm.sp.t>0)) # calculate tf-idf values
summary(term_tfidf) # check median... note value for next line... 
a.tdm.sp.t.tdif <- a.tdm.sp.t[,term_tfidf>=1.0] # keep only those terms that are slightly less frequent that the median
a.tdm.sp.t.tdif <- a.tdm.sp.t[row_sums(a.tdm.sp.t) > 0, ]
summary(col_sums(a.tdm.sp.t.tdif)) # have a look

# Before going right into generating the topic model and analysing the output, we need to decide on the number of topics that the model should use
# Here's a function to loop over different topic numbers, get the log liklihood of the model for each topic number and plot it so we can pick the best one
# The best number of topics is the one with the highest log liklihood value.

require(topicmodels)
cl.best.model <- lapply(seq(2, 50, by = 1), function(d){LDA(cl.tm.sparse, d)}) # this will make a topic model for every number of topics between 2 and 50... it will take some time! 
cl.best.model.logLik <- as.data.frame(as.matrix(lapply(cl.best.model, logLik)))  # this will produce a list of logLiks for each model... 

# plot the distribution of logliklihoods by topic
cl.best.model.logLik.df <- data.frame(topics=c(2:50), LL = as.numeric(as.matrix(cl.best.model.logLik)))
ggplot(cl.best.model.logLik.df, aes(x = topics, y = LL)) + 
  xlab("Number of topics") + 
  ylab("Log likelihood of the model") + 
  geom_line() + 
  theme_bw()  + 
  opts(axis.title.x = theme_text(vjust = -0.5, size = 14)) + 
  opts(axis.title.y=theme_text(size = 14, angle=90, vjust= -0.25)) + 
  opts(plot.margin = unit(c(1,1,2,2), "lines"))  

# ggsave(file = "model_LL_per_topic_number.pdf") # export the plot to a PDF file
# it's not easy to see exactly which topic number has the highest LL, so let's look at the data...
cl.best.model.logLik.df.sort <- cl.best.model.logLik.df[order(-cl.best.model.logLik.df$LL), ] # sort to find out which number of topics has the highest loglik, in this case 23 topics. 
cl.best.model.logLik.df.sort # have a look to see what's at the top of the list, the one with the highest score
ntop <- cl.best.model.logLik.df.sort[1,]$topics


lda <- LDA(a.tdm.sp.t.tdif, ntop) # generate a LDA model the optimum number of topics
get_terms(lda, 5) # get keywords for each topic, just for a quick look
get_topics(lda, 5) # gets topic numbers per document
lda_topics<-get_topics(lda, 5) 
beta <- lda@beta # create object containing parameters of the word distribution for each topic
gamma <- lda@gamma # create object containing posterior topic distribution for each document
terms <- lda@terms # create object containing terms (words) that can be used to line up with beta and gamma
colnames(beta) <- terms # puts the terms (or words) as the column names for the topic weights.
id <- t(apply(beta, 1, order)) # order the beta values
beta_ranked <- lapply(1:nrow(id),function(i)beta[i,id[i,]])  # gives table of words per topic with words ranked in order of beta values. Useful for determining the most important words per topic


# Other approach to topic modeling
# Remove sparse terms
cl.tm.sparse <- removeSparseTerms(clinton.tm, sparse=0.65)  # I found I had to iterate over this to ensure the tdm doesn't get too small... for example: 0.990 nrow=88, 0.989, nrow=67, 0.985, nrow=37, 0.98 nrow=23, 0.95 nrow=6
sa.tm.sparse <- removeSparseTerms(sanders.tm, sparse = 0.65)
tr.tm.sparse <- removeSparseTerms(trump.tm, sparse = 0.65)

# Stem the words
cl.tm.sparse.st <- wordStem(cl.tm.sparse$dimnames$Terms, language = character(), warnTested = FALSE)
sa.tm.sparse.st <- wordStem(sa.tm.sparse$dimnames$Terms, language = character(), warnTested = FALSE)
tr.tm.sparse.st <- wordStem(tr.tm.sparse$dimnames$Terms, language = character(), warnTested = FALSE)

cl.tm.sparse.t <- t(cl.tm.sparse) # transpose document term matrix, necessary for the next steps using mean term frequency-inverse document frequency (tf-idf) to select the vocabulary for topic modeling
summary(col_sums(cl.tm.sparse.t)) # check median...
cl_tfidf <- tapply(cl.tm.sparse.t$v/row_sums(cl.tm.sparse.t)[cl.tm.sparse.t$i], cl.tm.sparse.t$j,mean) * log2(nDocs(cl.tm.sparse.t)/col_sums(cl.tm.sparse.t>0)) # calculate tf-idf values
summary(cl_tfidf) # check median... note value for next line... 
cl.tdm.sp.t.tdif <- cl.tm.sparse.t[,cl_tfidf>=-2.943] # keep only those terms that are slightly less frequent that the median
cl.tdm.sp.t.tdif <- cl.tm.sparse.t[row_sums(cl.tm.sparse.t) > -3, ]
summary(col_sums(cl.tdm.sp.t.tdif)) # have a look




##################################################
# SECTION: ANOTHER APPROACH TO TOPIC MODELING
###################################################

# Convert dfms for use with topicmodels package
clinton.topic <- convert(clinton.dfm, to="topicmodels")
sanders.topic <- convert(sanders.dfm, to="topicmodels")
trump.topic <- convert(trump.dfm, to="topicmodels")

#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 12

# CLINTON TOPIC MODEL
#Run LDA using Gibbs sampling
lda.cl <-LDA(clinton.topic,k, method='Gibbs', 
             control=list(nstart=nstart, 
                          seed = seed, best=best, 
                          burnin = burnin, iter = iter, 
                          thin=thin))

#write out results
#docs to topics
ldaCL.topics <- as.matrix(topics(lda.cl))
write.csv(ldaCL.topics,file=paste('LDAGibbs',k,'ClDocsToTopics.csv'))

#top terms in each topic
ldaCl.terms <- as.matrix(terms(lda.cl,20))
write.csv(ldaCl.terms,file=paste('LDAGibbs',k,'ClTopicsToTerms.csv'))

#probabilities associated with each topic assignment
ClTopicProbabilities <- as.data.frame(lda.cl@gamma)
write.csv(ClTopicProbabilities,file=paste('LDAGibbs',k,'ClTopicProbabilities.csv'))

#Find relative importance of top 2 topics
ClTopic1ToTopic2  <- lapply(1:nrow(clinton.topic),function(x)
  sort(ClTopicProbabilities[x,])[k]/sort(ClTopicProbabilities[x,])[k-1])

#Find relative importance of second and third most important topics
ClTopic2ToTopic3 <- lapply(1:nrow(clinton.topic),function(x)
  sort(ClTopicProbabilities[x,])[k-1]/sort(ClTopicProbabilities[x,])[k-2])

#write to file
write.csv(ClTopic1ToTopic2,file=paste('LDAGibbs',k,'ClTopic1ToTopic2.csv'))
write.csv(ClTopic2ToTopic3,file=paste('LDAGibbs',k,'ClTopic2ToTopic3.csv'))

get_terms(lda.cl, 5) # get keywords for each topic, just for a quick look
get_topics(lda.cl, 5) # gets topic numbers per document
lda_topics<-get_topics(lda.cl, 5) 
cl.beta <- lda.cl@beta # create object containing parameters of the word distribution for each topic
cl.gamma <- lda.cl@gamma # create object containing posterior topic distribution for each document
cl.terms <- lda.cl@terms # create object containing terms (words) that can be used to line up with beta and gamma
colnames(cl.beta) <- cl.terms # puts the terms (or words) as the column names for the topic weights.
cl.id <- t(apply(cl.beta, 1, order)) # order the beta values
cl.beta_ranked <- lapply(1:nrow(cl.id),function(i)cl.beta[i,cl.id[i,]])  # gives table of words per topic with words ranked in order of beta values. Useful for determining the most important words per topic
head(cl.beta_ranked)

clTopicTerms <- data.frame(as.list(cl.beta_ranked))
names(clTopicTerms)
clTopicTerms <- gather(clTopicTerms, Words, Frequency, wall:wealthy)
saTop <- arrange(saTop, Words)
write.csv(saTop, file="satop.csv")

# SANDERS TOPIC MODEL
k=9
#Run LDA using Gibbs sampling
lda.sa <-LDA(sanders.topic,k, method='Gibbs', control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

#write out results
#docs to topics
ldaSA.topics <- as.matrix(topics(lda.sa))
write.csv(ldaSA.topics,file=paste('LDAGibbs',k,'SaDocsToTopics.csv'))

#top terms in each topic
ldaSa.terms <- as.matrix(terms(lda.sa,20))
write.csv(ldaSa.terms,file=paste('LDAGibbs',k,'SaTopicsToTerms.csv'))

#probabilities associated with each topic assignment
SaTopicProbabilities <- as.data.frame(lda.sa@gamma)
write.csv(SaTopicProbabilities,file=paste('LDAGibbs',k,'SaTopicProbabilities.csv'))

#Find relative importance of top 2 topics
SaTopic1ToTopic2 <- lapply(1:nrow(sanders.topic),function(x)
  sort(SaTopicProbabilities[x,])[k]/sort(SaTopicProbabilities[x,])[k-1])

#Find relative importance of second and third most important topics
SaTopic2ToTopic3 <- lapply(1:nrow(sanders.topic),function(x)
  sort(SaTopicProbabilities[x,])[k-1]/sort(SaTopicProbabilities[x,])[k-2])

#write to file
write.csv(SaTopic1ToTopic2,file=paste('LDAGibbs',k,'SaTopic1ToTopic2.csv'))
write.csv(SaTopic2ToTopic3,file=paste('LDAGibbs',k,'SaTopic2ToTopic3.csv'))

get_terms(lda.sa, 5) # get keywords for each topic, just for a quick look
get_topics(lda.sa, 5) # gets topic numbers per document
sa.lda_topics<-get_topics(lda.sa, 5) 
sa.beta <- lda.sa@beta # create object containing parameters of the word distribution for each topic
sa.gamma <- lda.sa@gamma # create object containing posterior topic distribution for each document
sa.terms <- lda.sa@terms # create object containing terms (words) that can be used to line up with beta and gamma
colnames(sa.beta) <- sa.terms # puts the terms (or words) as the column names for the topic weights.
sa.id <- t(apply(sa.beta, 1, order)) # order the beta values
sa.beta_ranked <- lapply(1:nrow(sa.id),function(i)sa.beta[i,sa.id[i,]])  # gives table of words per topic with words ranked in order of beta values. Useful for determining the most important words per topic
head(sa.beta_ranked)


# TRUMP TOPIC MODEL
k=9
#Run LDA using Gibbs sampling
lda.tr <-LDA(trump.topic,k, method='Gibbs', control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

#write out results
#docs to topics
ldaTR.topics <- as.matrix(topics(lda.tr))
write.csv(ldaTR.topics,file=paste('LDAGibbs',k,'trDocsToTopics.csv'))

#top terms in each topic
ldaTr.terms <- as.matrix(terms(lda.tr,20))
write.csv(ldaTr.terms,file=paste('LDAGibbs',k,'trTopicsToTerms.csv'))

#probabilities associated with each topic assignment
TrTopicProbabilities <- as.data.frame(lda.tr@gamma)
write.csv(TrTopicProbabilities,file=paste('LDAGibbs',k,'trTopicProbabilities.csv'))

#Find relative importance of top 2 topics
TrTopic1ToTopic2 <- lapply(1:nrow(trump.topic),function(x)
  sort(TrTopicProbabilities[x,])[k]/sort(TrTopicProbabilities[x,])[k-1])

#Find relative importance of second and third most important topics
TrTopic2ToTopic3 <- lapply(1:nrow(trump.topic),function(x)
  sort(TrTopicProbabilities[x,])[k-1]/sort(TrTopicProbabilities[x,])[k-2])

#write to file
write.csv(ClTopic1ToTopic2,file=paste('LDAGibbs',k,'TrTopic1ToTopic2.csv'))
write.csv(ClTopic2ToTopic3,file=paste('LDAGibbs',k,'TrTopic2ToTopic3.csv'))

get_terms(lda.tr, 5) # get keywords for each topic, just for a quick look
get_topics(lda.tr, 5) # gets topic numbers per document
tr.lda_topics<-get_topics(lda.tr, 5) 
tr.beta <- lda.tr@beta # create object containing parameters of the word distribution for each topic
tr.gamma <- lda.tr@gamma # create object containing posterior topic distribution for each document
tr.terms <- lda.tr@terms # create object containing terms (words) that can be used to line up with beta and gamma
colnames(tr.beta) <- tr.terms # puts the terms (or words) as the column names for the topic weights.
tr.id <- t(apply(tr.beta, 1, order)) # order the beta values
tr.beta_ranked <- lapply(1:nrow(tr.id),function(i)tr.beta[i,tr.id[i,]])  # gives table of words per topic with words ranked in order of beta values. Useful for determining the most important words per topic
head(tr.beta_ranked, 10)
