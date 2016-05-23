
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



#################################################
# FIND KEYWORDS BASED ON TOPICS
##################################################

clinton.tm <- convert(clinton.dfm, to="tm")
sanders.tm <- convert(sanders.dfm, to="tm")
trump.tm <- convert(trump.dfm, to="tm")

# Word associations
require(RKEA)
require(RKEAjars)
cl.strong.keywords <- list(c("peace", "sanctions", "secure", "values"),
                    c("work", "defend", "problems", "fighting"),
                    c("working", "results", "problems", "country"),
                    c("justice", "white", "opportunity", "action"))
                    

cl.latent.keywords <- list(c("shared", "commitment", "relationship", "deter"),
                           c("women", "dignity", "muslims", "job"),
                           c("rights", "progress", "immigrants", "violence"),
                           c("justice", "white", "opportunity", "action"))

tmpdir <- tempfile()
dir.create(tmpdir)
cl.model <- file.path(tmpdir, "clStrongModel")
createModel(clinton.sent, cl.strong.keywords, cl.model)


# N-gram analysis
NgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 4))
cl.ngram <- DocumentTermMatrix(clinton.tm$dimnames$Terms, control = list(tokenize = NgramTokenizer))
# little bit of regex to remove bigrams with stopwords in them, cf. http://stackoverflow.com/a/6947724/1036500
stpwrds <- paste(stopwords("en"), collapse = "|")
x$dimnames$Terms[!grepl(stpwrds, x$dimnames$Terms)]

