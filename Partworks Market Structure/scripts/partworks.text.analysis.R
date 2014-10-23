#######################################################
# DeA Partworks Model Users Analysis - Text Analysis  #
# --------------------------------------------------- #
# source: http://forum.model-space.co.uk/             #
#######################################################

############################
# Prerequisites
#####
require("tm")
require("RCurl")
require("reshap")
require("wordcloud")

# Get the lexicons, then add some words with high contextual discriminativer power
neg.lexicon <- c(scan(
  textConnection(
    getURL("https://raw2.github.com/SocialMediaMininginR/neg_words/master/negative-words.txt")
  ),
  what = "character",
  comment.char = ";"
), "missing", "differs", "differed", "replacements", "replace", "partsmissing", "boomberang" )

pos.lexicon <- scan(
  textConnection(
    getURL("https://raw2.github.com/SocialMediaMininginR/pos_words/master/positive-words.txt")
  ),
  what = "character",
  comment.char = ";"
)

############################
# Preliminary Analysis
#####

# Some non-discriminative words are added by rule of thumb
model.stopwords <- unique(c(
  stopwords("SMART"), 
  stopwords("english"),
  "model", "wrote", "dont", "forum", "post", "mclaren", "victory", "cheers", "tomick", "mike", "deagostini",
  "deag", "models", "john", "email", "isnt", "yamato", "youll", "agostini", "tamiya", "endeavour", "admin"))

# For some reason, controlling stopwords won't work, so a term clean corpus is built
model.tdm <- TermDocumentMatrix(tm_map(model.corpus, removeWords, model.stopwords),
                                control = list(removePunctuation = TRUE,
                                               weighting = weightTf,
                                               removeNumbers = TRUE,
                                               wordLengths = c(4, 20)))

print(model.tdm)

#model.freqTerms <- findFreqTerms(model.tdm, lowfreq = 50)
#findAssocs(model.tdm, "felipe", 0.10)

model.df <- scale(as.data.frame(as.matrix(removeSparseTerms(model.tdm, sparse=0.97))))

# Explorative Clustering
model.fit <- hclust(dist(model.df, method = "euclidean"), method = "ward.D")

plot(model.fit, main = "Cluster - Partworks Modelling")
rect.hclust(model.fit, k=5, border="red")

# K-Means Clustering
model.kmeans <- kmeans(removeSparseTerms(model.tdm, sparse=0.96), 4)

# Use this list to spot most-frequent words.
# These words have poor discriminative power, so they're good candidates
# to be added to a stopwords list.
model.frequent <- sort(rowSums(as.matrix(model.tdm)), decreasing = TRUE)

############################
# Topic Correlations
#####
model.rsums <- sort(
  rowSums(
    as.matrix(removeSparseTerms(model.tdm, sparse=0.99)))
  , decreasing = TRUE)

model.rsum.df <- data.frame(word=names(model.rsums), freq=model.rsums)
row.names(model.rsum.df) <- NULL

wordcloud(model.rsum.df$word,
          model.rsum.df$freq,
          scale=c(7,.2),
          min.freq=4,
          max.words=250,
          random.order=FALSE,
          colors=brewer.pal(9, "BuGn")[-(1:2)]
          )

############################
# Sentiment Analysis
#####
model.buzz <- cast(melt(model.posts), model ~ Year, length)


