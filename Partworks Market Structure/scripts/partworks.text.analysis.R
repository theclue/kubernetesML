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

# Get the lexicons
neg.lexicon <- scan(
  textConnection(
    getURL("https://raw2.github.com/SocialMediaMininginR/neg_words/master/negative-words.txt")
  ),
  what = "character",
  comment.char = ";"
)

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
model.tdm <- TermDocumentMatrix(model.corpus,
                                control = list(removePunctuation = TRUE,
                                               stopwords = TRUE,
                                               removeNumbers = TRUE,
                                               wordLengths = c(3, 20)))

print(model.tdm)

#model.freqTerms <- findFreqTerms(model.tdm, lowfreq = 50)
#findAssocs(model.tdm, "felipe", 0.10)

model.df <- scale(as.data.frame(as.matrix(removeSparseTerms(model.tdm, sparse=0.95))))

# Explorative Clustering
model.fit <- hclust(dist(model.df, method = "euclidean"), method = "ward.D")

plot(model.fit, main = "Cluster - Partworks Modelling")
rect.hclust(model.fit, k=5, border="red")

# K-Means Clustering
model.kmeans <- kmeans(removeSparseTerms(model.tdm, sparse=0.95), 4)

############################
# Sentiment Analysis
#####

