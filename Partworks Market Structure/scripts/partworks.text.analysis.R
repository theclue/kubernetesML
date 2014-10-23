#######################################################
# DeA Partworks Model Users Analysis - Text Analysis  #
# --------------------------------------------------- #
# source: http://forum.model-space.co.uk/             #
#######################################################

############################
# Prerequisites
#####
setwd("./Partworks Market Structure")

require("tm")
require("RCurl")
require("reshap")
require("wordcloud")
require("ggplot2")

# Get the lexicons, then add some words with high contextual discriminativer power

# TODO: remove "issue" and "issues" as they could have not a negative meaning in the partworks scenario
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

https.Load("https://raw2.github.com/SocialMediaMininginR/sentiment_function/master/sentiment.R")

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

model.score <- score.sentiment(model.posts$plain.Body, pos.lexicon, neg.lexicon, .progress = "text")

model.posts <- cbind(model.posts, model.score$score)

colnames(model.posts) <- c("Thread.ID", "Post.Num", "Post.Author", "Post.Datetime", "Post.Body", "Post.Title", "Involved.Model", "Post.Year", "Post.Score")

# TODO: This pivot actually won't work...
#cast(melt(model.posts), Involved.Model ~ Post.Year, value = "Post.Score", fun.aggregate = mean, fill = 0)

# Since the casting won't work, let's degradate to  the good ok' ggplot2...
model.boxplot <- ggplot(model.posts, 
                        aes(x = model.posts$Post.Year, y= model.posts$Post.Score)) +
                        geom_boxplot(aes(fill = model.posts$Post.Year),
                                     outlier.colour = "black",
                                     outlier.shape = 16,
                                     outlier.size = 2) +
                        xlab("Year") +
                        ylab("Sentiment") +
                        ggtitle("Partworks Modelling - Sentiment over time")

model.boxplot.kit <- ggplot(model.posts, 
                        aes(x = model.posts$Involved.Model, y= model.posts$Post.Score)) +
  geom_boxplot(aes(fill = model.posts$Involved.Model),
               outlier.colour = "black",
               outlier.shape = 16,
               outlier.size = 2) +
  xlab("Year") +
  ylab("Sentiment") +
  ggtitle("Partworks Modelling - by Kit")

s3.saveData(model.posts, output.dir = "./tableau")
