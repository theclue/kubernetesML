########################################################################
# Twitter Scrapring of data related to hashtags #rpg (and similars)    #
# ---------------------------------------------------------------------#
# source: Twitter                                                      #
########################################################################

############################
# Prerequisites
#####
require("twitteR")
require("streamR")
require("RJSONIO")
require("tm")
require("wordcloud")
require("xlsx")

# Download needed certification schemes
if(nchar(system.file("CurlSSL", "cacert.pem", package = "RCurl")) == 0){
  download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="./data/cacert.pem" )
  cacert <- "./data/cacert.pem"
} else {
  cacert <- system.file("CurlSSL", "cacert.pem", package = "RCurl")
}

twitter.oauth <- read.csv2("./data/twitter_oauth.csv", sep=";", stringsAsFactors=FALSE)

twitter.cred <- OAuthFactory$new(
  consumerKey=twitter.oauth[,"key"],
  consumerSecret=twitter.oauth[,"secret"],
  requestURL="https://api.twitter.com/oauth/request_token",
  accessURL="https://api.twitter.com/oauth/access_token",
  authURL="https://api.twitter.com/oauth/authorize")

#twitter.cred$handshake(cainfo="./data/cacert.pem")

# This is for reusing and avoid re-authorizing each time
save(twitter.cred, file = "./data/twitter.OAuth.RData")

############################
# Mining Data
# ------------
#####
rpg.twitter.df <- rpg.loadData(file="rpg.twitter.csv.zip", overwrite=FALSE, sep=";", header=T, quote="\"", stringsAsFactors=FALSE)

registerTwitterOAuth(twitter.cred)


#rpg.twitter <- searchTwitter(searchString = "#rpg", n=4000, lang="en", cainfo=cacert)
#rpg.twitter.df <- twListToDF(rpg.twitter)

#dnd.twitter <- searchTwitter(searchString = "#dnd", n=4000, lang="en", cainfo=cacert)
#dnd.twitter.df <- twListToDF(dnd.twitter)
#rpg.saveData(dnd.twitter.df, row.names=FALSE)

rpg.twitter.1 <- searchTwitter(searchString = "#rpg", sinceID=max(rpg.twitter.df$id), n=3000, lang="en", cainfo=cacert)
rpg.twitter.df.1 <- twListToDF(rpg.twitter.1)
rpg.twitter.df.1$created <- as.character(rpg.twitter.df.1$created)
rpg.twitter.df <- rbind(rpg.twitter.df, rpg.twitter.df.1)
rpg.saveData(rpg.twitter.df, row.names=FALSE)

dnd.twitter.1 <- searchTwitter(searchString = "#dnd", sinceID=max(dnd.twitter.df$id), n=3000, lang="en", cainfo=cacert)
dnd.twitter.df.1 <- twListToDF(dnd.twitter.1)
dnd.twitter.df.1$created <- as.character(dnd.twitter.df.1$created)
dnd.twitter.df <- rbind(dnd.twitter.df, dnd.twitter.df.1)
rpg.saveData(dnd.twitter.df, row.names=FALSE)


<<<<<<< HEAD
#  Users lookup EXPERIMENTAL
rpg.twitter.users <- lookupUsers(unique(rbind(rpg.twitter.df, dnd.twitter.df)$screenName))

rpg.twitter.userbase.SName <- sapply(rpg.twitter.users,screenName)
rpg.twitter.userbase.name <- sapply(rpg.twitter.users,name)
rpg.twitter.userbase.description <- sapply(rpg.twitter.users,description)
rpg.twitter.userbase.location <- sapply(rpg.twitter.users,location)
rpg.twitter.userbase.statusesCount <- sapply(rpg.twitter.users,statusesCount)
rpg.twitter.userbase.followersCount <- sapply(rpg.twitter.users,followersCount)
rpg.twitter.userbase.friendsCount <- sapply(rpg.twitter.users,friendsCount)

rpg.twitter.users.df <-cbind(rpg.twitter.userbase.name, rpg.twitter.userbase.SName,   rpg.twitter.userbase.description,  rpg.twitter.userbase.location, rpg.twitter.userbase.statusesCount, rpg.twitter.userbase.followersCount, rpg.twitter.userbase.friendsCount)

write.xlsx(x = rpg.twitter.users.df, file = "./data/twitter.users.xlsx",
           sheetName = "Twitter Users", row.names = FALSE)


=======
>>>>>>> e6f2d3a116bf7744dbcbd8d7b16894b1a0143f84
############################
# Corpus Creation: full text
# ---------------
# The corpus is created and cleaned, while
# relevant metadata is added to it
#####

rpg.twitter.df$hashtags <- regmatches(rpg.twitter.df$text,gregexpr("#(\\d|\\w)+",rpg.twitter.df$text))
rpg.twitter.df$mentions <- regmatches(rpg.twitter.df$text,gregexpr("@(\\d|\\w)+",rpg.twitter.df$text))

# Unlisting entities
rpg.twitter.df$hashtags <- sapply(rpg.twitter.df$hashtags, paste, collapse = " ")
rpg.twitter.df$mentions <- sapply(rpg.twitter.df$mentions, paste, collapse = " ")

rpg.twitter.corpus <-
  Corpus(VectorSource(rpg.twitter.df$text), list(language="en"))

# Some corpora cleaning
rpg.twitter.corpus <- tm_map(rpg.twitter.corpus, content_transformer(tolower))
rpg.twitter.corpus<- tm_map(rpg.twitter.corpus, removePunctuation)
rpg.twitter.corpus<- tm_map(rpg.twitter.corpus, stripWhitespace)
rpg.twitter.corpus <- tm_map(rpg.twitter.corpus, removeWords, stopwords("english"))

# Metadata
DublinCore(rpg.twitter.corpus, tag="creator") <- rpg.twitter.df$screenName
DublinCore(rpg.twitter.corpus, tag="date") <- rpg.twitter.df$created
DublinCore(rpg.twitter.corpus, tag="identifier") <- rpg.twitter.df$id
DublinCore(rpg.twitter.corpus, tag="source") <- rpg.twitter.df$statusSource
meta(rpg.twitter.corpus, tag="hashtags") <- rpg.twitter.df$hashtags

############################
# Hashtags Term-Document matrix
# ---------------
# This matrix is built using only hashtags
# It'll be smaller, but it's supposed to be cleaner
#####

rpg.hashtags.tdm <- TermDocumentMatrix(
  rpg.twitter.corpus,
  control = list(dictionary = unique(unlist(strsplit(gsub("#", "", rpg.twitter.df$hashtags), " ", fixed = TRUE)))
))

# Some explorative clustering
rpg.twitter.tdm.df <- as.data.frame(as.matrix(removeSparseTerms(rpg.hashtags.tdm, sparse = 0.99)))
rpg.twitter.tdm.df <- scale(rpg.twitter.tdm.df)

rpg.twitter.tdm.dist <- dist(rpg.twitter.tdm.df, method = "euclidean")

plot(hclust(rpg.twitter.tdm.dist, method = "ward"), main = "Cluster - #rpg")

rect.hclust(hclust(rpg.twitter.tdm.dist, method = "ward"), k = 5, border = "blue")


############################
# Exploration
# ---------------
#####
#wordcloud(rpg.twitter.corpus)

rpg.twitter.tdm <- TermDocumentMatrix(rpg.twitter.corpus)

findFreqTerms(rpg.twitter.tdm, lowfreq = 10)

rpg.twitter.tdm.clean <- removeSparseTerms(rpg.twitter.tdm, sparse = 0.98)

# Some explorative clustering
rpg.twitter.tdm.df <- as.data.frame(as.matrix(rpg.twitter.tdm.clean))
rpg.twitter.tdm.df <- scale(rpg.twitter.tdm.df)

rpg.twitter.tdm.dist <- dist(rpg.twitter.tdm.df, method = "euclidean")

plot(hclust(rpg.twitter.tdm.dist, method = "ward"), main = "Cluster - #rpg")

rect.hclust(hclust(rpg.twitter.tdm.dist, method = "ward"), k = 5, border = "blue")

#test <- filterStream(file.name = "", timeout=10, oauth=twitter.cred)
#test <- sampleStream(file.name = "", timeout = 10, oauth=twitter.cred)
#test.df <- parseTweets(test)