########################################################################
# Trivial Twitter Sentiment Analysis (don't expect too much from it!)  #
# ---------------------------------------------------------------------#
# source: Twitter                                                      #
########################################################################

############################
# Prerequisites
#####
require("tm")

https.Load("https://raw.githubusercontent.com/SocialMediaMininginR/sentiment_function/master/sentiment.R")

positive.words <- scan(file.path(".", "scripts", "includes", "positive-words.txt"), what = "character", comment.char = ";")
negative.words <- scan(file.path(".", "scripts", "includes", "negative-words.txt"), what = "character", comment.char = ";")