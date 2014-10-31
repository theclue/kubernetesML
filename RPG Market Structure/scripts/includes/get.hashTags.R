##########################
# get.hashTags
# ------------------------
# Get hashtags from a collection of tweets
# and load into an ordered data frame
###########

get.hashTags <- function(vec){
  
  hash.pattern <- "#[[:alpha:]]+"
  have.hash = grep(x = vec, pattern = hash.pattern)
  
  hash.matches <- gregexpr(pattern = hash.pattern,
                          text = vec[have.hash])
  extracted.hash <- regmatches(x = vec[have.hash], m = hash.matches)
  
  df <- data.frame(table(tolower(unlist(extracted.hash))))
  colnames(df) <- c("tag","freq")
  df <- df[order(df$freq,decreasing = TRUE),]
  return(df)
}