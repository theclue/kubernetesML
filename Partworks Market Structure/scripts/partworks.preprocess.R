########################################################
# DeA Partworks Model Users Analysis                   #
# ---------------------------------------------------- #
# source: http://forum.model-space.co.uk/              #
########################################################

############################
# Prerequisites
#####
setwd("./Partworks Market Structure")

require("tm")
require("koRpus")
require("SnowballC")
require("plyr")
require("tm.plugin.webmining")

require("rJava")
.jinit(parameters="-Xmx128g")

# TODO it's needed on Linux, but it gets Windows processing slowly
options(mc.cores=1)

require("RWeka")

source("../common/cSplit.R")
source("../common/s3.loadData.R")
source("../common/https.loadScript.R")

###############################
# Load Data
# ---------
#####
model.posts <- s3.loadData(file="model.posts.csv.zip", input.dir="partworks", overwrite=FALSE, sep=";", header=T, stringsAsFactors=FALSE)
model.threads <- s3.loadData(file="model.threads.csv.zip", input.dir="partworks", overwrite=FALSE, sep=";", header=T, stringsAsFactors=FALSE)

###############################
# Corpus Preprocessing
# --------------------
#####

# Assure the row.names is the index of the file
rownames(model.posts) <- model.posts$X
rownames(model.threads) <- model.threads$X

# Remove quoted text, then convert to plain text
model.posts$plain.Body <- (
  unlist(
    lapply(
      model.posts$Body, function(x)
        gsub("^\\s+|\\s+$", "", 
             extractHTMLStrip(
               gsub("(<DIV class=quote><B>[\\w]+\\s*wrote:<\\/B>\\s*<DIV class=innerquote>).*?(<\\/DIV><\\/DIV>)", "", 
                    gsub("(?<=[.,!?()])(?! )", " ", 
                         gsub("<BR>|<\\/LI>|<\\/BR>", " ", x, perl=TRUE, ignore.case = TRUE)
                         , perl=TRUE)
                    , perl=TRUE, ignore.case = TRUE)
               )
             )
      )
    )
  )

# TODO: remove signatures

model.posts <- model.posts[,!(names(model.posts) %in% c("X", "Body"))]
model.threads <- model.threads[, !(names(model.threads) %in% c("X"))]

model.posts <- merge(x=
                 merge(x=model.posts,
                      y=data.frame(model.threads[, (names(model.threads) %in% c("ID", "Title"))], Num=1),
                      by.x=c("thread.ID", "Num"),
                      by.y=c("ID", "Num"),
                      all.x=TRUE),
               y=model.threads[, (names(model.threads) %in% c("ID", "model"))],
               by.x="thread.ID",
               by.y="ID",
               all.x=TRUE
)

############################
# Corpus Creation
# ---------------
# The corpus is created and cleaned, while
# relevant metadata is added to it
#####
model.corpus <- Corpus(
  VectorSource(
    paste(
      gsub("NA. ", "", 
           paste(model.posts$Title, model.posts$plain.Body, sep=". ")
           )
      )
    )
  , list(language="en")
  )

# Corpus preprocessing
model.corpus <- tm_map(model.corpus, tolower)
model.corpus <- tm_map(model.corpus, stripWhitespace)
model.corpus <-  tm_map(model.corpus, PlainTextDocument)

# Metadata
DublinCore(model.corpus, tag="creator") <- model.posts$Author
DublinCore(model.corpus, tag="title") <- model.posts$Title
DublinCore(model.corpus, tag="date") <- model.posts$DT
DublinCore(model.corpus, tag="identifier") <- paste(model.posts$thread.ID, model.posts$Num, sep="/")
