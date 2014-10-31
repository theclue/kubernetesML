#################################################################################
# Role Playing Games Market Structure and Players' Preferences (Italy)          #
# ------------------------------------------------------------------------------#
# source: http://gdrplayers.it                                                  #
#################################################################################

############################
# Prerequisites
#####
require("tm")
require("koRpus")
require("SnowballC")
require("plyr")
require("igraph")
require("rgexf")
require("data.table")

require("lubridate")

require("rJava")
.jinit(parameters="-Xmx128g")

# TODO it's needed on Linux, but it gets Windows processing slowly
options(mc.cores=1)

require("RWeka")

source("./scripts/includes/rpg.synonyms.R")
source("./scripts/includes/cSplit.R")
source("./scripts/includes/rpg.loadData.R")

#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")


###############################
# Load Data
# ---------
#####

gdr.players <- rpg.loadData(file="gdr.players.csv.zip", overwrite=FALSE, sep=",", header=T, quote="\"", stringsAsFactors=FALSE)

rpg.list.italians <- rbind(
  rpg.loadData(file="rpg.list.italians.csv.zip", overwrite=FALSE, sep="|", header=T, quote="", stringsAsFactors=FALSE), 
  rpg.loadData(file="rpg.list.csv.zip", overwrite=FALSE, sep="|", header=T, quote="", stringsAsFactors=FALSE)
)

rpg.list.italians.attributes <- rbind(
  rpg.loadData("rpg.list.attributes.italians.csv.zip", overwrite=FALSE, sep=",", header=T, quote="\"", stringsAsFactors=FALSE),
  rpg.loadData("rpg.list.attributes.csv.zip", overwrite=FALSE, sep=",", header=T, quote="\"", stringsAsFactors=FALSE)
)

italian.places <- rpg.loadData(file="italian.places.csv.zip", overwrite=FALSE, sep=";", header=T, quote="", stringsAsFactors=FALSE)

colnames(rpg.list.italians.attributes)[1] <- "Link"
rpg.list.italians.attributes <- merge(
  x=rpg.list.italians[,c("Link", "TitoloNorm")], 
  y=rpg.list.italians.attributes[,!names(rpg.list.italians.attributes) %in% "Titolo"], 
  by="Link", all.x=TRUE)[,!names(rpg.list.italians.attributes) %in% "Link"]


############################
# Data Preprocessing
# ------------------
# Data is cleaned beforer being loaded
# into a corpus
#####

gdr.players$dt <- as.character(parse_date_time(gdr.players$dt, "dmy"))

gdr.players$role <- 
           ifelse(grepl("(ricerca|cerco|cerchiamo|cercando|cercasi|cerca|cercavo)([^\\s]* ){1,4}(avventurier|giocator|element|component|person)(.*)", paste(gdr.players$titolo, gdr.players$testo, sep = ". "), ignore.case = TRUE, perl = TRUE) |
                    grepl("(crea|creare|creando|forma|formare|formando|amplia|ampliare|ampliando)([^\\s]* ){1,4}grupp(.*)", paste(gdr.players$titolo, gdr.players$testo, sep = ". "), ignore.case = TRUE, perl = TRUE),
                  "Game Master",
                ifelse(grepl("(ricerca|cercasi|cerco|cerchiamo|cercando|cercavo|chiedo|chiedere|chiedendo|chiedevo|unire|unirti|unirvi)([^\\s]* ){1,4}grupp(.*)", paste(gdr.players$titolo, gdr.players$testo, sep = ". "), ignore.case = TRUE, perl = TRUE) |
                           grepl("sono([^\\s]* ){1,3}(giocatore|giocatrice)([^\\s]* ){1,5}(cerco|cercando|unir)(.*)", paste(gdr.players$titolo, gdr.players$testo, sep = ". "), ignore.case = TRUE, perl = TRUE) |
                           grepl("(ricerca|cercasi|cerco|cerchiamo|cercando|cerca|cercavo)([^\\s]* ){1,4}(party|master|narratore|storyteller|custode|gm|dm|dungeon master)(.*)", paste(gdr.players$titolo, gdr.players$testo, sep = ". "), ignore.case = TRUE, perl = TRUE) | 
                         grepl("aggregare", paste(gdr.players$titolo, gdr.players$testo, sep = ". "), ignore.case = TRUE, perl = TRUE),                      
                          "Player", NA )
                  )


############################
# Corpus Creation
# ---------------
# The corpus is created and cleaned, while
# relevant metadata are added to it
#####

# Load the corpus
# TODO check if it's ok to merge title and content
gdr.players.corpus <-
  Corpus(VectorSource(paste(gdr.players$titolo,  gdr.players$testo, sep = ". ")), list(language="it"))

# Case lowering
gdr.players.corpus <- tm_map(gdr.players.corpus, tolower)

# Resolve ambiguous abbreviations and synonyms
gdr.players.corpus <- tm_map(gdr.players.corpus, resolveRpgSynonyms)

# Remove extra trailing spaces
gdr.players.corpus<- tm_map(gdr.players.corpus, stripWhitespace)

# Coercing to PlainTextDocument
gdr.players.corpus <-  tm_map(gdr.players.corpus, PlainTextDocument)

# Metadata
DublinCore(gdr.players.corpus, tag="identifier") <- gsub("http://www.gdrplayers.it/", "", substr(gdr.players$url, 1, nchar(gdr.players$url)-1))
DublinCore(gdr.players.corpus, tag="creator") <- gdr.players$author
DublinCore(gdr.players.corpus, tag="title") <- gdr.players$titolo
DublinCore(gdr.players.corpus, tag="date") <- gdr.players$dt
DublinCore(gdr.players.corpus, tag="source") <- gdr.players$url
meta(gdr.players.corpus, tag="role") <- gdr.players$role
meta(gdr.players.corpus, tag="replies") <- gdr.players$comments
meta(gdr.players.corpus, tag="place") <- gdr.players$provincia


############################
# Phase 3: Adjacence Matrix
# Matrix is built against the dictionary of known gdr names
#####

FourGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 4))

# term-document matrix
gdr.players.tdm <- TermDocumentMatrix(gdr.players.corpus, control = list(tokenize = FourGramTokenizer, dictionary = rpg.list.italians$TitoloNorm))

# document-term matrix
# gdr.players.dtm <- DocumentTermMatrix(gdr.players.corpus, control = list(tokenize = FourGramTokenizer,  dictionary = rpg.list.italians$TitoloNorm))

# adjacence matrix
gdr.tdm.matrix <- as.matrix(gdr.players.tdm)
colnames(gdr.tdm.matrix) <- meta(gdr.players.corpus, type="indexed")$id

gdr.adj.matrix <- gdr.tdm.matrix
gdr.adj.matrix[gdr.tdm.matrix>=1] <- 1
gdr.adj.matrix <- gdr.adj.matrix %*% t(gdr.adj.matrix)

#####################
# Phase 4: Attributes
#####

rpg.list.italians.attributes$Genre <- gsub("/", " / ", rpg.list.italians.attributes$Genre)
rpg.list.italians.attributes$Genre <- gsub("Science-Fantasy", "Science / Fantasy", rpg.list.italians.attributes$Genre)
rpg.list.italians.attributes$Genre <- gsub("Science Fantasy", "Science / Fantasy", rpg.list.italians.attributes$Genre)
rpg.list.italians.attributes$Genre <- gsub("Sci-Fi", "Science Fiction", rpg.list.italians.attributes$Genre)
rpg.list.italians.attributes$Genre <- gsub("Sci Fi", "Science Fiction", rpg.list.italians.attributes$Genre)
rpg.list.italians.attributes$Genre <- gsub("/", " / ", rpg.list.italians.attributes$Genre)
rpg.list.italians.attributes$Genre <- stripWhitespace(rpg.list.italians.attributes$Genre)

rpg.list.italians.attributes$System <- gsub("/", " / ", rpg.list.italians.attributes$System)
rpg.list.italians.attributes$System <- gsub("/", " / ", rpg.list.italians.attributes$System)
rpg.list.italians.attributes$System <- stripWhitespace(rpg.list.italians.attributes$System)

#rpg.authors <- as.data.frame(dcast.data.table(cSplit(rpg.list.italians.attributes, "Author", ", ", "long"), TitoloNorm ~ Author, value.var = "Author", fun.aggregate = length))
#rpg.genres <- as.data.frame(dcast.data.table(cSplit(rpg.list.italians.attributes, "Genre", " / ", "long"), TitoloNorm ~ Genre, value.var = "Genre",fun.aggregate = length))
#rpg.settings <- as.data.frame(dcast.data.table(rpg.list.italians.attributes, TitoloNorm ~ Genre, value.var = "Setting", fun.aggregate = length))
#rpg.systems <- as.data.frame(dcast.data.table(cSplit(rpg.list.italians.attributes, "System", " / ", "long"), TitoloNorm ~ System, value.var = "System",fun.aggregate = length))

rpg.genres.italians <- as.data.frame(cSplit(rpg.list.italians.attributes, "Genre", " / ", "long"))[, c("TitoloNorm", "Genre")]