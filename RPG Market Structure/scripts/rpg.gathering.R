#################################################################################
# Role Playing Games Market Structure and Players' Preferences                  #
# ------------------------------------------------------------------------------#
# source: http://forum.rpg.net/forumdisplay.php?21-Gaming-Gatherings            #
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

require("rJava")
.jinit(parameters="-Xmx128g")

# TODO it's needed on Linux, but it gets Windows processing slowly
options(mc.cores=1)

require("RWeka")

source("../common/cSplit.R")
source("../common/s3.loadData.R")
source("../common/https.loadScript.R")
source("./scripts/includes/rpg.synonyms.R")

#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
#biocLite("Rlibstree") 


###############################
# Load Data
# ---------
#####

rpg.gathering <- rpg.loadData(file="rpg.gathering.posts.csv.zip", overwrite=FALSE, sep="|", header=T, quote="", stringsAsFactors=FALSE)
rpg.list <- rpg.loadData(file="rpg.list.csv.zip", overwrite=FALSE, sep="|", header=T, quote="", stringsAsFactors=FALSE)

############################
# Data Preprocessing
# ------------------
# Data is cleaned beforer being loaded
# into a corpus
#####

rownames(rpg.gathering) <- rpg.gathering$Id

rpg.gathering$RoleNorm <- ifelse(
                            (grepl("(GM|DM|storyteller|st|narrator)(.*)", rpg.gathering$Role, ignore.case = TRUE) &
                             grepl("(player|playing character|pc)(.*)", rpg.gathering$Role, ignore.case = TRUE)) |
                            (grepl("(both|either)(.*)", rpg.gathering$Role, ignore.case = TRUE)), "Both",
                              ifelse(
                                grepl("(GM|DM|storyteller|st|narrator)(.*)", rpg.gathering$Role, ignore.case = TRUE) |
                                grepl("(Game|Dungeon)(\\s)?Master", rpg.gathering$Role, ignore.case = TRUE), "Game Master",
                                ifelse(grepl("(player|playing character|pc)(.*)", rpg.gathering$Role, ignore.case = TRUE), "Player",
                                        ifelse(grepl("(look|search|looking|searching)([^\\s]* ){1,6}(player|playing character|pc)(.*)", rpg.gathering$Testo, ignore.case = TRUE, perl = TRUE) |
                                                 grepl("(form|forming)([^\\s]* ){1,6}group(.*)", rpg.gathering$Testo, ignore.case = TRUE, perl = TRUE),
                                                 "Game Master",
                                          ifelse(grepl("(join|joining|need|needing|search|searching)([^\\s]* ){1,6}group(.*)", rpg.gathering$Testo, ignore.case = TRUE, perl = TRUE), "Player",
                                                 ifelse(grepl("(need|needing|search|searching|seek|seeking)([^\\s]* ){1,6}(GM|DM|storyteller|st|narrator)(.*)", rpg.gathering$Testo, ignore.case = TRUE, perl = TRUE), "Player", NA
                                                 )
                                          )
                                      )  
                                )  
                              )
                            )

rpg.gathering$Anno <- substr(rpg.gathering$Data, nchar(rpg.gathering$Data)-4+1, nchar(rpg.gathering$Data))

rpg.gathering$Id <- as.numeric(as.character(rpg.gathering$Id))
rpg.gathering <- rpg.gathering[which(!is.na(rpg.gathering$Id)),]

############################
# Corpus Creation
# ---------------
# The corpus is created and cleaned, while
# relevant metadata are added to it
#####

# Load the corpus
# TODO check if it's ok to merge title and content
rpg.gathering.corpus <-
  Corpus(VectorSource(paste(rpg.gathering$Titolo,  rpg.gathering$Testo, sep = ". ")), list(language="en"))

# Case lowering
rpg.gathering.corpus <- tm_map(rpg.gathering.corpus, tolower)

# Resolve ambiguous abbreviations and synonyms
rpg.gathering.corpus <- tm_map(rpg.gathering.corpus, resolveRpgSynonyms)

# Remove extra trailing spaces
rpg.gathering.corpus<- tm_map(rpg.gathering.corpus, stripWhitespace)

# Coercing to PlainTextDocument
rpg.gathering.corpus <-  tm_map(rpg.gathering.corpus, PlainTextDocument)

# Metadata
DublinCore(rpg.gathering.corpus, tag="creator") <- rpg.gathering$Started
DublinCore(rpg.gathering.corpus, tag="title") <- rpg.gathering$Titolo
DublinCore(rpg.gathering.corpus, tag="date") <- rpg.gathering$Data
DublinCore(rpg.gathering.corpus, tag="identifier") <- rpg.gathering$Id
DublinCore(rpg.gathering.corpus, tag="source") <- rpg.gathering$Url
meta(rpg.gathering.corpus, tag="keywords") <- rpg.gathering$Keywords
meta(rpg.gathering.corpus, tag="role") <- rpg.gathering$RoleNorm
meta(rpg.gathering.corpus, tag="replies") <- rpg.gathering$Replies
meta(rpg.gathering.corpus, tag="views") <- rpg.gathering$Views

############################
# Phase 3: Adjacence Matrix
# Matrix is built against the dictionary of known RPG names
#####

FourGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 4))

# term-document matrix
rpg.gathering.tdm <- TermDocumentMatrix(rpg.gathering.corpus, control = list(tokenize = FourGramTokenizer, dictionary = rpg.list$TitoloNorm))

# document-term matrix
# rpg.gathering.dtm <- DocumentTermMatrix(rpg.gathering.corpus, control = list(tokenize = FourGramTokenizer,  dictionary = rpg.list$TitoloNorm))

# adjacence matrix
rpg.tdm.matrix <- as.matrix(rpg.gathering.tdm)
colnames(rpg.tdm.matrix) <- meta(rpg.gathering.corpus, type="indexed")$id

rpg.adj.matrix <- rpg.tdm.matrix
rpg.adj.matrix[rpg.tdm.matrix>=1] <- 1
rpg.adj.matrix <- rpg.adj.matrix %*% t(rpg.adj.matrix)


############################
# Phase 5: Prepare Gephi Graph
#####
rpg.gathering.gephi <- as.data.frame(as.table(rpg.adj.matrix))
rpg.gathering.gephi <- rpg.gathering.gephi[rpg.gathering.gephi$Freq > 0 & rpg.gathering.gephi$Terms != rpg.gathering.gephi$Terms.1, ] 
# Remove permutations
rpg.gathering.gephi <- rpg.gathering.gephi[!duplicated(t(apply(rpg.gathering.gephi[1:2], 1, sort))), ]
row.names(rpg.gathering.gephi) <- NULL
colnames(rpg.gathering.gephi) <- c("Source", "Target", "Weight")

# Build a interactive graph
rpg.graph.components <- edge.list(as.matrix(rpg.gathering.gephi[1:2]))
rpg.edges <- data.frame(rpg.graph.components$edges, rpg.gathering.gephi[3])
colnames(rpg.edges)[3] <- "weight"

rpg.nodes <- merge(x=merge(x=rpg.graph.components$nodes, y=rpg.list, by.x = "label", by.y = "TitoloNorm", all.x = TRUE), y=rpg.gathering.freq, by.x="label", by.y="ST", all.x=TRUE)[, c("id", "label","Pos", "Titolo", "Publisher","Anno", "Freq" )]

colnames(rpg.nodes) <- c("id", "label", "rank", "title", "publisher", "year", "occurrences")

rpg.graph <- simplify(graph.data.frame(rpg.edges, directed=FALSE, vertices=rpg.nodes))

V(rpg.graph)$name <- V(rpg.graph)$label
V(rpg.graph)$label <- V(rpg.graph)$title

# Output
write.graph(rpg.graph, "./graphs/rpg.gathering.graphml", format=c("graphml"))

# Tableau Output
write.csv2(rpg.gathering, "./tableau/rpg.gathering.tableau.csv", row.names=FALSE)
