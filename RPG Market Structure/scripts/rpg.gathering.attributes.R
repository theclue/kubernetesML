###########################################
# Appendix: Additional Attributes Analysis
###########

############################
# Prerequisites
#####
require("tm")
require("plyr")
require("data.table")

# Execute previous step if required data has not been processed
if(!exists("rpg.list")){ 
  source("./scripts/rpg.gathering.R")  
}

if(!exists("cSpit")){ 
  source("./scripts/includes/cSplit.R")
}
if(!exists("rpg.loadData")){ 
  source("./scripts/includes/rpg.loadData.R")
}

###################################
# Phase 1: Get Data
###########

rpg.list.attributes <- rpg.loadData("rpg.list.attributes.csv.zip", overwrite=FALSE, sep=",", header=T, quote="\"", stringsAsFactors=FALSE)

colnames(rpg.list.attributes)[1] <- "Link"
rpg.list.attributes <- merge(x=rpg.list[,c("Link", "TitoloNorm")], y=rpg.list.attributes[,!names(rpg.list.attributes) %in% "Titolo"], by="Link", all.x=TRUE)[,!names(rpg.list.attributes) %in% "Link"]

###################################
# Phase 2: Corpus Preparation
###########

rpg.list.attributes$Genre <- gsub("/", " / ", rpg.list.attributes$Genre)
rpg.list.attributes$Genre <- gsub("Science-Fantasy", "Science / Fantasy", rpg.list.attributes$Genre)
rpg.list.attributes$Genre <- gsub("Science Fantasy", "Science / Fantasy", rpg.list.attributes$Genre)
rpg.list.attributes$Genre <- gsub("Sci-Fi", "Science Fiction", rpg.list.attributes$Genre)
rpg.list.attributes$Genre <- gsub("Sci Fi", "Science Fiction", rpg.list.attributes$Genre)
rpg.list.attributes$Genre <- gsub("/", " / ", rpg.list.attributes$Genre)
rpg.list.attributes$Genre <- stripWhitespace(rpg.list.attributes$Genre)

rpg.list.attributes$System <- gsub("/", " / ", rpg.list.attributes$System)
rpg.list.attributes$System <- gsub("/", " / ", rpg.list.attributes$System)
rpg.list.attributes$System <- stripWhitespace(rpg.list.attributes$System)

#rpg.authors <- as.data.frame(dcast.data.table(cSplit(rpg.list.attributes, "Author", ", ", "long"), TitoloNorm ~ Author, value.var = "Author", fun.aggregate = length))
#rpg.genres <- as.data.frame(dcast.data.table(cSplit(rpg.list.attributes, "Genre", " / ", "long"), TitoloNorm ~ Genre, value.var = "Genre",fun.aggregate = length))
#rpg.settings <- as.data.frame(dcast.data.table(rpg.list.attributes, TitoloNorm ~ Genre, value.var = "Setting", fun.aggregate = length))
#rpg.systems <- as.data.frame(dcast.data.table(cSplit(rpg.list.attributes, "System", " / ", "long"), TitoloNorm ~ System, value.var = "System",fun.aggregate = length))

rpg.genres <- as.data.frame(cSplit(rpg.list.attributes, "Genre", " / ", "long"))[, c("TitoloNorm", "Genre")]

# Build a interactive graph
rpg.genre.components <- edge.list(as.matrix(rpg.genres))
rpg.genre.edges <- data.frame(rpg.genre.components$edges)

#rpg.genre.nodes <- merge(x=merge(x=rpg.genre.components$nodes, y=rpg.list, by.x = "label", by.y = "TitoloNorm", all.x = TRUE), y=rpg.gathering.freq
#, by.x="label", by.y="ST", all.x=TRUE)[, c("id", "label","Pos", "Titolo", "Publisher","Anno", "Freq" )]
rpg.genre.nodes <- merge(x=rpg.genre.components$nodes, y=data.frame(rpg.list, type="Game")[,c("TitoloNorm", "Titolo", "type")], by.x="label", by.y="TitoloNorm", all.x=TRUE)[,c("id", "label", "Titolo", "type")]


rpg.genre.nodes$type <- as.character(rpg.genre.nodes$type)
rpg.genre.nodes[which(is.na(rpg.genre.nodes$type)),]$type <- "Genre"

colnames(rpg.genre.nodes) <- c("id", "label", "titolo", "type")

rpg.genre.graph <- simplify(graph.data.frame(rpg.genre.edges, directed=FALSE, vertices = rpg.genre.nodes))
#rpg.genre.graph <- simplify(graph.data.frame(rpg.genres, directed=FALSE))

V(rpg.graph)$name <- V(rpg.graph)$label
V(rpg.graph)$label <- V(rpg.graph)$title

# Output
write.graph(rpg.genre.graph, "./graphs/rpg.genre.graphml", format=c("graphml"))

rpg.list.genres <- merge(x=rpg.genres, y=rpg.list, by = "Link", all.x = TRUE)[, union(names(rpg.list), names(rpg.genres))]

# Genre-Genre Analysis
rpg.genres.t <- t(as.matrix(rpg.genres))
colnames(rpg.genres.t) <- rpg.genres.t[1,]
rpg.genres.t <- rpg.genres.t[-1,]
rpg.genres.t <- apply(rpg.genres.t, 2, function(x) as.numeric(as.character(x)))
row.names(rpg.genres.t) <- row.names(t(as.matrix(rpg.genres)))[-1]

# adjacence matrix
rpg.genres.adj <- rpg.genres.t %*% t(rpg.genres.t)

# output

rpg.genres.adj.gephi <- as.data.frame(as.table(rpg.genres.adj))
rpg.genres.adj.gephi <- rpg.genres.adj.gephi[rpg.genres.adj.gephi$Freq > 0 & rpg.genres.adj.gephi$Var1 != rpg.genres.adj.gephi$Var2, ] 
# Remove permutations
rpg.genres.adj.gephi <- rpg.genres.adj.gephi[!duplicated(t(apply(rpg.genres.adj.gephi[1:2], 1, sort))), ]
row.names(rpg.genres.adj.gephi) <- NULL
colnames(rpg.genres.adj.gephi) <- c("Source", "Target", "Weight")

# Build a interactive graph
rpg.genre.adj.graph.components <- edge.list(as.matrix(rpg.genres.adj.gephi[1:2]))
rpg.genre.adj.edges <- data.frame(rpg.genre.adj.graph.components$edges, rpg.genres.adj.gephi[3])
colnames(rpg.genre.adj.edges)[3] <- "weight"

rpg.genre.adj.nodes <- rpg.genre.adj.graph.components$nodes

rpg.genre.adj.graph <- simplify(graph.data.frame(rpg.genre.adj.edges, directed=FALSE, vertices=rpg.genre.adj.nodes))

V(rpg.genre.adj.graph)$name <- V(rpg.genre.adj.graph)$label

# Gephi Output
write.graph(rpg.genre.adj.graph, "./graphs/rpg.genres.adj.graphml", format=c("graphml"))
