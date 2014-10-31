##############################################
# CSV Files for further analysis using Tableau
###########

############################
# Citation-level Detail Cube
#####
gdr.players.tableau <- as.data.frame(as.table(gdr.tdm.matrix))
gdr.players.tableau <- gdr.players.tableau[which(gdr.players.tableau$Freq > 0),]

gdr.players.tableau <- merge(x=gdr.players.tableau, y=meta(gdr.players.corpus, type="indexed", tag=c("id", "author", "datetimestamp", "role", "replies", "place")), by.x = "Docs", by.y = "id", all.x = TRUE)
gdr.players.tableau <- merge(x=gdr.players.tableau, y=rpg.list.italians, by.x = "Terms", by.y = "TitoloNorm", all.x = TRUE)
gdr.players.tableau <- merge(x=gdr.players.tableau, y=rpg.genres.italians, by.x = "Terms", by.y = "TitoloNorm", all.x = TRUE)
gdr.players.tableau <- merge(x=gdr.players.tableau, y=unique(italian.places[,c("Provincia", "Prov", "Regione", "Latitudine.Prov", "Longitudine.Prov")]), by.x = "place", by.y = "Provincia", all.x = TRUE)

gdr.players.tableau <- gdr.players.tableau[,c("Docs", "author", "role", "Terms", "Titolo", "Publisher", "Genre", "Anno", "Pos", "replies", "place", "Regione", "Latitudine.Prov", "Longitudine.Prov", "datetimestamp")]

colnames(gdr.players.tableau) <- c("Post ID", "Author", "Role", "Game ID", "Game Title", "Publisher", "Genre", "Publishing Year", "Rank Position", "Number of Replies", "Place", "Region",  "Latitude", "Longitude", "Post Date")

write.csv2(gdr.players.tableau, "./tableau/data/gdr.players.details.csv", row.names=FALSE)

#############################################
# RPG-level Aggregated Cube (with frequences)
#####

# Most frequent games
temp <- inspect(gdr.players.tdm)
gdr.players.freq <- data.frame(apply(temp, 1, sum))
gdr.players.freq <- data.frame(ST = row.names(gdr.players.freq), Freq = gdr.players.freq[, 1])
gdr.players.freq <- gdr.players.freq[order(gdr.players.freq$Freq, decreasing = T), ]
row.names(gdr.players.freq) <- NULL
rm(temp)

write.csv2(
  merge(
      x=merge(x=rpg.genres.italians, y=rpg.list.italians[,!names(rpg.list.italians) %in% c("Link", "Genre")], by="TitoloNorm"), 
      y=gdr.players.freq, by.x="TitoloNorm",
      by.y="ST", all.x=TRUE), 
  "./tableau/data/rpg.italians.list.csv", row.names=FALSE)
