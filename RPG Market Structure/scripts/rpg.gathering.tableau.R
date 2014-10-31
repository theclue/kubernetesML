##############################################
# CSV Files for further analysis using Tableau
###########

############################
# Citation-level Detail Cube
#####
rpg.gathering.tableau <- as.data.frame(as.table(rpg.tdm.matrix))
rpg.gathering.tableau <- rpg.gathering.tableau[which(rpg.gathering.tableau$Freq > 0),]

rpg.gathering.tableau <- merge(x=rpg.gathering.tableau, y=meta(rpg.gathering.corpus, type="indexed", tag=c("id", "author", "datetimestamp", "role", "replies", "views")), by.x = "Docs", by.y = "id", all.x = TRUE)
rpg.gathering.tableau <- merge(x=rpg.gathering.tableau, y=rpg.list, by.x = "Terms", by.y = "TitoloNorm", all.x = TRUE)
rpg.gathering.tableau <- merge(x=rpg.gathering.tableau, y=rpg.genres, by.x = "Terms", by.y = "TitoloNorm", all.x = TRUE)


rpg.gathering.tableau <- rpg.gathering.tableau[,c("Docs", "author", "role", "Terms", "Titolo", "Publisher", "Genre", "Anno", "Pos", "replies" , "views", "datetimestamp")]

colnames(rpg.gathering.tableau) <- c("Post ID", "Author", "Role", "Game ID", "Game Title", "Publisher", "Genre", "Publishing Year", "Rank Position", "Number of Replies", "Number of Views", "Post Date")

write.csv2(rpg.gathering.tableau, "./tableau/data/rpg.details.csv", row.names=FALSE)

#############################################
# RPG-level Aggregated Cube (with frequences)
#####
write.csv2(
  merge(
      x=merge(x=rpg.genres, y=rpg.list[,!names(rpg.list) %in% c("Link", "Genre")], by="TitoloNorm"), 
      y=rpg.gathering.freq, by.x="TitoloNorm",
      by.y="ST", all.x=TRUE), 
  "./tableau/data/rpg.list.csv", row.names=FALSE)
