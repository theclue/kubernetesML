###########################################
# Exploratory Analysis on some attributes
# ---------------------------------------
# This is better performed on a data discovery
# tool (ie. Tableau), but some preliminary
# analysis is easily done on R, too
###########

# Most active authors
sort(table(meta(rpg.gathering.corpus)$author), decreasing = TRUE)[1:10]

# Which kind of role ppl prefer to act to?
table(meta(rpg.gathering.corpus)$role, useNA = "ifany")

# Depth of conversations
sort(table(meta(rpg.gathering.corpus)$replies), decreasing = TRUE)

# Reach of conversations
sort(table(meta(rpg.gathering.corpus)$views), decreasing = TRUE)

# Most frequent games
temp <- inspect(rpg.gathering.tdm)
rpg.gathering.freq <- data.frame(apply(temp, 1, sum))
rpg.gathering.freq <- data.frame(ST = row.names(rpg.gathering.freq), Freq = rpg.gathering.freq[, 1])
rpg.gathering.freq <- rpg.gathering.freq[order(rpg.gathering.freq$Freq, decreasing = T), ]
row.names(rpg.gathering.freq) <- NULL
rm(temp)
