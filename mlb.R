mlbPlayers=read.table(file=file.choose()
                      header=T, sep=" ",
                      na.strings="`",
                      stringsAsFactors=F)
playerData = mlbPlayers[,c("RBI","AVG")]

png(file="player_rbi_avg.png")
plot(x=playerData$RBI, y=playerData$AVG,
     xlab="RBI", ylab="AVG", main="RBIs and Average")
dev.off()