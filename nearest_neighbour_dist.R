rm(list=ls())


########get Nearest Neighbour Distances############

dist <- read.csv("file path name/dists.csv",
                 row.names=1)

#get nearest neighbour distance from all unique distances
nearest.neighbour <- do.call("rbind", lapply(unique(dist$tile.id), function(tile){
  
  print(tile)
  
  tile.dist <- dist[dist$tile.id == tile,]
  tile.dist$ID1 <- as.character(tile.dist$ID1)
  tile.dist$ID2 <- as.character(tile.dist$ID2)
  
  do.call("rbind", lapply(unique(unlist(tile.dist[,c("ID1","ID2")])),
                            function(ID){
                              
                              to.dists <- tile.dist[tile.dist$ID1 == ID,]
                              
                              from.dists <- tile.dist[tile.dist$ID2 == ID,]
                              from.dists <- from.dists[,c(1,3,2,4)]
                              colnames(from.dists) <- colnames(to.dists)
                              
                              comb.dists <- rbind(to.dists, from.dists)
                              
                              min.dist <- comb.dists[which.min(comb.dists$distance),]
                              colnames(min.dist)[2:3] <- c("fID", "nID")
                              
                              return(min.dist)
                              
                              }))
  
}))

write.csv(nearest.neighbour, "nearest.csv")
