rm(list=ls())

# Set working directory
setwd("file path name")

# Load in larvae coordinates from other script
larvae <- read.csv("larvae_tile_coords.csv", row.names=1)

# Lod our face adjustment values for adjacent and opposite distance measures
adj_faces <- read.csv("tile_adj_faces.csv")
opp_faces <- read.csv("tile_opp_faces.csv")

# set up unique ID for each larvae on each tile
larvae$ID <- paste(larvae$face.id, larvae$larvae.id,
                   sep=":")

# this loops over the larvae in each tile, calculating distances between
# each pair of larvae

dists <- do.call("rbind", lapply(split(larvae, f=larvae$tile.id), function(tile){

print(tile$tile.id[1])
  if(length(unique(tile$ID)) < 2){return(NULL)}
  
# get pairwise combinations of larvae IDs
tile.pairwise <- as.data.frame(t(combn(tile$ID, 2)))

# Match values for each pairwise combination
pairwise1 <- data.frame(ID = tile.pairwise[,1])
match1 <- match(pairwise1$ID, tile$ID)
pairwise1 <- cbind(pairwise1, tile[match1, !colnames(tile) %in% c("tile.id", "ID")])
pairwise2 <- data.frame(ID = tile.pairwise[,2])
match2 <- match(pairwise2$ID, tile$ID)
pairwise2 <- cbind(pairwise2, tile[match2, !colnames(tile) %in% c("tile.id", "ID")])
pairwise.df <- cbind(pairwise1, pairwise2)
colnames(pairwise.df) <- paste0(colnames(pairwise.df), rep(1:2, each=floor(ncol(pairwise.df)/2)))

# Work out whether each pair is on the same, adjacent or opposite sides of the
# tile
pairwise.df$category <- NA
pairwise.df$category[pairwise.df$face.id1 == pairwise.df$face.id2] = "Same"

adj.bin <- apply(pairwise.df[, c("face.id1", "face.id2")], 1, 
      function(x){
        x[2] %in% adj_faces[adj_faces[,1] == x[1], 2]
        })

pairwise.df$category[adj.bin] = "Adjacent"
pairwise.df$category[is.na(pairwise.df$category)] = "Opposite"

# CALCULATE DISTANCES

# This is a second loop for each pair of larvae within each tile. It makes the
# necessary adjustments to flatten the faces of the tile into a 2D plane, then
# calculates distance on that plane. For larvae on opposite faces, it gets the
# distance in each direction, then returns the smallest distance. For larvae on
# opposite sides of the two large faces, it also calculates the distance through
# the central hole, returning that distance if it's shorter than the other distances.
pairwise.df$distance <- lapply(1:dim(pairwise.df)[1], function(n){
  # make a copy of our pairwise.df with a shorter name so our code doesn't look
  # really messy
  pair <- pairwise.df
  
  # coordinates are a 2x2 matrix. Rows are larvae, columns are x and y axes
  coords <- data.frame(x = c(pair[n, "x.coord1"], pair[n, "x.coord2"]),
                       y = c(pair[n, "y.coord1"], pair[n, "y.coord2"]))
  
  # Are the larvae on the same face?
  if(pair[n, "category"] == "Same"){
    
    # simply calculate Eucalidean distance, and return from inner loop
    return(dist(coords))
    
  }
  
  # Are the larvae on adjacent faces?
  if(pair[n, "category"] == "Adjacent"){
  
  # Get the modifications we need to flatten the adjacent axes
  plane.mods <- adj_faces[adj_faces$face == pair[n, "face.id1"] &
                          adj_faces$adj.face == pair[n, "face.id2"],]
  
  # Step 1: Do we need to swap the x and y axes of face 2? This is essentially
  # a rotation of the face
  if(plane.mods$flip){
    
    coords[2, c(1,2)] = coords[2, c(2,1)]
    
  }
  
  # Step 2: Do we need to modify the axes of the second face? This is essentially
  # a horizontal or vertical flip of the face.
  # If we don't need to modify the faces, this just adds 0 so it's not a big problem
  coords[2, ] =  abs(coords[2, ] + c(plane.mods$modify.x,
                                     plane.mods$modify.y))
  
  # Step 3: Flatten faces to 2D plane by adding stacking the two beside or on top
  # of eachother (by adding the width/height of the beginning face to the ending face)
  coords[1,"x"] <- coords[1,"x"] + plane.mods$x.coord1 
  coords[1,"y"] <- coords[1,"y"] + plane.mods$y.coord1 
  coords[2, "x"] <- coords[2,"x"] + plane.mods$x.coord2 
  coords[2, "y"] <- coords[2, "y"] + plane.mods$y.coord2
 
  # Calculate Eudclidean distance, and return from inner loop
  return(dist(coords))
  
}

  # Are the larvae on opposite faces?
  if(pair[n, "category"] == "Opposite"){
    
    # Get the modifications to flatten faces in four different directions around
    # the tile
    plane.mods <- opp_faces[opp_faces$face == pair[n,"face.id1"],]
    
    # Repeat our coordinates 4 times so we can estimate four different distances
    opp.coords <- rbind(coords, coords, coords, coords)
    
    # Modify face 2 axes for each direction (each will be different)
    # This is equivalent to a horizontal or vertical flip
    opp.coords[c(2,4,6,8), ] =  abs(opp.coords[c(2,4,6,8), ] + cbind(plane.mods$modify.x,
                                                                 plane.mods$modify.y))
      
    # Adjust coords to put faces on a flat 2D plane
    opp.coords[c(1,3,5,7),"x"] <- opp.coords[c(1,3,5,7),"x"] + plane.mods$x.coord1 
    opp.coords[c(1,3,5,7),"y"] <- opp.coords[c(1,3,5,7),"y"] + plane.mods$y.coord1
    opp.coords[c(2,4,6,8),"x"] <- opp.coords[c(2,4,6,8),"x"] + plane.mods$x.coord2 
    opp.coords[c(2,4,6,8),"y"] <- opp.coords[c(2,4,6,8),"y"] + plane.mods$y.coord2
    
    # Calculate distance in each direction, keeping the shortest distance
    cand.dist<-min(c(dist(opp.coords[1:2,]),
                     dist(opp.coords[3:4,]),
                     dist(opp.coords[5:6,]),
                     dist(opp.coords[7:8,])))
    
    # If larvae are on the two large faces, calculate distance through the
    # circle
    if(pair[n, "face.id1"] %in% c(1,2)){
     
      # function to get the point on the edge of a circle, given the x and y
      # coordinates of the circle centre, the radius (r) and the degree around
      # the circle
      circ.point<-function(x, y, r, deg){
        c(x + r * cos((deg * pi) / (180)),
          y + r * sin((deg * pi) / (180)))
      }
      
      # Distance to hole for point on face 1 (Each point intersects the circle
      # twice, with angle, and angle + 180 degrees). Get both angles
      point.angles <- atan(coords[1,2] / coords[1,1]) * 180 / pi
      point.angles <- c(point.angles, point.angles + 180)
                        
      # Calculate points for both angles
      circle.offset <- rbind(circ.point(2.5, 2.5, 0.5, point.angles[1]),
                             circ.point(2.5, 2.5, 0.5, point.angles[2]))
      
      # The distance from larvae one to the circle is the smallest of the two
      # distances between the larvae and the edge of the circle
      circle.dist1 <- min(c(dist(rbind(coords[1,],
                                      circle.offset[1,])),
                           dist(rbind(coords[1,],
                                      circle.offset[2,]))))
      
      # Do the same for larvae two on the other side of the tile
      point.angles <- atan(coords[2,2] / coords[2,1]) * 180 / pi
      point.angles <- c(point.angles, point.angles + 180)
      
      circle.offset <- rbind(circ.point(2.5, 2.5, 0.5, point.angles[1]),
                             circ.point(2.5, 2.5, 0.5, point.angles[2]))
      
      circle.dist2 <- min(c(dist(rbind(coords[2,],
                                       circle.offset[1,])),
                            dist(rbind(coords[2,],
                                       circle.offset[2,]))))
      
      # The smallest distance is either the smallest overland distance, or the
      # distance from larvae 1 to the circle edge + larvae 2 to the circle edge +
      # the width of the tile
      cand.dist <- min(cand.dist,
                       sum(c(circle.dist1, 1, circle.dist2)))
      
    }
    
    # Return from the loop with the smallest distance
    return(cand.dist)
    
  }

})

# Once all pairwise distance are calculated, add the Tile ID to our data-frame
pairwise.df$tile.id <- tile$tile.id[1]

# Return from the outer loop with all the pairwise distances for this tile
return(pairwise.df[,c("tile.id", "ID1", "ID2", "distance")])

}))

#turn dists output into a matrix, as some columns are lists, then save
dists$distance <- unlist(dists$distance)
write.csv(dists, "dists.csv")















