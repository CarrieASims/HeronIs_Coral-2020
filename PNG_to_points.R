# clear everything our of workspace
rm(list=ls())
# load libraries
library(raster)
library(spatstat)
library(rgdal)
library(rgeos)
library(maptools)
library(imager)


folder<- "file path name"

tile.files <- list.files(folder, pattern=".png")

tile <- tile.files[1]

# start a loop for each tile file
results <- do.call("rbind", lapply(tile.files, function(tile){
  
  print(paste0("Starting ", tile))
  
  x<-raster(paste0(folder, "/", tile))
  x[x==255]=NA
  x[x==0] = NA # get rid of black outlines
  
  # retain only the most common pixel color (point interior)
  pixel.table <- table(as.vector(x))
  common <- names(pixel.table)[which.max(pixel.table)]
  x[x != as.numeric(common) & !is.na(x)] = NA
  x[x == as.numeric(common) & !is.na(x)] = 1
  
  # get ID numbers for tile and face
  tile.id <- as.numeric(substr(tile, 1, regexpr("_", tile)-1))
  face.id <- as.numeric(substr(tile, 
                               regexpr("_", tile)+1,
                               regexpr("-", tile)-1))
  
  s<-as.matrix(extent(x)) #giving pixels on x and y axis
  
  # get pixel to cm conversion based on face number
  if(face.id %in% c(1,2)){
    
    x.pixels <- s[1,2] / 5
    y.pixels <- s[2,2] / 5
    
  }
  
  if(face.id %in% 3){
    
    x.pixels <- s[1,2] / 5
    y.pixels <- s[2,2] / 1
    
  }
  
  if(face.id %in% 4){
    
    x.pixels <- s[1,2] / 5
    y.pixels <- s[2,2] / 1
    
  }
  
  if(face.id %in% 5){
    
    x.pixels <- s[1,2] / 5
    y.pixels <- s[2,2] / 1
    
  }
  
  if(face.id %in% 6){
    
    x.pixels <- s[1,2] / 5
    y.pixels <- s[2,2] / 1
    
  }
  
  f<-rasterToPolygons(x, n=4, dissolve=TRUE)

  # separate the one polygon layer into multiple separate polygons
  j<-disaggregate(f)
  
  # Write shape file - change between settlement and survival
  #writeOGR(obj = j,
          # dsn = paste0(folder, "/settlement shape file"),
          # layer = gsub("\\.png", "", tile),
          # driver = "ESRI Shapefile")
  
  # final points dataframe which is the centroids of those polygons
  centers <- SpatialPointsDataFrame(gCentroid(j, byid=TRUE),j@data, match.ID=FALSE)
  
  l.coords <- as.data.frame(centers@coords)
  l.coords$x <- l.coords$x / x.pixels
  l.coords$y <- l.coords$y / y.pixels
  
  return(data.frame(tile.id = tile.id,
                    face.id = face.id,
                    larvae.id = 1:dim(l.coords)[1],
                    x.coord = l.coords$x,
                    y.coord = l.coords$y))
  
}))

write.csv(results, "larvae_tile_coords.csv")



