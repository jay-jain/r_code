# Read in species means data with latitude and longitude
trees <- read.csv("/home/jay/Desktop/traits_speciesmean.csv",header = TRUE)

trees$lon
trees$lat

plot(newmap,xlim=c(-90,90),ylim=c(-180,180))
points(trees$lon,trees$lat,col="green",cex= 0.6,lwd=2)

library(ggmap)
map <- get_map("https://www.google.com/maps/@37.5579513,1.63669,3z")

ggmap(map) + geom_point(aes(x = lon, y = lat), data = trees[,c(3,2)], alpha = .5,size = 2,col="red")


mapPoints <- ggmap(map) + geom_point(aes(x = lon, y = lat, size = sqrt(flights)), data = airportD, alpha = .5)

# try_location_trait_byobs.csv
try_location_trait_byobs <- trees <- read.csv("/home/jay/Desktop/try_location_trait_byobs.csv",header = TRUE)
View(try_location_trait_byobs)

distinctCoords <- try_location_trait_byobs[!duplicated(try_location_trait_byobs$lat) & !duplicated(try_location_trait_byobs$lon),]
View(distinctCoords)

plot(newmap,xlim=c(-130,-100),ylim=c(20,80))
points(try_location_trait_byobs$lon,try_location_trait_byobs$lat,col="red",cex= 0.6)
