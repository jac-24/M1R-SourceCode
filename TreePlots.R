# The code for the plots of trees in a borough

library(spatstat)
library(sf)


tree <- read.csv("LondonTrees.csv", header=T, sep=",")

point_plot <- function(area_name, tree_name){
  # Read in the CSV and store the latitude and longitude of tree locations
  lat <- tree$latitude
  long <- tree$longitude
  
  # Filter and store the latitude and longitude of trees in location you want
  area_lat <- lat[tree$borough==area_name & tree$gla_tree_name==tree_name]
  area_long <- long[tree$borough==area_name & tree$gla_tree_name==tree_name]
  
  lat_long_df <- data.frame(longitude=area_long, latitude=area_lat)
  
  # Convert latitude and longitude to planar coordinates so can use spatstat
  convert_sf <- st_as_sf(lat_long_df, coords=c("longitude", "latitude"), crs=4326)
  
  # Transforms coordinates to British National Grid coordinates
  planar_proj <- st_transform(convert_sf, 27700)
  planar_coord <- st_coordinates(planar_proj)
  
  boroughs <- read.csv("london_boroughs.csv", header=T, sep=",")
  
  # Now store boundaries of the borough chosen
  x_coords <- boroughs$x
  y_coords <- boroughs$y
  
  area_x <- as.numeric(na.omit(x_coords[boroughs$borough==area_name]))
  area_y <- as.numeric(na.omit(y_coords[boroughs$borough==area_name]))
  
  window <- owin(poly=list(x=rev(area_x), y=rev(area_y)))
  
  # Make the locations of the trees into a point pattern object
  point <- ppp(planar_coord[,1], planar_coord[,2], window=window)

  # Plots the boundary of the borough
  plot(area_x, 
       area_y, 
       type="l", 
       axes=F, 
       main="", 
       xlab="", 
       ylab="",
       lwd=2,
       asp=1
       )
  
  # Plots the locations of the trees
  points(planar_coord[,1],
         planar_coord[,2],
         pch=19,
         cex=0.85,
         col="red"
         )
  
}

point_plot("Westminster", "Apple")








