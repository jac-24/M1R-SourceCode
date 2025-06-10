# The code for the how isotropic edge correction works

library(spatstat)
library(sf)
library(plotrix)


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
  
  
  # Plot the borough
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
  
  # Plot the tree locations
  points(planar_coord[,1],
         planar_coord[,2],
         pch=19,
         cex=0.85,
         col="red"
  )
  
  
  # Choose a particular tree and highlight its location
  centre_x_coord <- planar_coord[489, 1]
  centre_y_coord <- planar_coord[489, 2]
  points(centre_x_coord, centre_y_coord, col="blue", pch=19, cex=0.85)

  
  # Draw a circle around this tree
  circ_coords <- draw.circle(x=centre_x_coord,
                             y=centre_y_coord,
                             nv=100,
                             radius=1000, 
                             lwd=2,
                             border="blue")
  
  # Highlight the region of the circle outside of the borough observation window
  points(circ_coords$x[50:70], 
         circ_coords$y[50:70], 
         type="l", 
         lwd=2, 
         col="lightblue1"
         )

}

point_plot("Westminster", "Apple")