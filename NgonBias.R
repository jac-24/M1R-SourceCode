library(spatstat)


# Finds the circumradius for a given regular polygon with n sides
find_circumradius <- function(n){
  side_len <- sqrt((4*100*tan(pi/n))/(n))
  
  return(side_len/(2*sin(pi/n)))
}

# Calculates estimated bias at some value frac of the radius Kest is evaluated
# at to the circumradius of the polygon
bias_ngon_frac <- function(n, area, lambda, frac){
  side_len <- sqrt((4*area*tan(pi/n))/(n))
  # Creates a regular polygon of fixed area
  window <- regularpolygon(n, edge=side_len, centre=c(0, 0))
  
  
  # Stores bias at the particular point the polygon
  point_bias <- c()
  
  # Calculates the radius Kest needs to be calulcated at so the ratio of
  # that r to the circumradius of the polygon is correct
  radius <- find_circumradius(n) * frac
  for (i in 1:1000){
    pois_pp <- rpoispp(lambda, win=window)
    rip_k <- Kest(pois_pp, correction="isotropic", rmax=radius)
    point_bias <- append(point_bias, rip_k$iso[length(rip_k$r)])
  }
  
  # Calculates the estimated bias by taking the average of the simulations
  bias <- (sum(point_bias)/1000) - (pi*radius**2)
  return(bias)
}

# List containing number of sides of polygons
x_list <- seq(from=3, to=100, by=1)

# Plot the bias at this value of frac for a triangle
rip <- bias_ngon_frac(n=3, area=100, lambda=0.5, frac=0.1)

# List containing estimated bias for each polygon with frac=0.1
y_list <- c(rip)

plot(3,
     rip,
     xlim=c(0, 100),
     ylim=c(-0.8, 0.8),
     xlab="Number of Sides",
     ylab="Estimated Bias",
     pch=19
     )

# Plot the legend
legend("topleft", legend=c("0.1", "0.4", "0.9"), fill=c("black", "red", "blue"), horiz=T)


# The code below adds the estimated bias for different polygons and
# values of frac to the plot

for (i in 3:100) {
  rip <- bias_ngon_frac(i, 100, 0.5, 0.9)
  points(i, rip, pch=19, col="blue")
}

for (i in 4:100) {
  rip <- bias_ngon_frac(i, 100, 0.5, 0.1)
  # Append bias for each polygon to y_list
  y_list <- append(y_list, rip)
  points(i, rip, pch=19)
}


for (i in 3:100) {
  rip <- bias_ngon_frac(i, 100, 0.5, 0.4)
  points(i, rip, pch=19, col="red")
}


# Print the correlation coefficient for the estimated bias and number of sides
print(cor(x_list, y_list), method="pearson")

