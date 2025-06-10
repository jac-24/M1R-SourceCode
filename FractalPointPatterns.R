library(pracma)
library(spatstat)

frac_point_pat <- function(iter, name, lambda){
  # Generate the fractal curve
  frac <- fractalcurve(iter, name)
  
  # Create a window object so can do point pattern analysis
  window <- owin(poly=list(x=(frac$x), y=(frac$y)))

  # Generate a homogeneous point pattern with the fractal as its window
  pois_pp <- rpoispp(lambda=lambda, win=window)
  return(pois_pp)
}


# Stores this point pattern object
pois <- Kest(frac_point_pat(6, "snowflake", 74.6), correction="isotropic")

# Create a list of the bias so far which is updated with each pattern generated
bias <- c(pois$iso - (pi*pois$r**2))


# Generate point patterns and iterate the current bias at each r
for (i in 1:99999) {
  pois <- Kest(frac_point_pat(6, "snowflake", 74.6), correction="isotropic")
  bias <- bias + (pois$iso - pi*pois$r**2)
  print(i)
}

# Plot the bias against the radius
plot(pois$r,
     bias/100000,
     type="l",
     xlab="Radius",
     ylab="Estimated Bias",
     col="blue"
     )


# Plot an example point pattern in a particular fractal, here is the
# Sierpinksi curve
plot(frac_point_pat(4, "sierpinski", 28),
     pch=19,
     main="",
     cols="red",
     col="lightblue1",
     lwd=1.5
     )

