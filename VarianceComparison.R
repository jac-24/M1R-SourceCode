# Comparing theoretical variance to sample variance

library(spatstat)

# Generates a homogeneous point pattern in a square/rectangle with
# given intensity
hpp_by_intensity_2D <- function(x_min, x_max, y_min, y_max, lambda){
  num_events <- rpois(1, lambda*(x_max-x_min)*(y_max-y_min))
  
  event_times_x <- runif(num_events, min=x_min, max=x_max)
  event_times_y <- runif(num_events, min=y_min, max=y_max)
  
  window <- owin(xrange=c(x_min, x_max), yrange=c(y_min, y_max))
  
  points_ppp <- ppp(x=event_times_x, y=event_times_y, window=window)

  # Calculates theoretical variance using Ripley's formula
  rip_k <- Kest(points_ppp, correction="isotropic", var.approx=T)
  # Plots the theoretical variance for isotropic correction proposed by Ripley
  plot(rip_k$r,
       rip_k$rip,
       type="l",
       xlab="Radius",
       ylab="Variance",
       main="",
       col="red",
       lwd=4
       )

  for (i in 1:40) {
    # Calculates sample variance using envelopes
    env <- envelope(points_ppp, Kest, nsim = 100, VARIANCE=T)
    # Plots this sample variance
    lines(env$r, env$var, type="l", col="blue")
  }
  
  # Plots theoretical variance as mentioned above
  lines(rip_k$r, rip_k$rip, col="red", lwd=4)
  
  # Creates the legend at the top of the graph
  legend("topleft",
         legend=c("Theoretical Variance", "Sample Variances"),
         fill=c("red", "blue")
  )
}

hpp_by_intensity_2D(0, 100, 0, 100, 5)

