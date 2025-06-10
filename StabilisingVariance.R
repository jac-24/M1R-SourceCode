# Compares sample variance of Kest with Lest

library(spatstat)

# Generates a homogeneous Poisson point pattern in a square/rectangle
# with a given intensity
hpp_by_intensity_2D <- function(x_min, x_max, y_min, y_max, lambda){
  # Number of events
  num_events <- rpois(1, lambda*(x_max-x_min)*(y_max-y_min))
  
  # Simulates coordinates of events
  event_times_x <- runif(num_events, min=x_min, max=x_max)
  event_times_y <- runif(num_events, min=y_min, max=y_max)
  
  window <- owin(xrange=c(x_min, x_max), yrange=c(y_min, y_max))
  
  points_ppp <- ppp(x=event_times_x, y=event_times_y, window=window)
  
  # Calculates sample variance using envelopes
  # Can choose either Lest or Kest depending on which variance is being plotted
  env <- envelope(points_ppp, Lest, funargs=list("isotropic"), rmax=20, nsim = 100, VARIANCE=T)
  
  # Plots the sample variance of this first envelope
  plot(env$r,
       env$var,
       col="blue",
       type="l",
       xlab="Radius",
       ylab="Sample Variance",
       main=""
  )
  
  # Repeats the above 39 more times, plotting the sample variance each time
  for (i in 1:39) {
    more_env <- envelope(points_ppp, Lest, rmax=20, nsim=100, VARIANCE=T)
    lines(more_env$r,
          more_env$var,
          col="blue"
          )
    
    
  }

}




hpp_by_intensity_2D(0, 100, 0, 100, 0.5)
