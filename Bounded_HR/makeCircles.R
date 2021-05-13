

# Create a function to draw circles of varying size around a central point

make_circles <- function(centers, radius, nPoints = 100){
  # centers: the data frame of centers with ID
  # radius: radius measured in kilometers
  #
  meanLat <- mean(centers$Lat)
  # length per longitude changes with lattitude, so need correction
  radiusLon <- radius /111 / cos(meanLat/57.3) 
  radiusLat <- radius / 111
  circleDF <- data.frame(ID = rep(centers$ID, each = nPoints), Name = rep(centers$Name, each = nPoints))
  angle <- seq(0,2*pi,length.out = nPoints)
  
  circleDF$lon <- unlist(lapply(centers$Long, function(x) x + radiusLon * cos(angle)))
  circleDF$lat <- unlist(lapply(centers$Lat, function(x) x + radiusLat * sin(angle)))
  return(circleDF)
}
