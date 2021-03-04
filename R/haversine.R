# The haversine formula determines the great-circle distance between two points
# on a sphere given their longitudes and latitudes (Sinnot, 1984).

haversine = function(lon1, lat1, lon2, lat2){
  # require(pracma)
  R = 6371000 # meters
  deltaLat = pracma::deg2rad(lat2 - lat1)
  deltaLon = pracma::deg2rad(lon2 - lon1)
  lat1 = pracma::deg2rad(lat1); lon1 = pracma::deg2rad(lon1)
  lat2 = pracma::deg2rad(lat2); lon2 = pracma::deg2rad(lon2)
  a = sin(deltaLat/2) * sin(deltaLat/2) + cos(lat1) * cos(lat2) * sin(deltaLon/2) * sin(deltaLon/2)
  c = 2 * atan2(sqrt(a), sqrt(1-a))
  d = R * c
  return(d)
}


# Winnipeg
lat1 <- 49.885
lon1 <- -97.126

# Gimli
lat2 <- 50.631
lon2 <- -96.985

# Distance from Winnipeg (The Forks) to Gimli in meters
haversine(lon1, lat1, lon2, lat2)

# Distance from Winnipeg (The Forks) to Gimli in kilometers
haversine(lon1, lat1, lon2, lat2)/1000
