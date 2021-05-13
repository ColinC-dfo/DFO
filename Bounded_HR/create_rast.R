

create_rast = function(boundary = LWpg2, node_spacing = 5000,
                       holes = list(black2, reindeer2, deer2, com2)){
  
  bound = as.matrix(boundary[,c("X","Y")])
  
  nodeFillingOutput <- nodeFill(poly = bound,
                                node_spacing = node_spacing,
                                hole_list = holes)
  rast1 <- nodeFillingOutput$nodes
  
  rast1 <- rast1 %>% 
    as.data.frame() %>% 
    dplyr::filter(y <= 5829654) %>% 
    as.matrix
  
  p <- ppp(rast1[,1], rast1[,2], poly = list(x = boundary$X, y = boundary$Y))
  # You need to convert your shp data to spatstat segment pattern object. 
  # To do so, you can load the shp file with commands from
  # maptools and than convert into a spatstat object:
  
  # To calculate your nearest neighbour distance, you have to use nncross
  Sl1 = Line(boundary[,c("X","Y")])
  S1 = Lines(list(Sl1), ID="boundary")
  Sl = SpatialLines(list(S1))
  shp = as.psp(Sl) 
  
  rast1 <- cbind(rast1, nncross(p, shp)/1000)
  rast1 <- rast1[,c(1,2,3)]
  
  rast1 <- rast1 %>% 
    as.data.frame() %>% 
    as.matrix()
  
  return(rast1)
}
