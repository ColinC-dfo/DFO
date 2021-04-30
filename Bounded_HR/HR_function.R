


nodeFill <- function (poly, node_spacing, hole_list = NULL){
  
  poly <- as.matrix(poly)
  if (!is.null(hole_list)) {
    number.holes <- length(hole_list)
    for (k in 1:number.holes) {
      hole_list[[k]] <- as.matrix(hole_list[[k]])
    }
    # print("This function does not check to see if the holes")
    # print("are nonintersecting, or whether they are contained")
    # print("inside the boundary")
  }
  node_spacing <- as.numeric(node_spacing)
  EW_locs <- seq(min(poly[, 1]) + node_spacing/2, max(poly[, 
                                                           1]) - node_spacing/2, by = node_spacing)
  NS_locs <- seq(min(poly[, 2]) + node_spacing/2, max(poly[, 
                                                           2]) - node_spacing/2, by = node_spacing)
  bound_array <- expand.grid(EW_locs, NS_locs)
  names(bound_array) <- c("x", "y")
  nodes <- bound_array[splancs::inout(pts = bound_array, poly = poly, 
                                      bound = TRUE), ]
  nodes <- splancs::as.points(as.matrix(nodes))
  output <- list(EW_locs = EW_locs, NS_locs = NS_locs, nodes = nodes, 
                 poly = poly, node_spacing = node_spacing, hole_list = hole_list)
  class(output) <- "nodeFillingOutput"
  if (!is.null(hole_list)) {
    for (k in 1:number.holes) {
      output <- removeHole(hole_list[[k]], output)
    }
  }
  return(output)
}



bound_HR <- function(data, size = 50, smoother = "default", boundary = NULL,
                     holes = list(black2, reindeer2, deer2, com2), percent = 95,
                     northInd = 10000, bathymetry = depth){
  
  
  locs <- cbind(data$Lon.utm, data$Lat.utm)
  
  ##define dimensions for area where the kd will be calculated
  # x.range <- max(locs[,1]) - min(locs[,1])
  # y.range <- max(locs[,2]) - min(locs[,2])
  min.x <- min(locs[,1]) - 5000
  max.x <- max(locs[,1]) + 5000
  min.y <- min(locs[,2]) - 5000
  max.y <- max(locs[,2]) + 5000
  ##create coordinates for grid array (used to back-translate kde estimate)
  size = size
  seq.x <- seq(min.x, max.x, (max.x-min.x)/size)
  seq.y <- seq(min.y, max.y, (max.x-min.x)/size)
  array<-dim(0)
  for (a in 1:length(seq.x)){
    array <- rbind(array, cbind(rep(seq.x[a],length(seq.y)), seq.y))
  }
  # plot(array)
  
  if(is.null(boundary)){
    cat("Please supply a boundary (shoreline) for your project area")
    stop()
  }
  
  array <- as.data.frame(array) 
  names(array) = c("X","Y")
  inp = GEOmap::inpoly(array$X, array$Y, POK = list(x = boundary$X, y = boundary$Y))
  array <- array[inp == 1,]
  
  # array %>% 
  #   # head %>% 
  #   ggplot(data = .) + 
  #   geom_point(aes(X, Y)) + 
  #   geom_polygon(data = LWpg2, aes(X, Y), col = 1, fill = NA) +
  #   geom_point(data = as.data.frame(locs), aes(V1, V2), col = 2)
  
  
  bound = as.matrix(boundary[,c("X","Y")])
  
  nodeFillingOutput <- nodeFill(poly = bound,
                                node_spacing = 5000,
                                hole_list = holes)
  rast1 <- nodeFillingOutput$nodes
  
  p = ppp(rast1[,1], rast1[,2], poly = list(x = boundary$X, y = boundary$Y))
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
  
  # DO NOT FUCKING DELETE THIS COLIN
  lakeDist <- data.frame(Y = seq(min(boundary$Y), max(boundary$Y), by = northInd))
  lakeDist$index = 1:nrow(lakeDist)
  # Interpolation on the raster northing UTM coordinates
  index <- approx(lakeDist$Y, lakeDist$index, xout = rast1[,2], method = "linear")$y
  rast1 <- cbind(rast1, index)
  # Interpolation on the fish positions UTM coordinates
  # lakeDist$ind <- approx(lakeDist$Y, lakeDist$index, xout = locs[,2], method = "linear")$y
  
  ## bound is just a copy of our lake Winnipeg shoreline polygon with coordinates in UTM
  # make bound a data frame and then convert the "depth" of the shoreline to zero
  bound <- bound %>% 
    as.data.frame() %>% 
    mutate(Depth = 0)
  
  # randomly select 5000 data pointd from the bathymetry dataframe to make this run more
  # quickly (I wouldn't go much more than 10k points for efficiency)
  d2 <- bathymetry %>% 
    distinct() %>% 
    sample_n(5000) %>% 
    dplyr::select(X, Y, Depth) %>% 
    bind_rows(., bound)
  
  # do a check to see if any points fall outside the Lake Winnipeg boundary
  inp = GEOmap::inpoly(d2$X, d2$Y, POK = list(x = boundary$X, y = boundary$Y))
  
  # remove all points outside the lake
  d2 = d2[inp == 1,]
  
  # plot it
  # ggplot(d2, aes(X, Y, col = Depth)) + geom_point() + 
  #   geom_point(data = as.data.frame(rast1), aes(x, y), col = 1)
  
  # interpolate the depth of the lake across the lake
  res <- interp::interp(x = d2$X, y = d2$Y, z = d2$Depth,
                        xo = as.vector(rast1[,1]), yo = as.vector(rast1[,2]),
                        duplicate = "strip", output = "points")
  
  # make the interpolated data a dataframe and then plot to see the output
  res = data.frame(x = res$x, y = res$y, z = res$z)
  
  # NOTE: this is not correct for the north basin b/c we don't have any data 
  # up there for this exercise (but the home range should only be calculated for
  # areas that we have data)
  # ggplot(res, aes(x, y, col = z)) + geom_point()
  
  rast1 <- cbind(rast1, res$z)
  
  rasters<-list(0)
  # Interpolated Depth where fish was located (5th column)
  rasters[[1]] <- rasterFromXYZ(cbind(rast1[,1], rast1[,2], rast1[,5]))
  # This one is the indicator for northing across the lake (4th column in rast1)
  rasters[[2]] <- rasterFromXYZ(cbind(rast1[,1], rast1[,2], rast1[,4]))
  
  
  landscape<-dim(0) #empty matrix for landscape variables
  ##transform resight locations to landscape variables; also extract landscape variables to grid array
  for (r in 1:length(rasters)){ ##for each raster layer
    landscape <- cbind(landscape, raster::extract(rasters[[r]], 
                                                  locs[,1:2], na.rm = F,
                                                  method = 'bilinear')) ##calculate the raster values at locs
    array<-cbind(array, raster::extract(rasters[[r]], array[,1:2],
                                        na.rm = F, method = 'bilinear'))##add raster values to array
  }
  if (length(is.na(landscape[,1])) > length(landscape)/2) {
    print('Warning: Raster layers do not cover the extent of the location data')
  }
  
  # smoother = "default"
  # smoother <- cbind(c(0.5,0), c(0,0.2)) #(DEPTH, 0) (0, NORTHING)
  ##calculate kernel density values at each point in the grid array; kde function is generated using landscape variables of re-sight locations
  if (smoother[1] == 'default') {
    kd <- kde(x = na.omit(landscape), eval.points = array[,3:dim(array)[2]])
    # xmin = c(min.x, min.y), xmax = c(max.x, max.y))
  } else {
    kd <- kde(x = na.omit(landscape), H = smoother, eval.points = array[,3:dim(array)[2]])
  }
  smoother <- kd$H
  density.array <- kd$estimate
  
  ##transform density values to probability values
  z <- density.array/sum(density.array, na.rm = TRUE) ##make array sum to one
  array <- cbind(array, z) 
  ##make a matrix of array points that are within the 90% probability kernel
  array.order<-array[order(array[,dim(array)[2]], decreasing = TRUE),]
  
  if (exists('percent')) {
    percent <- percent/100
  } else {
    percent <- 0.95
  }
  
  for (i in 1:length(array.order[,dim(array)[2]])) {
    if (sum(array.order[1:i,dim(array)[2]]) >= percent) {
      HRpoints <- array.order[1:i,]
      critval <- array.order[i,dim(array)[2]]
      break
    }
  }
  
  ##create a raster layer of the grid array with probability values
  HR.grid <- rasterFromXYZ(cbind(array[,1:2],array[,dim(array)[2]]))
  ##create a raster layer of the grid array points within the 90% kernel
  HR.rast <- rasterFromXYZ(cbind(HRpoints[,1:2],rep(1,dim(HRpoints)[1])), res=abs(seq.y[1]-seq.y[2]))
  ##define the area of tiny fragment polygons (4 pixels or smaller)
  min.poly.size <- 4*((HR.rast@extent@xmax-HR.rast@extent@xmin)/HR.rast@ncols)^2
  ##convert HR.rast to polygons, which denote the permissible home range
  HR.poly <- rasterToPolygons(HR.rast, n = 16, na.rm = T, digits = 4, dissolve = T)
  
  ##remove polygons that are 4 pixels or smaller
  s<-"polyID"
  HR.polys<-list(0)
  pr <- 0
  area <- 0
  for (p in 1:length(HR.poly@polygons[[1]]@Polygons)) {
    if (HR.poly@polygons[[1]]@Polygons[[p]]@area > min.poly.size) {
      pr <- pr + 1
      HR.polys[[pr]] <- HR.poly@polygons[[1]]@Polygons[[p]]
      area <- area + HR.poly@polygons[[1]]@Polygons[[p]]@area/1000^2
      # area <- area + area(as.data.frame(HR.poly@polygons[[1]]@Polygons[[p]]@coords))/1000^2
    }
  }
  
  
  HR.polys <- Polygons(HR.polys, ID = s) #convert to polygons object
  HR.polys <- checkPolygonsHoles(HR.polys) #assign holes correctly
  HR.polys <- SpatialPolygons(list(HR.polys)) #convert to spatial polygons
  HR.polys <- rgeos::gSimplify(HR.polys, tol = 20)
  
  HR <- list(Polygon = HR.polys, locs = locs,
             HRpoints = HRpoints, array = HR.grid, smoother = smoother, Area = area)
}
