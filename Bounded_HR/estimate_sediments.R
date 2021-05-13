

estimate_sediments <- function(sediments, rast = rast1, radius = 5){

  ####
  #
  ## Make 0.5 km radius circles around each station
  #
  ####
  xx = SpatialPoints(rast[,c(1:2)],
                     proj4string = CRS("+proj=utm +zone=14 +datum=NAD83 +units=m
                    +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  # coordinates(xx) <- c("x", "y")
  xx = spTransform(xx, CRS("+init=epsg:4326"))
  xx = as.data.frame(xx)
  names(xx) = c("Long", "Lat")
  
  xx$Name = 1:nrow(xx)
  xx$ID = 1:nrow(xx)
  # convUL will automatically detect what UTM zone you are in based on the coordinates
  
  ## Make 5 km radius circles around each station
  myCircles = make_circles(xx, radius)
  
  IDs = as.character(unique(myCircles$Name))
  One_KM = NULL
  for(i in IDs){
    dat = myCircles %>% 
      dplyr::filter(Name == i)
    
    # radius around a station
    H = list(x = dat$lon, y = dat$lat)
    
    # extract points within radius of station
    inp = GEOmap::inpoly(sediments$Long, sediments$Lat, POK = H)
    
    dat1 = sediments %>% 
      dplyr::filter(inp == 1) %>% 
      group_by(BottomType) %>% 
      tally() %>% 
      mutate(PROP = n/sum(n)) %>% 
      dplyr::select(-n) %>% 
      mutate(Name = as.character(unique(dat$Name)))
    
    # if(nrow(dat1) == 0){
    #   dat1 <- data.frame(BottomType = 0, PROP = 1, Name = i)
    # }
    
    One_KM = rbind(One_KM, dat1)
    cat(i)
  }
  rm(i, H, dat, dat1, inp, IDs)
  
  sub_type = One_KM %>%
    mutate(BottomType = paste("Type", BottomType)) %>% 
    tidyr::pivot_wider(names_from = BottomType, values_from = PROP,
                       values_fill = list(PROP = 0))
  
  # sed_type = data.frame(Type = paste("Type", 1:5),
  #                       # Val = c(2,1,4,3,5),
  #                       Des = c("Silt","Clay-Fine","Gravel","Sand","Hard Bottom"),
  #                       stringsAsFactors = FALSE)
  sed <- xx %>% 
    mutate(Name = as.character(Name)) %>% 
    left_join(sub_type)
  
  return(sed)
}
