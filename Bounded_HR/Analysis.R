






rm(list = ls())


# Make sure all the files are in the same folder,
#and then change your working directory here

devtools::source_url("https://raw.githubusercontent.com/ColinC-dfo/DFO/master/Bounded_HR/HR_function.R")
devtools::source_url("https://raw.githubusercontent.com/ColinC-dfo/DFO/master/Bounded_HR/create_rast.R")
devtools::source_url("https://raw.githubusercontent.com/ColinC-dfo/DFO/master/Bounded_HR/estimate_sediments.R")
devtools::source_url("https://raw.githubusercontent.com/ColinC-dfo/DFO/master/Bounded_HR/makeCircles.R")


setwd("~/Charles/Nicole")


require(tidyverse)
require(sp)
require(rgdal)
require(adehabitatHR)
require(raster) #required for rast conversion & extract
require(ks) #required for kde
require(amap) #required for dist function
require(maptools) #required for checkpolygonholes function
require(spatstat)
require(latticeDensity)

# set the default ggplot theme to theme_bw() and center ggplot titles
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

# function to convert from tibble and look at first 6 rows
peek <- function(x){
  x %>% 
    head %>% 
    as.data.frame
}


# source("HR Function.R")


#### Read in bathymetry data for Lake Winnipeg

depth = readr::read_csv("Cluster5_Bathymetry_20191029.csv") %>% 
  dplyr::filter(BottomTypeStatus == "Valid") %>% 
  dplyr::select(Longitude_deg, Latitude_deg, Depth = BottomElevation_m, BottomType)

# convert the lat/long to UTM (columns need to be "X" and "Y")
d2 = depth %>% 
  dplyr::select(X = Longitude_deg, Y = Latitude_deg)

# Set the projection attribute to "LL".
attr(d2,"projection") = "LL"
# convUL will automatically detect what UTM zone you are in based on the coordinates
d2 = PBSmapping::convUL(d2, km = F)


# bund the UTM colmns back onto the depth data frame
depth = bind_cols(depth, d2)

peek(depth)



# Read in the Lake Winnipeg Data shoreline and island polygon data using read_csv()
LWpg = readr::read_csv("LakeWpgOutline.csv") %>% 
  dplyr::select(Long, Lat)

LWpg2 = LWpg
# The convUL() function requires that Lon and Lat actually be named "X" and "Y"
LWpg2$X = LWpg2$Long
LWpg2$Y = LWpg2$Lat
# Set the projection attribute to "LL".
attr(LWpg2,"projection") = "LL"
# convUL will automatically detect what UTM zone you are in based on the coordinates
LWpg2 = PBSmapping::convUL(LWpg2, km = F)


black2 = readr::read_csv("Black Isle.csv") %>% 
  rename(X = x,
         Y = y)
# Set the projection attribute to "LL".
attr(black2,"projection") = "LL"
# convUL will automatically detect what UTM zone you are in based on the coordinates
black2 = PBSmapping::convUL(black2, km = F)

reindeer2 = readr::read_csv("Reindeer Isle.csv")  %>% 
  rename(X = x,
         Y = y)
# Set the projection attribute to "LL".
attr(reindeer2,"projection") = "LL"
# convUL will automatically detect what UTM zone you are in based on the coordinates
reindeer2 = PBSmapping::convUL(reindeer2, km = F)

com2 = readr::read_csv("Commissioner Isle.csv")  %>% 
  rename(X = x,
         Y = y)
# Set the projection attribute to "LL".
attr(com2,"projection") = "LL"
# convUL will automatically detect what UTM zone you are in based on the coordinates
com2 = PBSmapping::convUL(com2, km = F)

deer2 = readr::read_csv("Deer Isle.csv")  %>% 
  rename(X = x,
         Y = y)
# Set the projection attribute to "LL".
attr(deer2,"projection") = "LL"
# convUL will automatically detect what UTM zone you are in based on the coordinates
deer2 = PBSmapping::convUL(deer2, km = F)


# Read in islands CSVs
com = readr::read_csv("Commissioner Isle.csv") 
deer = readr::read_csv("Deer Isle.csv") 
reindeer = readr::read_csv("Reindeer Isle.csv") 
black = readr::read_csv("Black Isle.csv") 






# 
LWdata = read_sf('~/Charles/Nicole/Mapping/LW_Red.shp')
# as.data.frame()
ggplot() + geom_sf(data = LWdata) + coord_sf(ylim = c(5555000, 5600000))

# df = read_sf('~/Charles/Nicole/Mapping/LW_Red.shp') %>%
#   as.data.frame()

df <- as.data.frame(LWdata$geometry[[1]][[1]])
names(df) = c("X","Y")
# convert from your current projection to Lat/Long in WGS84
LWpg = SpatialPoints(df[,c("X","Y")],
proj4string = CRS("+proj=utm +zone=14 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
# epsg:4326 is shorthand for the WGS84 conversion (can't remember where I found this. Somewhere online)
LWpg = spTransform(LWpg, CRS("+init=epsg:4326"))
LWpg = as.data.frame(LWpg)
names(LWpg) = c("x", "y")

LWpg <- map_df(LWpg, rev)
# paste your converted WGS84 data to your original data
# data = cbind(data, xx)
LWpg2 = LWpg
# The convUL() function requires that Lon and Lat actually be named "X" and "Y"
LWpg2$X = LWpg2$x
LWpg2$Y = LWpg2$y
# Set the projection attribute to "LL".
attr(LWpg2,"projection") = "LL"
# convUL will automatically detect what UTM zone you are in based on the coordinates
LWpg2 = PBSmapping::convUL(LWpg2, km = F)

xy.sp <- sp::SpatialPolygons(list(
  sp::Polygons(list(sp::Polygon(LWpg, hole = FALSE)),"1"),
  sp::Polygons(list(sp::Polygon(com, hole = TRUE)),"2"),
  sp::Polygons(list(sp::Polygon(deer, hole = TRUE)),"3"),
  sp::Polygons(list(sp::Polygon(reindeer, hole = TRUE)),"4"),
  sp::Polygons(list(sp::Polygon(black, hole = TRUE)),"5")
))
# see str(xy.sp) for the interal structure

sf_poly <- as(xy.sp, "sf")
sf::st_crs(sf_poly) <- 4326
sf_poly$id <- c(1, 2, 2, 2, 2)


ggplot(sf_poly) +
  geom_sf(aes(fill = factor(id))) +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "burlywood")) +
  ggtitle("Lake Winnipeg") +
  theme(legend.position = "none")


# 52.6 degrees (roughly) is the northern extent of the array. I used this to convert the lat to
# UTM to remove all grid points in the north basin where we have no data
# yy = data.frame(X = -97, Y = 52.6)
# # Set the projection attribute to "LL".
# attr(yy,"projection") = "LL"
# # convUL will automatically detect what UTM zone you are in based on the coordinates
# PBSmapping::convUL(yy, km = F)

sediments = readr::read_csv("~/Tyana/Cluster5_Bathymetry_20191029.csv") %>% 
  dplyr::filter(BottomTypeStatus == "Valid") %>% 
  dplyr::select(Long = Longitude_deg, Lat = Latitude_deg, 
                Depth = BottomElevation_m, BottomType)


rast1 <- create_rast()
# takes a minute or 2
sed <- estimate_sediments(sediments, radius = 3)


# Read in data from Nicole (daily positions)

migrantCOAlocs <- readr::read_csv("migrantCOAlocs.csv")

##
migrantCOAlocs$Lon.utm <- migrantCOAlocs$Lon.utm + runif(nrow(migrantCOAlocs), -500, 500)
migrantCOAlocs$Lat.utm <- migrantCOAlocs$Lat.utm + runif(nrow(migrantCOAlocs), -500, 500)

# migfall2017test <- migrantCOAlocs



dat <- migrantCOAlocs %>% 
  as.data.frame %>% 
  dplyr::filter(Tag.ID  == "120") %>% 
  mutate(date = as.POSIXct(TimeStep.coa, format = "%Y-%m-%d %H:%M")) %>% 
  group_by(Tag.ID, date = cut(date, breaks = "1 hour")) %>% 
  summarize(Lon.utm = mean(Lon.utm),
            Lat.utm = mean(Lat.utm)) %>% 
  mutate(date = as.POSIXct(as.character(date, format = "%Y-%m-%d %T"))) %>% 
  # dplyr::filter(date < as.POSIXct("2017-09-15")) %>% 
  mutate(TS = cut(date, breaks = "2 month")) %>% 
  dplyr::select(date, TS, Tag.ID, Lon.utm, Lat.utm) %>% 
  mutate(ID = paste0(Tag.ID, "_", TS)) %>% 
  dplyr::filter(ID == "120_2018-08-01")


ggplot(dat, aes(Lon.utm, Lat.utm)) + geom_point(col = 2) + 
  geom_polygon(data = LWpg2, aes(X,Y), fill = NA, col = 1)

# sed_type = data.frame(Type = paste("Type", 1:5),
#                       Val = c(2,1,4,3,5),
#                       Des = c("Silt","Clay-Fine","Gravel","Sand","Hard Bottom"),
#                       stringsAsFactors = FALSE)
rast1 <- create_rast()
# sed <- estimate_sediments(sediments)

HR <- bound_HR(dat, boundary = LWpg2,  smoother = "default",
               # smoother = cbind(c(0.001,0), c(0,0.1)),
               sediment = sed, rast1 = create_rast(), sed_type = "Type 4",
               SEDIMENT = T, NORTHING = TRUE, DEPTH = TRUE)


## plot of phre list objects
## zoomed in on polygons
plot(HR$Poly); plot(HR$Polygon, col = '#FF000028', add = T);
# plot(HR$array, add=T); 
points(HR$locs, pch = 20, cex = 1, col = "black")
polygon(LWpg2$X, LWpg2$Y, col = 'transparent', lwd = 2)

HR$Area
HR$smoother

