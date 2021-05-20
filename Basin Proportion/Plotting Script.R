
rm(list = ls())


require(tidyverse)
require(DBI)
require(sf)
require(sp)
require(PBSmapping)
require(ggthemes)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)


manitoba <- readr::read_csv("https://raw.githubusercontent.com/ColinC-dfo/DFO/master/files/manitoba.csv")
manitoba <- sp::SpatialPoints(manitoba[,c("Lon","Lat")], proj4string = sp::CRS("+init=epsg:4326"))
manitoba <- as.data.frame(manitoba@coords)


# create 
fileNames = c("assiniboine.csv", "cooks.csv", "cross.csv", "devils.csv", "east%20channel.csv",
              "far%20west%20channel.csv", "la%20salle.csv", "main%20channel.csv", "red%20river_canada.csv",
              "red%20river_us%20border%20to%20fargo.csv", "straight%20creek.csv", "west%20channel.csv", "winnipeg%20river.csv")
rivs <- NULL
for(i in 1:length(fileNames)){
  df1 = readr::read_csv(paste0("https://raw.githubusercontent.com/ColinC-dfo/DFO/master/files/", fileNames[i]))
  df1$part = i
  rivs = rbind(rivs,df1)
}

dataLL = data.frame(X = rivs$Lon,
                    Y = rivs$Lat)
attr(dataLL,"projection") = "LL"
rivs = cbind(rivs, convUL(dataLL, km = F))

Fargo <- data.frame(X = -96.789803, Y = 46.877186)
GrandForks <- data.frame(X = -97.032852, Y = 47.925259)
Winnipeg <- data.frame(X = -97.138451, Y = 49.895077)


ggplot() +
  geom_path(data = rivs, aes(Lon, Lat, group = part), col = '#9bbff4') +
  geom_text(data = Fargo, aes(X - 0.18, Y, label = "Fargo"), size = rel(2)) +
  geom_text(data = GrandForks, aes(X - 0.18, Y, label = "Grand Forks"), size = rel(2)) +
  geom_text(data = Winnipeg, aes(X + 0.18, Y, label = "Winnipeg"), size = rel(2))
# geom_text(data = Fargo, aes(X + 1.23, Y + 2.75, label = "Fargo"), size = rel(2)) +
# geom_text(data = GrandForks, aes(X + 1.33, Y + 2.75, label = "Grand Forks"), size = rel(2)) +
# geom_text(data = Winnipeg, aes(X + 0.18, Y, label = "Winnipeg"), size = rel(2))




# setwd("~/Charles/Nicole")

# Read in the Lake Winnipeg Data shoreline and island polygon data using read_csv()
LWpg <- readr::read_csv("https://raw.githubusercontent.com/ColinC-dfo/DFO/master/files/LakeWpgOutline.csv") %>% 
  dplyr::select(Long, Lat)


# Read in islands CSVs
com = readr::read_csv("https://raw.githubusercontent.com/ColinC-dfo/DFO/master/files/commissioner.csv") 
deer = readr::read_csv("https://raw.githubusercontent.com/ColinC-dfo/DFO/master/files/deer.csv") 
reindeer = readr::read_csv("https://raw.githubusercontent.com/ColinC-dfo/DFO/master/files/reindeer.csv") 
black = readr::read_csv("https://raw.githubusercontent.com/ColinC-dfo/DFO/master/files/black%20isle.csv") 

xy.sp <- sp::SpatialPolygons(list(
  sp::Polygons(list(sp::Polygon(LWpg)),"1"),
  sp::Polygons(list(sp::Polygon(com, hole = TRUE)),"2"),
  sp::Polygons(list(sp::Polygon(deer, hole = TRUE)),"3"),
  sp::Polygons(list(sp::Polygon(reindeer, hole = TRUE)),"4"),
  sp::Polygons(list(sp::Polygon(black, hole = TRUE)),"5")
))
# see str(xy.sp) for the interal structure

sf_poly <- as(xy.sp, "sf")
sf::st_crs(sf_poly) <- 4326
sf_poly$id <- c(1, 2, 2, 2, 2)

# geom_polygon(data = isles, aes(Long, Lat, group = part), fill = "#242f3e") +

ggplot() +
  geom_sf(data = sf_poly, aes(fill = factor(id))) +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "#242f3e")) +
  ggtitle("Lake Winnipeg") +
  # theme(legend.position = "none") + 
  geom_path(data = rivs, aes(Lon, Lat, group = part), col = '#9bbff4') +
  geom_text(data = Fargo, aes(X - 0.5, Y, label = "Fargo"), size = rel(2), col = "white") +
  geom_text(data = GrandForks, aes(X - 0.5, Y, label = "Grand Forks"), size = rel(2), col = "white") +
  geom_text(data = Winnipeg, aes(X + 0.4, Y, label = "Winnipeg"), size = rel(2), col = "white") + 
  guides(fill = FALSE)


p = ggplot() +
  geom_sf(data = sf_poly, aes(fill = factor(id))) +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "#242f3e")) +
  geom_path(data = rivs, aes(Lon, Lat, group = part), col = '#9bbff4') +
  geom_text(data = Fargo, aes(X - 0.18, Y, label = "Fargo"), size = rel(2), col = "white") +
  geom_text(data = GrandForks, aes(X - 0.18, Y, label = "Grand Forks"), size = rel(2), col = "white") +
  geom_text(data = Winnipeg, aes(X + 0.18, Y, label = "Winnipeg"), size = rel(2), col = "white") + 
  guides(fill = FALSE)



# import the plotting function to plot the detectons by basin

devtools::source_url("https://raw.githubusercontent.com/ColinC-dfo/DFO/master/R/basin_plot.R")



plot_coords = data.frame(Waterbody = c("North Basin", "Channel","South Basin","Winnipeg River","Red River"),
                         Long = c(-97.439, -96.676, -96.73, -96.294, -96.85),
                         Lat = c(52.245, 51.483, 50.878, 50.612, 50.279),
                         txtLong = c(-97.439, -96.676, -96.73, -96.20, -96.67),
                         txtLat = c(52.45, 51.495, 50.878, 50.612, 50.279) - 0.07,
                         col = c("black","black","black","white","white"))

cbbPalette = c("#FFFFFF", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7")
