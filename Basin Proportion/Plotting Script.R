rm(list = ls())

require(tidyverse)
require(DBI)
require(sf)
require(sp)
require(PBSmapping)
require(ggthemes)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)



manitoba = readr::read_csv("~/Charles/Mapping/Manitoba.csv")
manitoba = sp::SpatialPoints(manitoba[,c("Lon","Lat")], proj4string = sp::CRS("+init=epsg:4326"))
manitoba = as.data.frame(manitoba@coords)


setwd("~/Charles/Rivers")
files = list.files(pattern = "*.csv")
rivs = NULL
for(i in 1:length(files)){
  if(files[i] %in% c("Proposed Red River 2017 - 30km spacing.csv",
                     "Proposed Red River 2017 - US Portion.csv",
                     "Sheyenne River (Partial).csv",
                     "Red Lake River (Partial).csv",
                     "Pembina River (Partial).csv")) next
  df1 = readr::read_csv(files[i])
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


p = ggplot() +
  geom_path(data = rivs, aes(Lon, Lat, group = part), col = '#9bbff4') +
  geom_text(data = Fargo, aes(X - 0.18, Y, label = "Fargo"), size = rel(2)) +
  geom_text(data = GrandForks, aes(X - 0.18, Y, label = "Grand Forks"), size = rel(2)) +
  geom_text(data = Winnipeg, aes(X + 0.18, Y, label = "Winnipeg"), size = rel(2))
  # geom_text(data = Fargo, aes(X + 1.23, Y + 2.75, label = "Fargo"), size = rel(2)) +
  # geom_text(data = GrandForks, aes(X + 1.33, Y + 2.75, label = "Grand Forks"), size = rel(2)) +
  # geom_text(data = Winnipeg, aes(X + 0.18, Y, label = "Winnipeg"), size = rel(2))




# setwd("~/Charles/Nicole")

# Read in the Lake Winnipeg Data shoreline and island polygon data using read_csv()
LWpg = readr::read_csv("~/Charles/Mapping/LakeWpgOutline.csv") %>% 
  dplyr::select(Long, Lat)

# LWpg2 = LWpg
# # The convUL() function requires that Lon and Lat actually be named "X" and "Y"
# LWpg2$X = LWpg2$Long
# LWpg2$Y = LWpg2$Lat
# # Set the projection attribute to "LL".
# attr(LWpg2,"projection") = "LL"
# # convUL will automatically detect what UTM zone you are in based on the coordinates
# LWpg2 = PBSmapping::convUL(LWpg2, km = F)
# 
# 
# black2 = readr::read_csv("~/Charles/Mapping/Black Isle.csv") %>% 
#   rename(X = x,
#          Y = y)
# # Set the projection attribute to "LL".
# attr(black2,"projection") = "LL"
# # convUL will automatically detect what UTM zone you are in based on the coordinates
# black2 = PBSmapping::convUL(black2, km = F)
# 
# reindeer2 = readr::read_csv("~/Charles/Mapping/Reindeer Isle.csv")  %>% 
#   rename(X = x,
#          Y = y)
# # Set the projection attribute to "LL".
# attr(reindeer2,"projection") = "LL"
# # convUL will automatically detect what UTM zone you are in based on the coordinates
# reindeer2 = PBSmapping::convUL(reindeer2, km = F)
# 
# com2 = readr::read_csv("~/Charles/Mapping/Commissioner Isle.csv")  %>% 
#   rename(X = x,
#          Y = y)
# # Set the projection attribute to "LL".
# attr(com2,"projection") = "LL"
# # convUL will automatically detect what UTM zone you are in based on the coordinates
# com2 = PBSmapping::convUL(com2, km = F)
# 
# deer2 = readr::read_csv("~/Charles/Mapping/Deer Isle.csv")  %>% 
#   rename(X = x,
#          Y = y)
# # Set the projection attribute to "LL".
# attr(deer2,"projection") = "LL"
# # convUL will automatically detect what UTM zone you are in based on the coordinates
# deer2 = PBSmapping::convUL(deer2, km = F)

# Read in islands CSVs
com = readr::read_csv("~/Charles/Mapping/Commissioner Isle.csv") 
deer = readr::read_csv("~/Charles/Mapping/Deer Isle.csv") 
reindeer = readr::read_csv("~/Charles/Mapping/Reindeer Isle.csv") 
black = readr::read_csv("~/Charles/Mapping/Black Isle.csv") 

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


# basin_plot <- function(data){
#   
#   fishData = data %>% 
#     # head(n = 100) %>% 
#     # dplyr::filter(Species == levels$Species[j]) %>% 
#     # dplyr::filter(Tagging_Site == levels$Tagging_Site[j]) %>%
#     dplyr::filter(!Name %in% c("Traverse Bay 3", "Traverse Bay 2")) %>% 
#     mutate(Waterbody = ifelse(Waterbody == "Lake Winnipeg", "South Basin", Waterbody),
#            Waterbody = ifelse(Name == "Manigotagan River", "South Basin", Waterbody),
#            Waterbody = ifelse(Name == "LW 18A", "Winnipeg River", Waterbody),
#            Waterbody = ifelse(Name %in% c("Cooks Creek","Devils Creek"), "Red River", Waterbody),
#            Waterbody = ifelse(ReceiverLat > 51.154 & ReceiverLat < 51.765 | Name == "LW 55", "Channel", Waterbody),  
#            Waterbody = ifelse(ReceiverLat > 51.765 | Name %in% c("LW 74","LW 75"), "North Basin", Waterbody)) %>% 
#     mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
#     mutate(Month = lubridate::floor_date(Date, unit = "month")) %>%
#     group_by(Month, Waterbody) %>%
#     summarise(Detections = n(),
#               Tags = length(unique(FishID))) %>% 
#     left_join(plot_coords, by = "Waterbody")
#   
#   
#   nFish = data %>% 
#     # dplyr::filter(Species == levels$Species[j]) %>% 
#     # dplyr::filter(Tagging_Site == levels$Tagging_Site[j]) %>%
#     dplyr::filter(!Name %in% c("Traverse Bay 3", "Traverse Bay 2")) %>% 
#     mutate(Waterbody = ifelse(Waterbody == "Lake Winnipeg", "South Basin", Waterbody),
#            Waterbody = ifelse(Name == "Manigotagan River", "South Basin", Waterbody),
#            Waterbody = ifelse(Name == "LW 18A", "Winnipeg River", Waterbody),
#            Waterbody = ifelse(Name %in% c("Cooks Creek","Devils Creek"), "Red River", Waterbody),
#            Waterbody = ifelse(ReceiverLat > 51.154 & ReceiverLat < 51.765 | Name == "LW 55", "Channel", Waterbody),  
#            Waterbody = ifelse(ReceiverLat > 51.765 | Name %in% c("LW 74","LW 75"), "North Basin", Waterbody)) %>% 
#     mutate(Month = lubridate::floor_date(Date, unit = "month")) %>%
#     group_by(Month) %>%
#     summarize(N = length(unique(FishID)))
#   
#   MONTHS = unique(fishData$Month)
#   for(i in 1:length(MONTHS)){
#     dat = fishData %>% 
#       dplyr::filter(Month == MONTHS[i]) %>% 
#       mutate(Month1 = format(MONTHS[i], format = "%m %Y")) %>% 
#       mutate(Prop = Tags/sum(Tags))
#     
#     nFish1 = nFish %>%
#       dplyr::filter(Month == MONTHS[i])
#     
#     
#     Month1 = format(MONTHS[i], format = "%B %Y")
#     
#     p1 = p +
#       geom_point(data = dat, aes(x = Long, y = Lat, size = Prop, group = NULL),
#                  alpha = 0.99, col = cbbPalette[2]) +
#       geom_point(data = dat, aes(x = Long, y = Lat, size = Prop, group = NULL), shape = 1, colour = "black") + 
#       coord_sf(xlim = c(-99.4, -96), ylim = c(50, 54)) +
#       # ggtitle('Walleye Detections Apr-May') +
#       facet_wrap(~Month1) + 
#       geom_text(data = nFish1, aes(-97, 53.8, label = paste("# Fish Detected\n", N), group = NULL), 
#                 col = "white", size = rel(9)) + 
#       scale_size_continuous(range = c(2, 15), breaks = seq(0.2, 1, 0.2),
#                             limits = c(0, 1), labels = c(paste0(seq(20,100, 20), "%"))) +
#       geom_text(data = dat, aes(txtLong, txtLat, group = NULL, vjust = 1, col = col,
#                                 label = paste0(round(100*Prop,1), "%")), fontface = "bold", size = rel(5)) +
#       scale_colour_manual(values = c("black" = "black", "white" = "white")) +
#       guides(size = guide_legend(title = "Proportion\nof Tags"), colour = "none") +
#       theme(plot.title = element_text(hjust = 0.5, size = rel(5), colour = "white"),
#             legend.title = element_text(size = 18),
#             legend.text = element_text(size = 17),
#             strip.text = element_text(size = rel(3)),
#             plot.background = element_rect(fill = '#242f3e', colour = '#242f3e')) 
#   }
#   return(p1)
# }
  

basin_plot <- function(data, fileLoc = NULL){
  
  if(is.null(fileLoc)){
    cat("You need to specify a file location to save the plot")
    stop()
  } 
  
  fishData = data %>% 
    group_by(year, month, Waterbody) %>%
    summarize(Detections = n(),
              Tags = n_distinct(FishID)) %>% 
    collect %>% 
    left_join(plot_coords, by = "Waterbody")
  
  
  nFish = data %>% 
    group_by(year, month) %>%
    summarize(N = n_distinct(FishID)) %>% 
    collect 
  
  fishData$Month = paste(fishData$month, fishData$year)
  nFish$Month = paste(nFish$month, nFish$year)
  MONTHS = unique(paste(fishData$month, fishData$year))

  dat = fishData %>% 
    mutate(Month1 = format(as.Date(paste("1", Month), 
                                   format = "%d %m %Y"), format = "%B %Y")) %>% 
    mutate(Prop = Tags/sum(Tags))
  
  # Species <- x %>% group_by(Species) %>% summarize %>% pull
  Month1 <- format(as.Date(paste("1", MONTHS), format = "%d %m %Y"), format = "%B %Y")
  
  p1 = p +
    geom_point(data = dat, aes(x = Long, y = Lat, size = Prop, group = NULL),
               alpha = 0.99, col = cbbPalette[2]) +
    geom_point(data = dat, aes(x = Long, y = Lat, size = Prop, group = NULL), shape = 1, colour = "black") + 
    coord_sf(xlim = c(-99.4, -96), ylim = c(50, 54)) +
    # ggtitle('Walleye Detections Apr-May') +
    facet_wrap(~Month1) + 
    geom_text(data = nFish, aes(-97, 53.8, label = paste("# Fish Detected\n", N), group = NULL), 
              col = "white", size = rel(9)) + 
    scale_size_continuous(range = c(2, 15), breaks = seq(0.2, 1, 0.2),
                          limits = c(0, 1), labels = c(paste0(seq(20,100, 20), "%"))) +
    geom_text(data = dat, aes(txtLong, txtLat, group = NULL, vjust = 1, col = col,
                              label = paste0(round(100*Prop,1), "%")), fontface = "bold", size = rel(5)) +
    scale_colour_manual(values = c("black" = "black", "white" = "white")) +
    guides(size = guide_legend(title = "Proportion\nof Tags"), colour = "none", fill = "none") +
    theme_map() +
    theme(plot.title = element_text(hjust = 0.5, size = rel(5), colour = "white"),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 17),
          strip.text = element_text(size = rel(3)),
          plot.background = element_rect(fill = '#242f3e', colour = '#242f3e'))
  
  ggsave(filename = paste0(fileLoc, "/", Month1, ".tiff"), plot = p1,
         compression = "lzw", width = 8, height = 13, dpi = 600)
  # return(p1)
}


plot_coords = data.frame(Waterbody = c("North Basin", "Channel","South Basin","Winnipeg River","Red River"),
                         Long = c(-97.439, -96.676, -96.73, -96.294, -96.85),
                         Lat = c(52.245, 51.483, 50.878, 50.612, 50.279),
                         txtLong = c(-97.439, -96.676, -96.73, -96.20, -96.67),
                         txtLat = c(52.45, 51.495, 50.878, 50.612, 50.279) - 0.07,
                         col = c("black","black","black","white","white"))

cbbPalette = c("#FFFFFF", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7")
