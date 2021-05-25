

devtools::source_url("https://raw.githubusercontent.com/ColinC-dfo/DFO/master/Basin%20Proportion/Plotting%20Script.R")

setwd("~/2019 Telemetry")
con = dbConnect(RSQLite::SQLite(), "LWBT_2020.sqlite", extended_types = TRUE)

p <- ggplot() +
  geom_sf(data = sf_poly, aes(fill = factor(id))) +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "#242f3e")) +
  geom_path(data = rivs, aes(Lon, Lat, group = part), col = '#9bbff4') +
  geom_text(data = Fargo, aes(X - 0.18, Y, label = "Fargo"), size = rel(4), col = "white") +
  geom_text(data = GrandForks, aes(X - 0.18, Y, label = "Grand Forks"), size = rel(4), col = "white") +
  geom_text(data = Winnipeg, aes(X + 0.4, Y, label = "Winnipeg"), size = rel(7), col = "white") + 
  guides(fill = FALSE)


#################################


cbbPalette <- c("#FFFFFF", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7")

fishData <- con %>%
  tbl("rawData_fishTable") %>% 
  filter(!is.na(Species) | Species != "Sync Tag") %>%
  # mutate(Year = format(Date, format = "%Y")) %>% 
  group_by(Species, Name, year) %>%
  summarise(Detections = n(),
            Lat = mean(ReceiverLat),
            Long = mean(ReceiverLong),
            Tags = n_distinct(FishID)) %>% 
  collect()

nFish = con %>%
  tbl("rawData_fishTable") %>% 
  filter(!is.na(Species) | Species != "Sync Tag") %>%
  # mutate(Year = format(Date, format = "%Y")) %>% 
  group_by(Species, year) %>%
  # filter(ReceiverLat > 50.084) %>%
  summarize(N = n_distinct(FishID)) %>% 
  collect

# may need to update path directory depending on where the script is being run
path1 = "~/Charles/Lake Winnipeg Basin Movement Study/Fish Per Receiver"
path2 = "~/Charles/Lake Winnipeg Basin Movement Study/Detections Per Receiver"
for(i in 1:nrow(nFish)){
  
  if(!dir.exists(paste(path1, nFish$Species[i], sep ="/"))){
    dir.create(paste(path1, nFish$Species[i], sep ="/"), recursive = TRUE)
  }
  
  if(!dir.exists(paste(path2, nFish$Species[i], sep ="/"))){
    dir.create(paste(path2, nFish$Species[i], sep ="/"), recursive = TRUE)
  }
  
  xx <- fishData %>% 
    dplyr::filter(Species == nFish$Species[i] & year == nFish$year[i])
  
  yy <- nFish %>% 
    dplyr::filter(Species == nFish$Species[i] & year == nFish$year[i])
  
  p1 <- p +
    geom_point(data = xx, aes(x = Long, y = Lat, size = Tags, group = NULL),
               alpha = 0.99, col = cbbPalette[2]) +
    geom_point(data = xx, aes(x = Long, y = Lat, size = Tags, group = NULL),
               shape = 1, colour = "black") + 
    # coord_cartesian(xlim = c(-98.8, -95), ylim = c(50.1, 52.5)) +
    coord_sf(xlim = c(-99.4, -96), ylim = c(49, 52.6)) +
    ggtitle(paste0('Unique ', nFish$Species[i], ' Detected\nPer Receiver Lake Winnipeg Basin')) +
    facet_wrap(~year) +
    geom_text(data = yy, aes(-96.4, 52.5, label = paste("# Fish Detected\n", N),
                                group = NULL), col = "white", size = rel(8)) + 
    scale_size_continuous(range = c(1, 10), breaks = c(5, 10, 15, max(fishData$Tags))) +
    guides(size = guide_legend(title = "# Tags\nDetected")) +
    theme_map() +
    theme(plot.title = element_text(hjust = 0.5, size = rel(3), colour = "white"),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 17),
          strip.text = element_text(size = rel(3)),
          plot.background = element_rect(fill = '#242f3e', colour = '#242f3e'))
  
  ggsave(plot = p1, paste0(path1, "/", nFish$Species[i], "/Unique ", nFish$year[i],
                           " by Station.tiff"),
         compression = "lzw", width = 13, height = 15, dpi = 600)
  
  
  p2 <- p +
    geom_point(data = xx, aes(x = Long, y = Lat, size = Detections, group = NULL),
               alpha = 0.99, col = cbbPalette[2]) +
    geom_point(data = xx, aes(x = Long, y = Lat, size = Detections, group = NULL),
               shape = 1, colour = "black") + 
    # coord_cartesian(xlim = c(-98.8, -95), ylim = c(50.1, 52.5)) +
    coord_sf(xlim = c(-99.4, -96), ylim = c(49, 52.6)) +
    ggtitle(paste0('Unique ', nFish$Species[i], ' Detections\nPer Receiver Lake Winnipeg Basin')) +
    facet_wrap(~year) +
    geom_text(data = yy, aes(-96.4, 52.5, label = paste("# Detections\n", N),
                             group = NULL), col = "white", size = rel(8)) + 
    scale_size_continuous(range = c(1, 10), breaks = c(100, 1000, 10000, max(fishData$Detections))) +
    guides(size = guide_legend(title = "# Detections")) +
    theme_map() +
    theme(plot.title = element_text(hjust = 0.5, size = rel(3), colour = "white"),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 17),
          strip.text = element_text(size = rel(3)),
          plot.background = element_rect(fill = '#242f3e', colour = '#242f3e'))
  
  ggsave(plot = p2, paste0(path2, "/", nFish$Species[i], "/", nFish$year[i],
                           " Detections by Station.tiff"),
         compression = "lzw", width = 13, height = 15, dpi = 600)
}

