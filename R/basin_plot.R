

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
}
