


rm(list = ls())
require(tidyverse)
require(RSQLite)
require(grid)
require(gtable)

cbbPalette = c("#FFFFFF", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7")



Mortality_Plot = function(p){
  dummy <- p
  dummy$layers <- NULL
  dummy <- dummy + geom_rect(data = dat, xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf,
                             aes(fill = facet_fill_colour)) + 
    scale_fill_manual(values = c("Alive" = "lightblue","Some Data" = "yellow","Dead/No Data" = "tomato")) 
  
  g1 <- ggplotGrob(p)
  g2 <- ggplotGrob(dummy)
  
  gtable_select <- function (x, ...) 
  {
    matches <- c(...)
    x$layout <- x$layout[matches, , drop = FALSE]
    x$grobs <- x$grobs[matches]
    x
  }
  
  panels <- grepl(pattern="panel", g2$layout$name)
  strips <- grepl(pattern="strip-t", g2$layout$name)
  g2$layout$t[panels] <- g2$layout$t[panels] - 1
  g2$layout$b[panels] <- g2$layout$b[panels] - 1
  
  new_strips <- gtable_select(g2, strips|panels)
  
  gtable_stack <- function(g1, g2){
    g1$grobs <- c(g1$grobs, g2$grobs)
    g1$layout <- rbind(g1$layout, g2$layout)
    g1
  }
  ## ideally you'd remove the old strips, for now they're just covered
  new_plot <- gtable_stack(g1, new_strips)
  grid.newpage()
  grid.draw(new_plot)
  
  return(new_plot)
}



setwd("~/2019 Telemetry")
con = dbConnect(RSQLite::SQLite(), "LWBT_2019.sqlite")

# list the tables
DBI::dbListTables(con)
con %>%
  tbl("sqlite_stat1")
# con %>%
#   tbl("dataDictionary")


# biological info for the fish
Spp = con %>%
  tbl("speciesTable") %>%
  dplyr::filter(Species == "Common Carp") %>% 
  collect() %>%
  mutate(Date_Tagged = as.Date(Date_Tagged, origin = "1970-01-01"))



carp = c(paste0("Carp-00", 1:9), paste0("Carp-0", 10:41))
# fish movement data (and potential noise for the system)
df = con %>%
  tbl("rawData_fishTable") %>%
  dplyr::filter(FishID %in% carp) %>% 
  collect() %>%
  mutate(Date = as.POSIXct(Date, origin = as.POSIXct("1970-01-01"))) 


# load in stations data so we can merge the coordinates where the station was set
stations = con %>%
  tbl("stationsTable") %>%
  collect() %>%
  mutate(Start = as.POSIXct(Start, origin = as.POSIXct("1970-01-01", tz = "UTC"), tz = "UTC"),
         End = as.POSIXct(End, origin = as.POSIXct("1970-01-01", tz = "UTC"), tz = "UTC")) %>%
  distinct(Name, .keep_all = T)


dbDisconnect(con)
rm(con, carp)



df <- df %>%
  dplyr::select(-Transmitter, -Value, -Station, -SN) %>% 
  # left_join(., Spp[,c("FishID","Species","Tagging_Site","taggedLong","taggedLat")]) %>%
  left_join(., stations[,c("Name", "Waterbody")]) %>% 
  mutate(Waterbody = ifelse(ReceiverLat < 50.404230 & ReceiverLat > 50.298805, "Marsh", Waterbody)) %>%
  mutate(Waterbody = ifelse(Name == "Manigotagan River", "Lake Winnipeg", Waterbody)) %>%
  mutate(Marsh = ifelse(Waterbody == "Marsh", 1, 0)) %>% 
  mutate(Waterbody = ifelse(Name %in% c("La Salle River", "Cooks Creek", "Seine River",
                                        "Rat River","Roseau River"), "Red River", Waterbody),
         Waterbody = ifelse(Name %in% c("LW 18A", "LW 21", "LW 21A", "LW 26"), "Winnipeg River", Waterbody),
         Waterbody = ifelse(Name %in% c("Devils Creek", "Red rkm 1", "Morrison Lake",
                                        "Cochrane Lake","Straight Creek",
                                        "West Channel Red River","2nd East Channel Red River",
                                        "Far west Channel Red River","Lower Devil Lake",
                                        "Red rkm 14","Red rkm 9", "Red East Channel",
                                        "Red rkm 4"), "Marsh", Waterbody),
         Waterbody = ifelse(Name %in% c("Mouth of Dauphin River"), "Dauphin River", Waterbody),
         Waterbody = ifelse(Name == "Manigotagan River", "Lake Winnipeg", Waterbody)) %>% 
  mutate(Name = case_when(
    Name %in% c("East Doghead Point","Doghead Channel","West Doghead Point") ~ "Doghead",
    Name %in% c("LWGHW", "LWGHE") ~ "Gull Harbour",
    Name %in% c("LWBE", "LWBW") ~ "Black Island",
    Name %in% c("Traverse Bay 1", "Traverse Bay 2", "Traverse Bay 3") ~ "Traverse Bay",
    TRUE ~ as.character(Name)
  )) %>% 
  mutate(ReceiverLong = case_when(
    Name == "Doghead" ~ -96.81843,
    Name == "Gull Harbour" ~ -96.60469,
    Name == "Black Island" ~ -96.33323,
    Name == "Traverse Bay" ~ -96.36728,
    TRUE ~ ReceiverLong
  )) %>% 
  mutate(ReceiverLat = case_when(
    Name == "Doghead" ~ 51.73999,
    Name == "Gull Harbour" ~ 51.18060,
    Name == "Black Island" ~ 51.26189,
    Name == "Traverse Bay" ~ 50.64938,
    TRUE ~ ReceiverLat
  )) %>% 
  distinct() %>% 
  arrange(Date) %>% 
  mutate(rowID = 1:n()) 



# Account for double detections at Doghead, Gull Harbour, Black Island & Winnipeg River
# If the arrival time is less than 80 seconds at the gates pull out that rowID

badRows = df %>% 
  dplyr::filter(Name %in% c("Doghead", "Gull Harbour", "Black Island",
                            "Traverse Bay")) %>% 
  group_by(FishID, Name) %>% 
  arrange(FishID, Date) %>% 
  mutate(Diff = c(NA, diff.POSIXt(Date))) %>% 
  dplyr::filter(Diff < 80) %>% 
  pull(rowID)
  

# Remove the rows that were identified as < 80 sec arrival time 
df = df %>% 
  dplyr::filter(!(rowID %in% badRows)) %>% 
  dplyr::select(-rowID) 



rm(badRows)



####
#
## Haversine Function for calculating distance
#
####


haversine = function(lon1, lat1, lon2, lat2){
  require(pracma)
  R = 6371000 # meters
  deltaLat = deg2rad(lat2 - lat1)
  deltaLon = deg2rad(lon2 - lon1)
  lat1 = deg2rad(lat1); lon1 = deg2rad(lon1)
  lat2 = deg2rad(lat2); lon2 = deg2rad(lon2)
  a = sin(deltaLat/2) * sin(deltaLat/2) + cos(lat1) * cos(lat2) * sin(deltaLon/2) * sin(deltaLon/2)
  c = 2 * atan2(sqrt(a), sqrt(1-a))
  d = R * c
  return(d)
}


distFun = function(x){
  x1 = x %>%
    mutate(Distance = haversine(ReceiverLong, ReceiverLat,
                                dplyr::lag(ReceiverLong), dplyr::lag(ReceiverLat))/1000) %>%
    summarize(Dist = sum(Distance, na.rm = T))
  return(x1)
}



# last observation carry forward
na.locf <- function(x) {
  v <- !is.na(x)
  c(NA, x[v])[cumsum(v)+1]
}


peek <- function(x){
  x %>% 
    head(n = 8) %>% 
    as.data.frame
}



###


# Distance between the Traverse Bay receivers
haversine(-96.36636, 50.65033, -96.36087, 50.65564)
haversine(-96.36636, 50.65033, -96.37460, 50.64216)
haversine(-96.36087, 50.65564, -96.37460, 50.64216)



# df %>%
#   dplyr::filter(Name %in% c("Doghead", "Gull Harbour", "Black Island","LW 1","LW 2",
#                             "Traverse Bay")) %>% 
#   group_by(Name) %>% 
#   distinct(ReceiverLong, ReceiverLat) %>% 
#   group_by(Name) %>% 
#   summarize(Long = mean(ReceiverLong),
#             Lat = mean(ReceiverLat)) %>% 
#   peek



###


####
#
## After doing a little checking, I think Carp-033 has a false detection on LW 66. It was
## detected on the Straight Creek receiver in June 2016 then disappeared for 2 years
## and the on March 31, 2018 it showed up and then never again. I am removing the detection
## from the analysis
df %>% 
  dplyr::filter(FishID == "Carp-033" & Date > as.POSIXct("2018-01-01")) %>% 
  peek

df %>% 
  dplyr::filter(FishID == "Carp-023") %>% 
  dplyr::filter(!(FishID == "Carp-023" & Date > as.POSIXct("2016-08-01"))) %>% 
  tail

# all carp data
df %>% 
  nrow

# carp data without that one data point
df %>% 
  dplyr::filter(!(FishID == "Carp-033" & Date > as.POSIXct("2018-01-01"))) %>% 
  dplyr::filter(!(FishID == "Carp-023" & Date > as.POSIXct("2016-08-01"))) %>% 
  nrow

# Carp-033	2018-05-31 18:03:40	2018-05-31	LW 66	-96.63120	51.38758	Lake Winnipeg	0	115.0578	0


# remove the data point and continue with analysis
df = df %>% 
  dplyr::filter(!(FishID == "Carp-033" & Date > as.POSIXct("2018-01-01"))) %>% 
  dplyr::filter(!(FishID == "Carp-023" & Date > as.POSIXct("2016-08-01"))) 




df %>% 
  group_by(FishID) %>% 
  summarize(minDate = min(Date),
            maxDate = max(Date),
            N = n()) %>% 
  right_join(., Spp[,c("FishID")]) %>% 
  mutate(N = ifelse(is.na(N), 0, N),
         col = ifelse(N == 0, "red","black")) %>% 
  ggplot(., aes(y = reorder(FishID, desc(FishID)))) +
  geom_segment(aes(x = minDate, xend = maxDate,
                   y = reorder(FishID, desc(FishID)),
                   yend = reorder(FishID, desc(FishID))), lwd = 1.1) + theme_bw() +
  geom_vline(xintercept = as.POSIXct(c("2017-01-01","2018-01-01", "2019-01-01")),
             lty= 2) + xlab("Date") + ylab("FishID") +
  scale_x_datetime(breaks = "6 months", date_labels = "%b-%Y") + 
  geom_text(aes(x = as.POSIXct("2016-05-01"),
                y = reorder(FishID, desc(FishID)), label = paste0("(",N,")"), col = col), 
            fontface = "bold", size = rel(3)) + 
  scale_color_manual(values = c("red" = "red", "black" = "black")) + 
  theme(legend.position = "none")
ggsave("~/Bayes/Common Carp - January 2020/Figure X - Detection Gant Chart.tiff",
       dpi = 500, width = 20, height = 15, units = "cm", compression = "lzw")




####
#
## Function to identify death time for fish based on no movement
#
####



# Estimate the death time of fish using the record of detections.
# It uses distance moved between detections to identify movement.
# The method I use is distance as calculated by the haversine method 
# (Sinnot 1984??). It should be noted that this method doesn't
# differentiate between actual fish death, tag failure, 
# tag expulsion or fish moving outside the receiver array. One of the 
# main advantages of this method is that it can identify instances 
# where a tag was continuously detected on the same receiver over 
# a prolonged period of time. By removing these detections, the 
# analyst can remove data that could inflate habitat
# use (depth, lake vs river, temperature, etc.).
# It is up to the analyst to determine if an animals behaviour is 
# representative of the spatial/temporal record of the tag.

deathDate = function(dataframe){
  test = NULL
  x = dataframe$Distance
  for(i in 1:length(x)){
    test[i] = mean(x[(length(x) - i):length(x)], na.rm = TRUE)
    if(test[i] > 0){
      i = i-1
      break
    } else if(all(test == 0)){
      i = length(x)-1
    }
  }
  deathTime = dataframe$Date[(length(x)-i)]
  return(deathTime)
}






death = df %>% 
  group_by(FishID) %>% 
  # since we added in the tagging location for each fish
  # if that fish was not detected after tagging it will only have one
  # detection in the record. We will remove fish that were only detected 
  # once, otherwise it will mess up the deathDate function
  dplyr::filter(n() > 1) %>% 
  mutate(Distance = haversine(ReceiverLong,ReceiverLat,
                              lag(ReceiverLong), lag(ReceiverLat))/1000) %>% 
  dplyr::select(Date, Name, ReceiverLong, ReceiverLat, FishID, Distance) %>% 
  do(data.frame(deathTime = deathDate(.))) %>% 
  mutate(Status = ifelse(deathTime > as.POSIXct("2019-01-01"), "Alive","Dead/Fail")) %>% 
  right_join(., Spp[,c("FishID")]) %>% 
  as.data.frame() 




removeFish = df %>% 
  left_join(death) %>% 
  dplyr::filter(Date <= deathTime) %>% 
  dplyr::select(-deathTime) %>% 
  group_by(FishID) %>% 
  summarize(minDate = min(Date),
            maxDate = max(Date),
            N = n()) %>% 
  mutate(time = difftime(maxDate, minDate, units = "days")) %>% 
  arrange(time) %>% 
  dplyr::filter(time > 10)


# Remove fish that were detected less than 10 days
df = df %>% 
  dplyr::filter(FishID %in% removeFish$FishID) %>% 
  mutate(Distance = haversine(ReceiverLong,ReceiverLat,
                              lag(ReceiverLong), lag(ReceiverLat))/1000)

rm(removeFish)

# We've removed rarely detected fish












# Pull out the tagging details from the tagging sheet and add them to the fish 
# data so that we can add in the "first" location for each fish to the data frame 
TaggingData = Spp %>% 
  dplyr::rename(Date = Date_Tagged,
                ReceiverLong = taggedLong,
                ReceiverLat = taggedLat) %>%
  mutate(Date = as.POSIXct(paste(Date,"08:00:00")),
         Name = "Tagging Location",
         Waterbody = "Marsh",
         Marsh = 1,
         Distance = NA) %>% 
  dplyr::select(Date, Name, ReceiverLong, ReceiverLat, FishID, 
                Waterbody, Marsh, Distance) 


cat(nrow(df))

# this would add 40 data points (one for each fish's tagging location)
bind_rows(df, TaggingData) %>% 
  arrange(FishID, Date) %>% 
  nrow

# Since we've removed 12 fish because of death, we also need to remove 
# fish with only a single "detection" (the manually added tagging location)
# from the new data set
df = bind_rows(df, TaggingData) %>% 
  group_by(FishID) %>% 
  dplyr::filter(n() > 1) %>% 
  arrange(FishID, Date) %>% 
  ungroup()

# the data is now ready for further analysis
# See "Common Carp Script.R" for next stages

rm(TaggingData)


