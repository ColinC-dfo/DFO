
# Import the data and functions for analysis

source("~/Bayes/Common Carp - January 2020/1 - Data and Function Import.R")


# How many of the 40 Common Carp have been detected throughout the study
# and excluding fish that died shortly after tagging (10 days)

# Pull out the Fish IDs for common carp identified as alive for this analysis
live.carp = df %>% 
  distinct(FishID) %>% 
  pull

# how many?
length(live.carp)
# 28


####
#
## Table 1 -  Species Tagging Info
#
####

# Biological data for all the fish we tagged
Spp %>% 
  group_by(Tagging_Site, Sex) %>% 
  summarize(N = n(),
            min_weight = min(Weight_kg),
            max_weight = max(Weight_kg),
            min_length = min(ForkLength_mm),
            max_length = max(ForkLength_mm)) %>% 
  mutate(`Weight (kg)` = paste(min_weight, "-", max_weight),
         `Length (mm)` = paste(min_length, "-", max_length)) %>% 
  dplyr::select(Tagging_Site, Sex, N, `Weight (kg)`, `Length (mm)`) %>% 
  as.data.frame() %>% 
  format(justify = "left")



# Biological data for carp used in this analysis 
Spp %>% 
  dplyr::filter(FishID %in% live.carp) %>% 
  group_by(Tagging_Site, Sex) %>% 
  summarize(N = n(),
            min_weight = min(Weight_kg),
            max_weight = max(Weight_kg),
            min_length = min(ForkLength_mm),
            max_length = max(ForkLength_mm)) %>% 
  mutate(`Weight (kg)` = paste(min_weight, "-", max_weight),
         `Length (mm)` = paste(min_length, "-", max_length)) %>% 
  dplyr::select(Tagging_Site, Sex, N, `Weight (kg)`, `Length (mm)`) %>% 
  as.data.frame() %>% 
  format(justify = "left")

rm(live.carp)


# Range of dates when the fish were tagged
Spp %>% 
  summarize(minDate = min(Date_Tagged),
            maxDate = max(Date_Tagged))




# Look at a subsample of how many unique days a fish was detected as well as the date
# of the first and last detection by the telemetry system (before adding in
# the last observation carry forward approach)
df %>% 
  mutate(Day = as.Date(lubridate::floor_date(Date, unit = "days"))) %>% 
  group_by(FishID) %>% 
  summarize(Days = length(unique(Day)),
            first_detection = min(Day),
            last_detection = max(Day)) %>% 
  arrange(Days)
# Carp-023 was only detected on 3 separate days, between 2016-06-20 & 2016-07-23
# The value of 4 there is a bit misleading because it wasn't an actual
# detection but since we know exactly where we released it, we know the first location


# The number of fish by their last detection year (minus immediate mortalities)
df %>% 
  mutate(Day = as.Date(lubridate::floor_date(Date, unit = "days"))) %>% 
  group_by(FishID) %>% 
  summarize(Days = length(unique(Day)),
            first_detection = min(Day),
            last_detection = max(Day)) %>% 
  arrange(Days) %>% 
  mutate(last_year_det = format(last_detection, "%Y")) %>% 
  group_by(last_year_det) %>% 
  tally


# Sex ratio for fish left in 2019
df %>% 
  mutate(Day = as.Date(lubridate::floor_date(Date, unit = "days"))) %>% 
  group_by(FishID) %>% 
  summarize(Days = length(unique(Day)),
            first_detection = min(Day),
            last_detection = max(Day)) %>% 
  arrange(Days) %>% 
  mutate(last_year_det = format(last_detection, "%Y")) %>% 
  dplyr::filter(last_year_det == "2019") %>% 
  left_join(., Spp[, c("FishID","Sex")]) %>% 
  group_by(Sex) %>% 
  tally



# Get a summary of the number of fish in the dataset, as well as the
# datetime of the first and last detections on record (as of the 2019 data download)
df %>% 
  ungroup() %>% 
  summarize(Fish = length(unique(FishID)),
            first_detection = min(Date),
            last_detection = max(Date))




#########1#########2#########3#########4#########5#########6#########7#########8#########9
#
#     This section will implement the last observation carry forward approach (locf)
#     for the paper.
#
#                      It takes the data for each fish and:
#
# (1) Calculates the first and last date of detection
# (2) It creates a daily sequence of those dates and then joins the date
#     sequence with the fish data which fill in all the missing dates from the
#     telemetry record. The columns that were missing are filled in with NAs
# (3) Once the dates are filled in and the data is sorted by date, we implement
#     the locf method. This is done by replaceing the NAs from step 2 with the last 
#     observed non-NA data point from the start to the end of the data. There is 
#     a new column added called 'locf' which is just a string of 1's ansd 0's 
#     to indicate whether a point was carried over (1) or not (0)
# (4) Once the dataframe is filled in, we calculate the distance between successive 
#     data points (which could probably be ignored here since I re-calculate distance
#     during the movement summaries later on)
# (5) Finally we have to convert the Date column back to an actual date. The date class
#     was lost during the locf phase when I had to make it a character class so that
#     I didn't lose any information


df = df %>% 
  mutate(Day = as.Date(lubridate::floor_date(Date, unit = "days"))) %>% 
  group_by(FishID) %>% 
  nest() %>% 
  mutate(minDate = unlist(purrr::map(data, .f = function(x) as.character(min(x$Day)))),
         maxDate = unlist(purrr::map(data, .f = function(x) as.character(max(x$Day)))),
         Dates = purrr::map2(minDate, maxDate,
                             .f = function(x, y){
                               data.frame(Day = seq.Date(as.Date(x),
                                                         as.Date(y),
                                                         by = "day"))
                             })) %>%
  select(-minDate, -maxDate) %>% 
  mutate(Samp = purrr::map2(data, Dates, 
                            .f = function(x, y) full_join(x, y, by = "Day"))) %>% 
  dplyr::select(Samp) %>%
  ungroup() %>%
  unnest(Samp) %>% 
  dplyr::select(FishID, Date, Day, everything()) %>% 
  arrange(FishID, Day) %>% 
  group_by(FishID) %>% 
  mutate(locf = ifelse(is.na(Name), 1, 0)) %>% 
  mutate_at(c("Name", "ReceiverLong", "ReceiverLat", "Waterbody", "Marsh"), na.locf) %>% 
  mutate(Distance = haversine(ReceiverLong,ReceiverLat,
                              lag(ReceiverLong), lag(ReceiverLat))/1000,
         Date = ifelse(is.na(Date), paste(as.character(Day), "00:00:00"),
                       as.character(Date)), 
         Date = as.POSIXct(Date, origin = "1970-01-01", format = "%Y-%m-%d %T")) %>% 
  ungroup
  


# Now that the dates are "completed" we can calculate a summary of
# the proportion of days that a fish was detected by the telemetry 
# system
df %>% 
  group_by(FishID, Day) %>% 
  summarize(Days = length(Day),
            missing = sum(locf)) %>% 
  group_by(FishID) %>% 
  summarize(first_to_last = length(unique(Day)),
            missing = sum(missing)) %>% 
  mutate(days_detected = first_to_last - missing,
         prop = days_detected/first_to_last) %>% 
  arrange(desc(prop))


# Create a bar chart showing the percentage of days our fish
# were detected by the telemetry system based on the days
# we expect the fish to have been present and alive
df %>% 
  group_by(FishID, Day) %>% 
  summarize(Days = length(Day),
            missing = sum(locf)) %>% 
  group_by(FishID) %>% 
  summarize(first_to_last = length(unique(Day)),
            missing = sum(missing)) %>% 
  mutate(days_detected = first_to_last - missing,
         prop = days_detected/first_to_last) %>% 
  mutate(prop2 = cut(prop, breaks = c(0, seq(0.1, 1,.1)),
                     labels = c("0-10%",">10-20%",">20-30%",
                                ">30-40%",">40-50%",">50-60%",
                                ">60-70%",">70-80%",">80-90%",
                                ">90-100%"))) %>% 
  ggplot(aes(prop2)) + geom_histogram(stat = "count") + 
  ylab("Number of fish") + scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
  xlab("Proportion of days present in telemetry record") + 
  theme_bw()





# Look at the timinig of fish in the marsh in 2017 & 2018
df %>%
  dplyr::filter(Date > as.POSIXct("2017-01-01") & 
                  Date < as.POSIXct("2019-01-01")) %>% 
  group_by(FishID, Day = lubridate::floor_date(Date, unit = "days")) %>% 
  summarize(Marsh = median(Marsh)) %>% 
  mutate(Year = format(Day, format = "%Y")) %>% 
  ggplot(., aes(Day, Marsh, group = FishID)) +
  geom_line() +
  facet_wrap(~Year, scales = "free_x") + theme_bw()



# Look at the average number days fish spent in the marsh by month in 2017 & 2018
df %>%
  dplyr::filter(Date > as.POSIXct("2017-01-01") & 
                  Date < as.POSIXct("2019-01-01")) %>% 
  group_by(FishID, Day = lubridate::floor_date(Date, unit = "days")) %>% 
  summarize(Marsh = median(Marsh)) %>% 
  group_by(FishID, Month = lubridate::floor_date(Day, unit = "months")) %>% 
  summarize(Marsh = sum(Marsh)) %>% 
  group_by(Month) %>% 
  summarize(Marsh = mean(Marsh)) %>% 
  mutate(Year = format(Month, format = "%Y")) %>% 
  ggplot(., aes(Month, Marsh)) +
  geom_line() + geom_point() +
  facet_wrap(~Year, scales = "free_x") + 
  facet_grid(~Year, scale="free", space="free", switch = "x") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
  theme_bw() + 
  ylab("Average Number of Days Detected in Marsh") + xlab("Date") + 
  # scale_x_datetime(date_breaks = "1 month", date_labels = "%b") + 
  theme(strip.text = element_text(size = rel(1.2), face = "bold"),
        strip.background = element_blank(),
        strip.placement = "outside",
        # panel.grid = element_blank(),
        # panel.border = element_blank(),
        # panel.spacing = unit(0, "cm"),
        axis.title = element_text(colour = "black", size = rel(1.4)),
        axis.text = element_text(colour = "black", size = rel(1.2)),
        axis.line = element_line(colour = "black"))
ggsave("~/Bayes/Common Carp - January 2020/Mean Monthly Marsh Detections.tiff",
       dpi = 500, width = 25, height = 15, units = "cm", compression = "lzw")





# expected number of live fish per month
death$deathTime = ifelse(is.na(death$deathTime), as.POSIXct("2016-06-17"), death$deathTime)
death$deathTime = as.POSIXct(death$deathTime, origin = "1970-01-01")


days = seq(as.POSIXct("2016-06-12", tz = "UTC"),
            as.POSIXct("2019-05-01", tz = "UTC"), by = "1 day")
live.fish = data.frame(Day = days)
live.fish$live = NA
live.fish$live[1] = 40
for(i in 2:length(days)){
  dead = death %>% 
    dplyr::filter(deathTime > days[i]) %>% 
    nrow
  live.fish$live[i] = dead
}
rm(i, days, dead)


ggplot(live.fish, aes(Day, live)) + geom_line(lwd = 1.1) +
  facet_grid(~lubridate::year(Day), scale="free", space="free", switch = "x") +
  scale_x_datetime(breaks = seq(as.POSIXct("2016-01-01", tz = "UTC"),
                                as.POSIXct("2019-12-01", tz = "UTC"),
                                by = "2 months"), date_labels = "%b", expand = c(0,0)) +
  theme_bw() + ylab("Estimated Number of Live Fish") + xlab("Date") +
  theme(strip.background = element_blank(),
        axis.text = element_text(size = rel(1), colour = "black"),
        axis.title = element_text(size = rel(1.2), colour = "black"),
        strip.text = element_text(size = rel(1.3)),
        strip.placement = "outside",
        panel.spacing = unit(0, "cm"),
        axis.line = element_line(colour = "black")) 

ggsave("~/Bayes/Common Carp - January 2020/Fish mortality over time.tiff",
       dpi = 500, width = 25, height = 12, units = "cm", compression = "lzw")


live.fish$Day = as.Date(live.fish$Day)
df %>%
  dplyr::filter(Date > as.POSIXct("2016-07-01", tz = "UTC") & 
                  Date < as.POSIXct("2019-06-01", tz = "UTC")) %>% 
  mutate(Day = lubridate::floor_date(Date, unit = "day"),
         Day = as.Date(Day)) %>% 
  # group_by(Month = lubridate::floor_date(Date, unit = "months"), Waterbody) %>%
  group_by(Day, Waterbody) %>%
  summarize(Fish = length(unique(FishID))) %>% 
  dplyr::filter(Waterbody == "Winnipeg River") %>% 
  left_join(live.fish[,c("Day","live")]) %>% 
  mutate(Prop = Fish/live) %>% 
  ggplot(., aes(Day, Prop)) + geom_line(lwd = 1.1) + theme_bw() + 
  scale_x_date(breaks = seq(as.Date("2016-01-01"), as.Date("2019-12-01"),
                                by = "2 months"), date_labels = "%b", expand = c(0,0)) +
  facet_grid(~lubridate::year(Day), space = "free_x", scales = "free_x", switch = "x") +
  ylab("Proprtion of fish detected on Winnipeg River receivers") + xlab("Date") + 
  # scale_x_date(date_breaks = "3 months", date_labels = "%b/%y") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(strip.background = element_blank(),
        axis.text = element_text(size = rel(1), colour = "black"),
        axis.title = element_text(size = rel(1.2), colour = "black"),
        strip.text = element_text(size = rel(1.3)),
        strip.placement = "outside",
        panel.spacing = unit(0, "cm"),
        axis.line = element_line(colour = "black")) 
ggsave("~/Bayes/Common Carp - January 2020/Winnipeg River Detections.tiff",
       dpi = 500, width = 25, height = 15, units = "cm", compression = "lzw")


rm(live.fish)






# 
# 
# clust.data = df %>% 
#   dplyr::filter(Date > as.POSIXct("2017/11/15") & Date < as.POSIXct("2018/04/01")) %>% 
#   group_by(FishID) %>% 
#   summarize(Long = median(ReceiverLong),
#             Lat = median(ReceiverLat)) %>% 
#   # left_join(Spp[,c("FishID","ForkLength_mm", "Weight_kg")]) %>% 
#   dplyr::select(-FishID) %>% 
#   as.data.frame()
# 
# # k means clustering
# 
# # Initialize total within sum of squares error: wss
# wss <- 0
# 
# # Look over 1 to 15 possible clusters
# for (i in 1:15) {
#   # Fit the model: km.out
#   km.out <- kmeans(clust.data, centers = i, nstart = 5, iter.max = 50)
#   # Save the within cluster sum of squares
#   wss[i] <- km.out$tot.withinss
# }
# 
# # Produce a scree plot
# plot(1:15, wss, type = "b", 
#      xlab = "Number of Clusters", 
#      ylab = "Within groups sum of squares")
# 
# 
# # Select number of clusters
# k <- 4
# 
# # Build model with k clusters: km.out
# km.carp <- kmeans(clust.data, centers = k, nstart = 3, iter.max = 50)
# 
# # View the resulting model
# km.carp
# 
# 
# library(plotly)
# wpg = readr::read_csv("~/Transfer/LakeWpgOutline.csv")
# 
# fig <- plot_ly(clust.data, x = ~Long, y = ~Lat, color = ~km.carp$cluster)
# fig <- fig %>% add_markers()
# fig <- fig %>%
#   add_polygons(x = wpg$Long,
#                y = wpg$Lat,
#                line = list(width = 2, color = "black"),
#                fillcolor = 'transparent', inherit = FALSE)
# fig <- fig %>% layout(scene = list(xaxis = list(title = 'Longitude',
#                                                 range = c(-97, -96)),
#                                    yaxis = list(title = 'Latitude',
#                                                 range = c(50.3, 51.4))))
# # zaxis = list(title = 'Fork Length')))
# fig
# 
# rm(k, wss, i, fig, clust.data, km.carp, km.out)
# 


#####
wpg = readr::read_csv("~/Transfer/LakeWpgOutline.csv")


# See which Fish were detected north of Pine Dock for a test for North Basin Detctions
# Only 2 have gone north of Pine Dock (weekly mean position)
df %>% 
  group_by(FishID, Date = lubridate::floor_date(Date, unit = "week")) %>% 
  summarize(ReceiverLat = median(ReceiverLat),
            ReceiverLong = median(ReceiverLong)) %>% 
  dplyr::filter(any(ReceiverLat > 51.64)) %>% 
  ggplot(., aes(ReceiverLong, ReceiverLat, col = Date)) + geom_path() +
  facet_wrap(~FishID) + theme_void() + 
  geom_polygon(data = wpg, aes(Long, Lat), fill = NA, col = "black")



####
#
## Figure 1 - Study Area.tiff code can be located in:
## ~/Charles/Mapping/MB and SB of Lake Wpg.R
#
####



####
#
## Create Figure 2 - Proportion of detections in different habitat types
#
####

df %>%
  # dplyr::filter(Species == "Common Carp") %>% 
  dplyr::filter(Date >= as.POSIXct("2016-07-01") & Date < as.POSIXct("2019-06-01")) %>% 
  mutate(Day = lubridate::floor_date(Date, unit = "day")) %>% 
  # mutate(Month = lubridate::floor_date(Date, unit = "month")) %>% 
  dplyr::group_by(Day, Waterbody) %>% 
  summarize(N = length(unique(FishID))) %>%
  ungroup() %>% 
  tidyr::complete(Day, Waterbody, fill = list(N = 0)) %>% 
  group_by(Day) %>% 
  mutate(percentage = N/sum(N)) %>% 
  ggplot(data = ., aes(x = Day, y = percentage, group = Waterbody,
                       fill = Waterbody, col = Waterbody)) +
  geom_area() + scale_x_datetime(breaks = "2 months", date_labels = "%b",
                                 expand = c(0,0)) + theme_bw() + 
  scale_y_continuous(expand = c(0,0), labels = scales::percent) +
  facet_grid(~lubridate::year(Day), space = "free_x", scales = "free_x", switch = "x") +
  ylab("% of Fish Detected") +# facet_wrap(~Species) +
  scale_fill_manual(values = c("Lake Winnipeg" = cbbPalette[8],
                               "Marsh" = cbbPalette[2],
                               "Red River" = cbbPalette[5],
                               "Winnipeg River" = cbbPalette[4])) +
  scale_colour_manual(values = c("Lake Winnipeg" = cbbPalette[8], "Marsh" = cbbPalette[2],
                               "Red River" = cbbPalette[5], "Winnipeg River" = cbbPalette[4])) +
  theme_classic() + xlab("Date") +
  theme(legend.position = "top", 
        legend.title = element_blank(),
        axis.text = element_text(size = rel(1), colour = "black"),
        axis.title = element_text(size = rel(1.2), colour = "black"),
        strip.text = element_text(size = rel(1.3)),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.spacing = unit(0, "cm"),
        panel.border = element_blank()) 
ggsave("~/Bayes/Common Carp - January 2020/Figure 2 - Detections by Waterbody.tiff",
       dpi = 500, width = 20, height = 15, units = "cm", compression = "lzw")








test = df %>% 
  dplyr::filter(Day >= as.POSIXct("2016-07-01", tz = "UTC") &
                  Day <= as.POSIXct("2019-05-31", tz = "UTC")) %>%
  mutate(Day = lubridate::floor_date(Date, "day")) %>% 
  group_by(FishID, Day) %>%
  summarize(ReceiverLong = mean(ReceiverLong),
            ReceiverLat = mean(ReceiverLat)) %>% 
  mutate(Distance = haversine(ReceiverLong,ReceiverLat,
                              lag(ReceiverLong), lag(ReceiverLat))/1000) %>% 
  group_by(FishID, Month = lubridate::floor_date(Day, "month")) %>%
  summarize(moves = sum(+(na.omit(Distance) > 0)),
            Distance = sum(Distance, na.rm = TRUE),
            N = n()) %>%
  arrange(FishID, Month)


p = test %>%
  ggplot(aes(Month, Distance, group = Month)) + 
  geom_boxplot(fill = "lightgrey", outlier.shape = NA) + 
  ylab("Distance Travelled (km)") + xlab("Date") +
  theme_classic() +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b", expand = c(0.02,0)) +
  facet_grid(~lubridate::year(Month), space = "free_x", scales = "free_x", switch = "x") +
  theme(legend.position = "top", 
        legend.title = element_blank(),
        axis.text = element_text(size = rel(1), colour = "black"),
        axis.title = element_text(size = rel(1.2), colour = "black"),
        strip.text = element_text(size = rel(1.3)),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.spacing = unit(0, "cm"),
        panel.border = element_blank()) 


p1 = p + df %>%
  # dplyr::filter(Species == "Common Carp") %>%
  dplyr::filter(Day >= as.POSIXct("2016-07-01", tz = "UTC") & 
                  Day <= as.POSIXct("2019-05-31", tz = "UTC")) %>%
  group_by(Month = lubridate::floor_date(Date, "month")) %>%
  summarize(Distance = sum(na.omit(Distance/1000)),
            N = length(unique(FishID))) %>%
  geom_text(data = ., aes(Month, y = -5, label = paste0("(",N,")")),
            fontface = "bold", size = rel(6)) +
  coord_cartesian(ylim = c(0, 230))

ggsave(plot = p1, "~/Bayes/Common Carp - January 2020/Figure 3 - Monthly Distance.tiff",
       compression = "lzw",  width = 20, height = 10, dpi = 600)

rm(p, p1, test)




####


gimli = read_csv("~/Bayes/Common Carp - January 2020/weatherstats_gimli_daily.csv") %>% 
  mutate(sunrise = as.POSIXct(paste(date, sunrise), format = "%m/%d/%Y %T"),
         sunset = as.POSIXct(paste(date, sunset), format = "%m/%d/%Y %T")) %>% 
  dplyr::select(sunrise, sunset) %>% 
  arrange(sunrise) %>% 
  mutate(Day = lubridate::floor_date(sunrise,"day")) %>% 
  mutate(Day = as.Date(Day))

gimli %>% 
  peek


x = df %>% 
  # dplyr::filter(FishID %in% paste0("Carp-00",1:9)) %>%
  # dplyr::filter(FishID == "Carp-001") %>%
  # dplyr::filter(Day < as.Date("2016-08-01")) %>%
  inner_join(., gimli, by = "Day") %>% 
  mutate(dayNight = ifelse(Date > sunrise & Date < sunset, 'Day', 'Night')) %>% 
  mutate(diel_period = ifelse(Date < sunset, Day, Day + 1),
         diel_period = as.Date(diel_period, origin = "1970-01-01")) %>% 
  dplyr::select(-sunrise, -sunset) %>% 
  group_by(FishID, dayNight) %>% 
  nest() %>% 
  mutate(minDate = unlist(purrr::map(data, .f = function(x) as.character(min(x$Day)))),
         maxDate = unlist(purrr::map(data, .f = function(x) as.character(max(x$Day)))),
         Dates = purrr::map2(minDate, maxDate,
                             .f = function(x, y) data.frame(Day = seq.Date(as.Date(x),
                                                                           as.Date(y),
                                                                           by = "day")))) %>%
  select(-minDate, -maxDate) %>% 
  mutate(Samp = purrr::map2(data, Dates, .f = function(x, y) full_join(x, y, by = "Day"))) %>% 
  dplyr::select(Samp) %>%
  ungroup() %>%
  unnest(Samp) %>% 
  dplyr::select(FishID, Date, Day, everything()) %>% 
  arrange(FishID, Day) %>% 
  group_by(FishID) %>% 
  mutate(Date = case_when(
    is.na(Date) & dayNight == "Day" ~ as.character(paste(Day, "12:00:00")),
    is.na(Date) & dayNight == "Night" ~ as.character(paste(Day, "00:00:00")),
    TRUE ~ as.character(Date)
  )) %>% 
  mutate(Date = as.POSIXct(Date)) %>% 
  arrange(FishID, Date) %>% 
  mutate(locf = ifelse(is.na(Name), 1, 0),
         diel_period = ifelse(is.na(diel_period), as.character(Day), as.character(diel_period)),
         diel_period = as.Date(diel_period, origin = "1970-01-01")) %>%
  mutate_at(c("Name", "ReceiverLong","ReceiverLat",
              "Waterbody","Marsh","Distance"), na.locf) %>% 
  group_by(FishID, diel_period, dayNight) %>% 
  summarize(ReceiverLong = mean(ReceiverLong),
            ReceiverLat = mean(ReceiverLat)) %>% 
  # arrange(dayNight, diel_period) %>% 
  group_by(FishID) %>% 
  mutate(Distance = haversine(ReceiverLong,ReceiverLat,
                              lag(ReceiverLong), lag(ReceiverLat))/1000) %>% 
  mutate(Month = lubridate::floor_date(diel_period, "month")) %>% 
  # dplyr::filter(Month < as.Date("2016-10-01")) %>% 
  dplyr::filter(Month > as.Date("2016-06-01")) %>% 
  arrange(FishID, dayNight, Month, diel_period) %>% 
  group_by(FishID, Month, dayNight) %>% 
  summarize(Dist = sum(Distance, na.rm = TRUE)) 



x %>% 
  ggplot(., aes(Month, Dist, fill = dayNight, group = interaction(Month, dayNight))) +
  geom_boxplot()
