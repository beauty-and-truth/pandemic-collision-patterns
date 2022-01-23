library(tidyverse)
library(lubridate)  # for tidying dates
library(modelr)  # wrapper to pipe base R modeling functions
library(dbscan)  # clustering algorithm IDs dangerous spots
library(grid)  # for the nj map
library(png)  # for the nj map image
library(maps)  # for the US maps
library(mapproj)  # for the US maps
library(stringi) 
library(plotly)  # for making ggplots interactive

#### GET THE DATA ###############################
cols <- "ID	Severity	Start_Time	End_Time	Start_Lat	Start_Lng	End_Lat	End_Lng	Distance(mi)	Description	Number	Street	Side	City	County	State	Zipcode	Country	Timezone	Airport_Code	Weather_Timestamp	Temperature(F)	Wind_Chill(F)	Humidity(%)	Pressure(in)	Visibility(mi)	Wind_Direction	Wind_Speed(mph)	Precipitation(in)	Weather_Condition	Amenity	Bump	Crossing	Give_Way	Junction	No_Exit	Railway	Roundabout	Station	Stop	Traffic_Calming	Traffic_Signal	Turning_Loop	Sunrise_Sunset	Civil_Twilight	Nautical_Twilight	Astronomical_Twilight"
cols <- str_split(cols, "\t") %>% unlist()
setwd(file.path("R", "nn"))

accident_data <- read_csv(
  "US_accidents_Dec20_updated.csv",
  col_names = cols,
  col_types = "cfTTdddddccccccfccfcTdddddcddclllllllllllllcccc",
  skip = 1
)
rm(cols)
# Date and time values
accident_data <- accident_data %>%
  mutate(year = format(Start_Time, "%Y"),
         yday = yday(Start_Time),
         mo = format(Start_Time, "%m"),
         day = format(Start_Time, "%d"),
         hour = format(Start_Time, "%H"),
         date = floor_date(Start_Time, unit = "day"),
         daysSince = as.integer(difftime(ymd(date), mdy(02082016), units='days')),
         weekday = factor(weekdays(Start_Time), 
                          ordered = TRUE, 
                          levels = c("Monday", "Tuesday", "Wednesday",
                                     "Thursday", "Friday", "Saturday", 
                                     "Sunday")),
         season = ifelse(mo == "12" | mo == "01" | mo == "02", "winter",
                         ifelse(mo == "03" | mo == "04" | mo == "05", "spring",
                                ifelse(mo == "06" | mo == "07" | mo == "08", "summer",
                                       "fall"))))

# Remove extraneous columns
tidyAccident <- accident_data[, c(2,3,5,6,12:17, 22, 24:30, 48:56)]

### Infer Missing Values
# if wind direction is calm, wind speed is 0  
tidyAccident <- tidyAccident %>%
  mutate(`Wind_Speed(mph)` = ifelse(is.na(`Wind_Speed(mph)`) &
                                      Wind_Direction == "Calm", 0, 
                                    `Wind_Speed(mph)` ),
         `Precipitation(in)` = ifelse(is.na(`Precipitation(in)`) & 
                                        Weather_Condition == "Clear" |
                                        Weather_Condition == "Scattered Clouds" |
                                        Weather_Condition == "Partly Cloudy" |
                                        Weather_Condition == "Mostly Cloudy" |
                                        Weather_Condition == "Overcast",
                                      0.0, `Precipitation(in)`))

# Remove extreme values suggestive of errors
tidyAccident <- tidyAccident %>% 
  select(!c(Wind_Direction, Weather_Condition)) %>%
  mutate(Side = ifelse(Side == "Right", 1, 0)) %>%
  filter(`Temperature(F)` < 130, # highest recorded temp ever is 134
         `Temperature(F)` > -39, # very low values exclusively from FL, TX suggest error
         `Wind_Speed(mph)` < 150) %>%
  na.omit(tidyAccident)

# Pre-Covid data set covers 1483 days
PreCov <- tidyAccident %>%
  filter(Start_Time < mdy(03012020))
# Covid data set covers 306 days
Covid <- tidyAccident %>%
  filter(Start_Time >= mdy(03012020))

covRange <- range(crashCovid$Start_Time)
preCovRange <- range(crashPreCov$Start_Time)
difftime(preCovRange[2], preCovRange[1])
difftime(covRange[2], covRange[1])

##### Rush Hour Heatmap ########
txtHour = c("12am", "1am", "2am", "3am", "4am", "5am", "6am",
            "7am", "8am", "9am", "10am", "11am", "12pm",
            "1pm", "2pm", "3pm", "4pm", "5pm", "6pm", "7pm",
            "8pm", "9pm", "10pm", "11pm")
txtHour = factor(txtHour, ordered=TRUE, levels = txtHour)

preCovidHourWeek <- PreCov %>%
  group_by(hour, weekday) %>%
  summarize(count = n() * (7/1483)) #weeks in the dataset
preCovidHourWeek$txtHour <- factor(rep(txtHour, each = 7),
                                   ordered = TRUE,
                                   levels = txtHour)

# PRE-COVID HOUR AND WEEKDAY HEATMAP
ggplot(preCovidHourWeek, aes(x=weekday, y=txtHour, fill = count)) +
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red")+
  ggtitle("Average Crashes Per Hour of the Week, USA 2016-2019")+
  xlab("Day of the Week")+
  ylab("Time of Day")

# During Covid
covidHourWeek <- Covid %>%
  group_by(hour, weekday) %>%
  summarize(count = n() * (7/306)) #weeks in the dataset

covidHourWeek$txtHour <- rep(txtHour, each = 7)
covidHourWeek$txtHour <- factor(covidHourWeek$txtHour, ordered = TRUE,
                                levels = txtHour)

# COVID HOUR AND WEEKDAY HEATMAP
ggplot(covidHourWeek, aes(x=weekday, y=txtHour, fill = count)) +
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red")+
  ggtitle("Average Crashes Per Hour of the Week, Mar-Dec 2020")+
  xlab("Day of the Week")+
  ylab("Time of Day")

### Plot change due to covid ###
hour <- rep(txtHour, each = 7)

weekday <- rep(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                 "Saturday", "Sunday"), times = 24)
weekday <- factor(weekday, ordered = TRUE, 
                  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                             "Saturday", "Sunday"))

diff <- covidHourWeek$count - preCovidHourWeek$count
pctChange <- (diff/preCovidHourWeek$count)*100

dCrash <- tibble(hour = factor(hour, ordered=TRUE, levels=txtHour),
                 weekday = weekday, 
                 diff = diff, 
                 pctChange = pctChange)



# Absolute Change in Crashes for Each Hour of the Week
ggplot(dCrash, aes(x=weekday, y=hour, fill = diff)) +
  geom_tile() +
  scale_fill_gradient2(low = "deepskyblue", high = "tomato")+
  ggtitle("Absolute Change in Crashes per Hour During Covid")+
  labs(fill = "Additional Crashes")

# Percent Change in Crashes for Each Hour of the Week
ggplot(dCrash, aes(x=weekday, y=hour, fill = pctChange)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red") +
  ggtitle("Percent Change in Crashes \n per Hour of the Week During Covid")+
  labs(fill="% Change")


#### Time Series ##########
PreCov_hr <- PreCov %>%
  group_by(hour, date) %>%
  summarize(count = n()) 
ggplot(PreCov_hr, aes(x=date, y=hour, fill=count)) +
  theme_classic() +
  geom_tile()

Covid_hr <- Covid %>%
  group_by(hour, date) %>%
  summarize(count = n()) 
ggplot(Covid_hr, aes(x=date, y=hour, fill=count)) +
  theme_classic() +
  geom_tile()

ts_crash <- tidyAccident %>%
  mutate(date <- as.Date(date)) %>%
  group_by(date, weekday, daysSince) %>%
  summarize(count = n())

ggplot(ts_crash, aes(x=daysSince, y=count)) +
  geom_line()

ggplot(ts_crash, aes(x=date, y=count)) +
  geom_line() +
  geom_vline(aes(xintercept = as.numeric(date[c(1291)])), color="green")+
  geom_vline(aes(xintercept = as.numeric(date[c(1494)])), color="green")+
  geom_vline(aes(xintercept = as.numeric(date[c(1597)])), color="green")+
  geom_vline(aes(xintercept = as.numeric(date[c(1622)])), color="green")


## Remove Weekday Variation:
mod <- lm(count ~ weekday, data = ts_crash)

ts_wkday <- ts_crash %>%
  add_residuals(mod)
ts_wkday_vis <- ts_wkday %>%
  ggplot(aes(date, resid)) +
  geom_ref_line(h = 0) +
  geom_line() +
  geom_vline(aes(xintercept = as.numeric(date[c(1291)])), color="green")+
  geom_vline(aes(xintercept = as.numeric(date[c(1494)])), color="green")+
  geom_vline(aes(xintercept = as.numeric(date[c(1597)])), color="green")+
  geom_vline(aes(xintercept = as.numeric(date[c(1622)])), color="green")

# piecemeal approach: divide time series into eras
ts_crash <- ts_crash %>%
  mutate(era = ifelse(daysSince < 1296, "1",  # AUG 27 2019
                      ifelse(daysSince < 1501, "2",  # MAR 19 2020
                             ifelse(daysSince < 1605, "3",  # JULY 1 2020
                                    ifelse(daysSince < 1675, "4", "5")))))  
# SEPT 10 2020

mod1 <- lm(count ~ weekday * era, data = ts_crash)

ts_eras <- ts_crash %>%
  add_residuals(mod1)
ts_eras_vis <- ts_eras %>%
  ggplot(aes(date, resid)) +
  geom_ref_line(h = 0) +
  geom_line() +
  geom_vline(aes(xintercept = as.numeric(date[c(1291)])), color="green")+
  geom_vline(aes(xintercept = as.numeric(date[c(1494)])), color="green")+
  geom_vline(aes(xintercept = as.numeric(date[c(1597)])), color="green")+
  geom_vline(aes(xintercept = as.numeric(date[c(1622)])), color="green")

# While phase 4 represents mostly missing data, 
# the counts generally increase and variation increases
ts_crash %>% ggplot(aes(weekday, count, color=era)) +
  geom_boxplot()
ts_crash %>% ggplot(aes(era, count)) +
  geom_boxplot()

##### NJ Accident Map #################################

northmost <- 41.37
southmost <- 39

nj <- accident_data[, c(5,6,56)] %>%
  filter(Start_Lat > southmost & Start_Lat < northmost & 
           Start_Lng < -73.78 & Start_Lng > -75.745)

# eps = 25 meters converted to degrees Lat/Long
# m * (km/m) * (deg/km) = degrees
eps <- 21 * 1/1000 * 90/10000

Dbscan_cl <- dbscan(nj[, c(1:2)], eps, minPts = 15)

#pairs(njSpatial, col = Dbscan_cl$cluster + 1L)
nj <- nj %>%
  mutate(cluster = Dbscan_cl$cluster +1L)
nj <- nj %>%
  mutate(isCluster = as.logical(Dbscan_cl$cluster))

nj_clusters <- nj %>% 
  filter(isCluster)

nj_season <- nj %>%
  filter(season == "summer" | season == "winter") %>%
  mutate(season = season == "summer") %>%
  filter(isCluster) %>%
  group_by(cluster) %>%
  summarise(Latitude = mean(Start_Lat),
            Longitude = mean(Start_Lng),
            season = mean(season))

imageNJ <- png::readPNG("nj_surround.png")
## All NJ Collisions ##  
ggplot(data = nj, mapping=aes(x=Start_Lng, y=Start_Lat))+
  ylim(southmost, northmost)+
  ylab("Latitude")+
  xlab("Longitude")+
  ggtitle("All Accident Sites around New Jersey, 2016-2020")+
  annotation_custom(rasterGrob(imageNJ, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf)+
  geom_point(alpha = 1/45) 

imageNJ <- png::readPNG("nj_surround.png")
## NJ Collisions by Season ##
ggplot(data = nj_clusters, mapping = aes(x=Start_Lng, y=Start_Lat, color = season)) +
  ylim(southmost, northmost)+
  theme_classic() +
  ylab("Latitude")+
  xlab("Longitude")+
  ggtitle("New Jersey Frequent Collision Sites by Season")+
  annotation_custom(rasterGrob(imageNJ, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_point(data = filter(nj_clusters, season == "fall"), color = "yellow", size = 2) +
  geom_point(data = filter(nj_clusters, season == "winter"), color = "steelblue", size = 2.5) +
  geom_point(aes(color = season), alpha = 1/2) +
  scale_color_manual(limits = c("spring", "summer", "fall", "winter"),
                     values = c("green", "orangered", "yellow", "steelblue")) 



## Sites of Frequent Summer and Winter Collisions ##
ggplot(data = nj_season, mapping=aes(x=Longitude, y=Latitude, color= season))+
  ylim(southmost, northmost)+
  labs(color = "% Collisions\nOccurring\nin Summer") +
  ylab("Latitude")+
  xlab("Longitude")+
  ggtitle("New Jersey Frequent Collision Sites, Summer and Winter",
          "One dot represents 15 collisions within a 21-meter radius over 4 years")+
  annotation_custom(rasterGrob(imageNJ, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf)+
  geom_point() +
  scale_color_gradient2(low = "steelblue", mid = "plum4", high = "orangered", midpoint=0.5)


###########################################################

##~~~~~~##
imageUS <- jpeg::readJPEG("us_roads.jpg")

us_spatial <- accident_data[, c(5,6,56)] 
# eps = 25 meters converted to degrees Lat/Long
# m * (km/m) * (deg/km) = degrees
eps <- 21 * 1/1000 * 90/10000
Dbscan_cl_us <- dbscan(us_spatial[,c(1:2)], eps = eps, minPts = 15)

us_spatial <- us_spatial %>%
  mutate(cluster = Dbscan_cl_us$cluster +1L,
         isCluster = as.logical(Dbscan_cl_us$cluster)) %>%
  filter(isCluster) 

us_summer_winter <- us_spatial %>%
  filter(season == "winter" | season == "summer") %>%
  mutate(season = as.numeric(season == "summer")) %>%
  group_by(cluster) %>%
  summarize(season = mean(season),
            Lat = mean(Start_Lat),
            Long = mean(Start_Lng))

## US Summer and Winter ##
imageUS <- png::readPNG("us_roads3.png")
ggplot(us_summer_winter, aes(x=Long, y=Lat, color = season)) +
  ggtitle("Most Frequent Winter and Summer Accident Sites in the Continental US",
          "One dot represents 15 accidents between 2016 and 2020")+
  labs(color = "% Collisions\n occurring \nin Summer") +
  xlab("Longitude") +
  ylab("Latitude") +
  annotation_custom(rasterGrob(imageUS, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_point() +
  scale_color_gradient2(low = "steelblue", mid = "plum4", high = "orangered", midpoint=0.5)

## US by Season ##
imageUS <- png::readPNG("usroads2.png")
ggplot(us_spatial, aes(x=Start_Lng, y=Start_Lat
                       , color = season
)) +
  ggtitle("Most Frequent Accident Sites in the Continental US by Season")+
  xlab("Longitude") +
  ylab("Latitude") +
  annotation_custom(rasterGrob(imageUS, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_point(data = filter(us_spatial, season == "winter"), color = "steelblue", shape = 16) +
  geom_point(data = filter(us_spatial, season == "fall"), color = "yellow", shape = 16) +
  geom_point(data = filter(us_spatial, season == "fall"), color = "goldenrod", shape = 1) +
  geom_point(data = filter(us_spatial, season == "summer"), color = "orangered", shape = 1) +
  geom_point(data = filter(us_spatial, season == "fall"), color = "green", shape = 1, alpha=1/2) #+
# geom_point(shape = 1) +
# scale_color_manual(limits = c("spring", "summer", "fall", "winter"),
#                    values = c("green", "orangered", "goldenrod", "steelblue"))

## US Frequent Collision Sites ##
imageUS <- png::readPNG("us_roads3.png")
ggplot(us_spatial, aes(x=Start_Lng, y=Start_Lat), color = "royalblue") +
  ggtitle("Most Frequent Accident Sites in the Continental US")+
  xlab("Longitude") +
  ylab("Latitude") +
  annotation_custom(rasterGrob(imageUS, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_point(alpha = 1/60, color = "blue")

##~~~~~~##

######## Accidents by state population ############
stVar <- tidyAccident %>%
  group_by(State) %>%
  summarise(count = n()) %>%
  mutate(State = as.character(State))

# 2020 population data with state abbrev.
pop <- read_csv("ST-POP21.csv")
pop <- pop[,1:2] %>% 
  mutate(State = mapply(function(x) state.abb[grep(x, state.name)], State))
pop$State[47] <- "VA"
pop$State[9] <- "DC"

# join population and crashes, calculate accidents per 100k
pop <- pop %>% mutate(State = as.character(State))
stVar <- stVar %>% mutate(State = as.character(State))
stVar<- left_join(stVar, pop, by = "State")
stVar <- stVar %>% mutate(pop_per_100k = `2020_pop_est`/100000,
                          Accidents_per_100k = count/pop_per_100k)

stVar$State <- state.name[match(stVar$State, state.abb)]

states <- ggplot2::map_data("state")
states <- states %>%
  mutate(region = stri_trans_totitle(region))
stVar <- stVar %>% rename(region = State)
map.crash <- merge(states, stVar, by="region", all.x=T)
map.crash <- map.crash[order(map.crash$order),]

ggplot(map.crash, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill = Accidents_per_100k)) +
  geom_path()+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map()
