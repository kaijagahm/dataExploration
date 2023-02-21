# Exploring movement for the purpose of parameterizing BCRW's

library(tidyverse)
library(vultureUtils)
library(igraph)
library(feather)
library(sf)
library(ggraph)
library(tidygraph)
library(wesanderson)
library(mapview)
library(MASS)
library(amt) # animal movement tools, for calculating movement stats
source("permutations/funs_datastreamPermutations.R")

roostPolygons <- sf::st_read("data/roosts25_cutOffRegion.kml")

load("data/months.Rda")

# Just one week of data
testWeek <- months[[1]] %>%
  filter(lubridate::ymd(dateOnly) >= lubridate::ymd("2022-01-01") & lubridate::ymd(dateOnly) <= lubridate::ymd("2022-01-07"))

mapview(testWeek, zcol = "trackId")


# Just southern indivs ----------------------------------------------------
## get centroids
centroids <- testWeek %>%
  group_by(trackId) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_centroid()

## visualize centroids
mapview(centroids)
hist(st_coordinates(centroids)[,2]) # plot latitudes to find a good cutoff point

## filter to only southern indivs
southernIndivs <- centroids %>%
  filter(st_coordinates(.)[,2] < 32) %>%
  pull(trackId)

testWeekSouthern <- testWeek %>%
  filter(trackId %in% southernIndivs)

mapview(testWeekSouthern, zcol = "trackId")

## get only daylight points
times <- suncalc::getSunlightTimes(date = unique(lubridate::date(testWeekSouthern$timestamp)), lat = 31.434306, lon = 34.991889,
                                   keep = c("sunrise", "sunset")) %>%
  dplyr::select(date, sunrise, sunset) # XXX the coordinates I'm using here are from the centroid of Israel calculated here: https://rona.sh/centroid. This is just a placeholder until we decide on a more accurate way of doing this.
testWeekSouthern <- testWeekSouthern %>%
  dplyr::left_join(times, by = c("dateOnly" = "date")) %>%
  dplyr::mutate(daytime = dplyr::case_when(timestamp > sunrise &
                                             timestamp < sunset ~ T,
                                           TRUE ~ F))
testWeekSouthern %>% filter(dateOnly == "2022-01-04") %>% mapview(zcol = "trackId")
mapview(testWeekSouthern, zcol = "trackId")

## Determine a reasonable extent
bb <- st_transform(testWeekSouthern, 32636) %>% st_bbox()
latRange <- bb[4]-bb[2]
lonRange <- bb[3]-bb[1]
max(latRange, lonRange) # looks like we can use approx 120000 as our scale dimension.

# get starting scale
southernCentroids <- centroids %>%
  filter(st_coordinates(.)[,2] < 32)
bbCentroids <- st_transform(southernCentroids, 32636) %>% st_bbox()
latRangeCentroids <- bbCentroids[4]-bbCentroids[2]
lonRangeCentroids <- bbCentroids[3]-bbCentroids[1]
max(latRangeCentroids, lonRangeCentroids) # around 101000. Let's just call it 100000. That's 83% of the original home range. So the scale factor should be 1.2. Note that in the real data, unlike in Orr's simulation, the starting points take up most of the area.

tracks <- testWeekSouthern %>%
  sf::st_drop_geometry() %>%
  group_by(trackId, dateOnly) %>%
  filter(!duplicated(timestamp)) %>%
  ungroup() %>%
  amt::make_track(location_long, location_lat, .t = timestamp, trackId = trackId, crs = "WGS84", dateOnly = dateOnly, timestamp = timestamp) %>%
  transform_coords(., st_crs("EPSG:32636"))

# XXX start here with calculating steps
steps <- tracks %>%
  group_by(trackId, dateOnly) %>%
  filter(n() > 1) %>% # remove days with only one point
  group_split(.keep = T) %>%
  purrr::map_dfr(., ~steps(.x, keep_cols = "start"))

steps %>%
  ggplot(aes(x = log(sl_), fill = factor(trackId)))+
  geom_density(alpha = 0.4)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Step length (log-transformed)")+
  ggtitle("Log step lengths")

steps %>%
  ggplot(aes(x = ta_, fill = factor(trackId)))+
  geom_density(alpha = 0.4)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Turning angle")+
  ggtitle("Turning angles")

## Manually calculate displacement and daily travel distance
tracksSF <- tracks %>%
  sf::st_as_sf(coords = c("x_", "y_")) %>%
  sf::st_set_crs("32636")

dailyMovement <- tracksSF %>%
  group_by(trackId, dateOnly) %>%
  summarize(displacement = st_distance(geometry, geometry[1]),
            lead = geometry[row_number()+1],
            difftime_s = difftime(timestamp, lag(timestamp), units = "secs"),
            dist_m = st_distance(geometry, lead, by_element = T),
            speed_ms = as.numeric(dist_m)/as.numeric(difftime_s))

dailyMovementStats <- dailyMovement %>%
  group_by(trackId, dateOnly) %>%
  summarize(mxDisp = max(displacement, na.rm = T),
            totalDist = sum(dist_m),
            mnSpeed = mean(speed_ms, na.rm = T),
            mnDistPer10min = mnSpeed*600,
            sdSpeed = sd(speed_ms, na.rm  =T),
            sdDistPer10min = sdSpeed*600)

mean(dailyMovementStats$mnDistPer10min, na.rm  =T) # mean distance
mean(dailyMovementStats$sdDistPer10min, na.rm = T) # sd distance

dailyMovement %>%
  ggplot(aes(x = totalDistance_m, y = maxDisplacement_m, col = dateOnly, group = dateOnly))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()

dailyMovement %>%
  ggplot(aes(x = totalDistance_m, col = dateOnly, group = dateOnly))+
  geom_density()+
  theme_classic()

dailyDisplacements %>%
  ggplot(aes(x = displacement, col = dateOnly, group = dateOnly))+
  geom_density()+
  theme_classic()

mean(dailyDisplacements$displacement) #let's just say 8000 meters for now.
sd(dailyDisplacements$displacement) # we'll just say 15000 meters for now



