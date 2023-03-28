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
library(fitdistrplus)
library(amt) # animal movement tools, for calculating movement stats
source("permutations/funs_datastreamPermutations.R")

roostPolygons <- sf::st_read("data/roosts25_cutOffRegion.kml")

load("data/months.Rda")

# Just one mpnth of data
testMonth <- months[[1]]

# Just southern indivs ----------------------------------------------------
## get centroids
centroids <- testMonth %>%
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

testMonthSouthern <- testMonth %>%
  filter(trackId %in% southernIndivs)

#mapview(testMonthSouthern, zcol = "trackId")

## get only daylight points
times <- suncalc::getSunlightTimes(date = unique(lubridate::date(testMonthSouthern$timestamp)), lat = 31.434306, lon = 34.991889,
                                   keep = c("sunrise", "sunset")) %>%
  dplyr::select(date, sunrise, sunset) # XXX the coordinates I'm using here are from the centroid of Israel calculated here: https://rona.sh/centroid. This is just a placeholder until we decide on a more accurate way of doing this.
testMonthSouthern <- testMonthSouthern %>%
  dplyr::left_join(times, by = c("dateOnly" = "date")) %>%
  dplyr::mutate(daytime = dplyr::case_when(timestamp > sunrise &
                                             timestamp < sunset ~ T,
                                           TRUE ~ F))

## Determine a reasonable extent
bb <- st_transform(testMonthSouthern, 32636) %>% st_bbox()
latRange <- bb[4]-bb[2]
lonRange <- bb[3]-bb[1]
max(latRange, lonRange) # looks like we can use approx 120000 or 130000 as our scale dimension.

# get starting scale
southernCentroids <- centroids %>%
  filter(st_coordinates(.)[,2] < 32)
bbCentroids <- st_transform(southernCentroids, 32636) %>% st_bbox()
latRangeCentroids <- bbCentroids[4]-bbCentroids[2]
lonRangeCentroids <- bbCentroids[3]-bbCentroids[1]
max(latRangeCentroids, lonRangeCentroids) # around 101000. Let's just call it 100000. That's 83% of the original home range. So the scale factor should be 1.2. Note that in the real data, unlike in Orr's simulation, the starting points take up most of the area.

tracks <- testMonthSouthern %>%
  sf::st_drop_geometry() %>%
  group_by(trackId, dateOnly) %>%
  filter(!duplicated(timestamp)) %>%
  ungroup() %>%
  amt::make_track(location_long, location_lat, .t = timestamp, trackId = trackId, crs = "WGS84", dateOnly = dateOnly, timestamp = timestamp) %>%
  transform_coords(., st_crs("EPSG:32636"))

steps <- tracks %>%
  group_by(trackId, dateOnly) %>%
  filter(n() > 1) %>% # remove days with only one point
  group_split(.keep = T) %>%
  purrr::map_dfr(., ~steps(.x, keep_cols = "start"))

steps %>%
  ggplot(aes(x = sl_, fill = factor(trackId)))+
  geom_density(alpha = 0.4)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Step length")+
  ggtitle("Step lengths")

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
            speed_ms = as.numeric(dist_m)/as.numeric(difftime_s),
            dist_10minEquiv = speed_ms*600)

## plot 10min equivalent distances (i.e. step length corrected for time interval)
dailyMovement %>%
  ggplot(aes(x = dist_10minEquiv))+
  geom_histogram()+
  theme_classic()+
  theme(legend.position = "none") # even after this correction, the step lengths are still *extremely* right-skewed. Need to change the step selection portion of the model to account for this. 

# Let's fit a distribution. I think a gamma will be most reasonable.
### have to divide by 10 because the distribution can't be fitted otherwise. I followed the steps shown here: https://stackoverflow.com/questions/53557022/error-code-100-fitting-exp-distribution-using-fitdist-in-r
fitGamma <- fitdistrplus::fitdist(dailyMovement$dist_10minEquiv[!is.na(dailyMovement$dist_10minEquiv) & dailyMovement$dist_10minEquiv > 0]/10, distr = "gamma")
plot(fitGamma, las = 1) # that's actually a pretty bad fit.

fitExp <- fitdistrplus::fitdist(dailyMovement$dist_10minEquiv[!is.na(dailyMovement$dist_10minEquiv) & dailyMovement$dist_10minEquiv > 0]/10, distr = "exp")
plot(fitExp, las = 1) # oof, that's an even worse fit.
## Looks like I will have to draw the values from a non-parametric distribution. But for now, I'm just going to use the gamma, because we need to move forward and it doesn't have to be perfect.

fitGamma # shape = 0.305, rate = 0.003

# Actual data -------------------------------------------------------------
# I want to look at some actual data next to the modeled data to see how it compares
# Center the data so it's more directly comparable
tracksScaled <- tracks %>%
  mutate(x_ = x_-mean(x_),
         y_ = y_-mean(y_))

tracksScaled %>%
  ggplot(aes(x = x_, y = y_, col = factor(trackId)))+
  geom_path(linewidth = 0.5, aes(group = factor(trackId)))+
  theme_minimal()+
  theme(legend.position = "none")+
  coord_equal()+
  ggtitle("Real Agents") # as I suspected--highly correlated to geography, and they move together. 

# How do home ranges shift over the days?
dailyCentroids <- tracksSF %>%
  group_by(trackId, dateOnly) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_centroid()

polys <- tracksSF %>%
  filter(trackId == "A03w") %>%
  group_by(trackId, dateOnly) %>%
  summarize(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

tracksSF %>%
  filter(trackId == "A03w") %>%
  ggplot()+
  geom_sf(data = polys, aes(fill = as.factor(dateOnly), col = as.factor(dateOnly)), alpha = 0.1)+
  geom_sf(aes(col = as.factor(dateOnly)), alpha = 0.5, size = 2)+
  theme_minimal()+
  geom_sf(data = dailyCentroids %>% filter(trackId == "A03w"), aes(col = factor(dateOnly)), size = 5, pch = 8)+
  theme(legend.position = "none")

polys %>%
  ggplot()+
  geom_sf(aes(fill = as.factor(dateOnly), col = as.factor(dateOnly)), alpha = 0.1)+
  theme_minimal()+
  theme(legend.position = "none")

dailyCentroids %>%
  filter(trackId == "A03w") %>%
  ggplot()+
  geom_sf(aes(col = as.factor(dateOnly)))+
  theme_minimal()+
  theme(legend.position = "none")
