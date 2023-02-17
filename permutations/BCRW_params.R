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
source("funs_datastreamPermutations.R")

roostPolygons <- sf::st_read("data/roosts25_cutOffRegion.kml")

load("data/months.Rda")

testWeek <- months[[1]] %>%
  filter(lubridate::ymd(dateOnly) >= lubridate::ymd("2022-01-01") & lubridate::ymd(dateOnly) <= lubridate::ymd("2022-01-07"))

mapview(testWeek, zcol = "trackId")

tracks <- testWeek %>%
  sf::st_drop_geometry() %>%
  group_by(trackId) %>%
  filter(!duplicated(timestamp)) %>%
  ungroup() %>%
  amt::make_track(location_long, location_lat, .t = timestamp, trackId = trackId, crs = "WGS84", dateOnly = dateOnly, timestamp = timestamp) %>%
  transform_coords(., st_crs("EPSG:32636"))

steps <- tracks %>%
  nest(data = -"trackId") %>%
  mutate(steps = map(data, ~steps(.x))) %>%
  unnest(cols = steps)

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


# home ranges for each animal (https://cran.r-project.org/web/packages/amt/vignettes/p2_hr.html)
## Select two focal animals: A75w and E17w
a <- tracks %>% filter(trackId == "A75w")
b <- tracks %>% filter(trackId == "E17w")

## Create template rasters
trast <- make_trast(tracks %>% filter(trackId  %in% c("A75w", "E17w")), res = 50)

## Estimate home ranges
# hr_a <- hr_kde(a, trast = trast, levels = 0.9)
# hr_b <- hr_kde(b, trast = trast, levels = 0.9)
# plot(hr_a, main = "Home range of A75w")
# plot(hr_b, main = "Home range of E17w")

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



