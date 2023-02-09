# Permutations test script: Just 2022 data

library(tidyverse)
library(vultureUtils)
library(igraph)
library(feather)
library(sf)
library(ggraph)
library(tidygraph)
source("funs_datastreamPermutations.R")

load("data/datAnnotCleaned.Rda")
roostPolygons <- sf::st_read("data/roosts25_cutOffRegion.kml")

# Goal: year 2022, including December 2021 but excluding December 2022. To get this, we need to pull years 2021 and 2022 and then filter down.
# Extract only 2021 and 2022
tt <- datAnnotCleaned %>% filter(lubridate::year(timestamp) %in% c(2021, 2022))

# make this into an sf object:
tt <- tt %>%
  sf::st_as_sf(coords = c("location_long.1", "location_lat.1")) %>%
  sf::st_set_crs("WGS84") %>%
  mutate(timeOnly = stringr::str_extract(as.character(timestamp), "[0-9]{2}:[0-9]{2}:[0-9]{2}"))
roostPolygons <- sf::st_read("data/roosts25_cutOffRegion.kml", quiet = TRUE) %>%
  sf::st_transform("WGS84") %>%
  select(-c("Name", "Description"))

# Breeding: December through June
# Non-breeding: July through November
tt <- tt %>%
  mutate(season = case_when(lubridate::month(timestamp) %in% c(1:6) ~ "breeding_thisyear",
                            lubridate::month(timestamp) == 12 ~ "breeding_nextyear",
                            lubridate::month(timestamp) %in% c(7:11) ~ "nonbreeding",
                            TRUE ~ NA_character_),
         year = lubridate::year(timestamp),
         seasonUnique = case_when(season == "breeding_thisyear" ~ paste(year, "breeding", sep = "_"),
                                  season == "breeding_nextyear" ~ paste(year+1, "breeding", sep = "_"),
                                  season == "nonbreeding" ~ paste(year, "nonbreeding", sep = "_")))

# split by season and keep only 21-22 breeding and 22 nonbreeding
tt <- tt %>%
  filter(grepl("2022", seasonUnique))

seasons <- tt %>%
  group_by(seasonUnique) %>%
  group_split(.keep = T)

# alternatively, split by month
months <- tt %>%
  group_by("month" = lubridate::month(timestamp)) %>%
  group_split(.keep = T)

# get roosts
# monthRoosts <- purrr::imap(months, ~{
#   start <- Sys.time()
#   cat(paste("Running iteration", .y, "\n"))
#   out <- vultureUtils::get_roosts_df(df = .x, id = "trackId")
#   end <- Sys.time()
#   dur <- difftime(end, start, units = "mins")
#   cat(paste("Completed iteration", .y, "in", dur, "minutes.\n"))
#   return(out)
# })
# save(monthRoosts, file = "data/monthRoosts.Rda")
load("data/monthRoosts.Rda")

# By month, create networks (feeding, flying, two types of roosting)
#monthsFlight_2022 <- purrr::map(months, ~vultureUtils::getFlightEdges(dataset = .x, roostPolygons = roostPolygons, distThreshold = 1000, return = "both"))
#save(monthsFlight_2022, file = "data/monthsFlight_2022.Rda")
load("data/monthsFlight_2022.Rda")

#monthsFeeding_2022 <- purrr::map(months, ~vultureUtils::getFeedingEdges(dataset = .x, roostPolygons = roostPolygons, distThreshold = 50, return = "both"))
#save(monthsFeeding_2022, file = "data/monthsFeeding_2022.Rda")
load("data/monthsFeeding_2022.Rda")

#months_roost_distance <- purrr::map(monthRoosts, ~vultureUtils::getRoostEdges(dataset = .x, mode = "distance", distThreshold = 500, return = "both"))
#save(months_roost_distance, file = "data/months_roost_distance.Rda")
load("data/months_roost_distance.Rda")

#months_roost_polygon <- purrr::map(monthRoosts, ~vultureUtils::getRoostEdges(dataset = .x, mode = "polygon", roostPolygons = roostPolygons, return = "both"))
#save(months_roost_polygon, file = "data/months_roost_polygon.Rda")
load("data/months_roost_polygon.Rda")

# Create network graphs for each of these
allVerts <- unique(tt$trackId)

g_flight <- map(monthsFlight_2022, ~vultureUtils::makeGraph(mode = "sri", data = .x$sri, weighted = T, vertices = allVerts))
g_feeding <- map(monthsFeeding_2022, ~vultureUtils::makeGraph(mode = "sri", data = .x$sri, weighted = T))
g_roost_distance <- map(months_roost_distance, ~vultureUtils::makeGraph(mode = "sri", data = .x$sri, weighted = T))
g_roost_polygon <- map(months_roost_polygon, ~vultureUtils::makeGraph(mode = "sri", data = .x$sri, weighted = T))

# Make some plots using igraph
test <- g_flight[[1]]
plot(test, vertex.size = 10, edge.width = 30*E(test)$weight, edge.color = "black")
allVerts <- map(g_flight, ~V(.x)) %>% unlist() %>% names() %>% unique()
flightLayout <- layout_with_fr(graph = g_flight[[1]])
plot(test, vertex.size = 10, edge.width = 30*E(test)$weight, edge.color = "black", vertices = allVerts)

# Test data for permutations--let's use just one week of data.
testWeek <- months[[1]] %>%
  filter(lubridate::ymd(dateOnly) >= lubridate::ymd("2022-01-01") & lubridate::ymd(dateOnly) <= lubridate::ymd("2022-01-07"))

# Get roosts for this test week
testWeekRoosts <- vultureUtils::get_roosts_df(df = testWeek, id = "trackId")
save(testWeekRoosts, file = "data/testWeekRoosts.Rda")

# Get all four types of network
getNetworks <- function(dataset, roosts, roostPolygons){
  testWeekFlight <- getFlightEdges(dataset, roostPolygons, distThreshold = 1000, return = "both")
  testWeekFeeding <- getFeedingEdges(dataset, roostPolygons, distThreshold = 50, return = "both")
  testWeekRoostD <- getRoostEdges(roosts, mode = "distance", idCol = "trackId", return = "both", distThreshold = 200)
  testWeekRoostP <- getRoostEdges(roosts, mode = "polygon", roostPolygons = roostPolygons, idCol = "trackId", return = "both")
  outList <- list("flight" = testWeekFlight, "feeding" = testWeekFeeding, "roostD" = testWeekRoostD, "roostP" = testWeekRoostP)
}

testWeekNetworks <- getNetworks(dataset = testWeek, roosts = testWeekRoosts, roostPolygons = roostPolygons)
save(testWeekNetworks, file = "data/testWeekNetworks.Rda")

# Make graphs
testWeekGraphs <- map(testWeekNetworks, ~makeGraph(mode = "sri", data = .x$sri, weighted = T))

# Make plots
plot(testWeekGraphs$flight, vertex.size = 15, edge.width = 20*E(testWeekFlight_g)$weight, vertex.label = NA, vertex.color = "lightblue", main = "co-flight")
plot(testWeekGraphs$feeding, vertex.size = 15, edge.width = 20*E(testWeekFeeding_g)$weight, vertex.label = NA,
     vertex.color = "orange", main = "co-feeding")
plot(testWeekGraphs$roostD, vertex.size = 15, edge.width = E(testWeekRoostD_g)$weight, vertex.label = NA, vertex.color = "firebrick3", main = "co-roosting (distance)")
plot(testWeekGraphs$roostP, vertex.size = 15, edge.width = E(testWeekRoostP_g)$weight, vertex.label = NA,
     vertex.color = "firebrick3", main = "co-roosting (shared polygons)")

# PERMUTATIONS
nperm <- 10

## PERMUTATION 1: Random shuffle
# shuffled <- map(1:nperm, ~p_randomDays(dataset = testWeek, idCol = "trackId", dateCol = "dateOnly", timeCol = "timeOnly"), .progress = T)
# save(shuffled, file = "data/shuffled.Rda")
# shuffledRoosts <- map(shuffled, ~vultureUtils::get_roosts_df(df = .x, id = "trackId", quiet = T), .progress = T)
# save(shuffledRoosts, file = "data/shuffledRoosts.Rda")
# shuffledNetworks <- map(shuffled, ~getNetworks(dataset = .x, roosts = testWeekRoosts, roostPolygons = roostPolygons), .progress = T)
# save(shuffledNetworks, file = "data/shuffledNetworks.Rda")
load("data/shuffled.Rda")
load("data/shuffledRoosts.Rda")
load("data/shuffledNetworks.Rda")
shuffledGraphs <- map(shuffledNetworks, ~map(.x, ~makeGraph(mode = "sri", data = .x$sri, weighted = T)))

## PERMUTATION 2: Shift
# shifted <- map(1:nperm, ~p_shift(dataset = testWeek, shiftMax = 5, idCol = "trackId", dateCol = "dateOnly", timeCol = "timeOnly"))
# save(shifted, file = "data/shifted.Rda")
# shiftedRoosts <- map(shifted, ~vultureUtils::get_roosts_df(df = .x, id = "trackId", quiet = T), .progress = T)
# save(shiftedRoosts, file = "data/shiftedRoosts.Rda")
# shiftedNetworks <- map(shifted, ~getNetworks(dataset = .x, roosts = testWeekRoosts, roostPolygons = roostPolygons), .progress = T)
# save(shiftedNetworks, file = "data/shiftedNetworks.Rda")
load("data/shifted.Rda")
load("data/shiftedRoosts.Rda")
load("data/shiftedNetworks.Rda")
shiftedGraphs <- map(shiftedNetworks, ~map(.x, ~makeGraph(mode = "sri", data = .x$sri, weighted = T)))

## PERMUTATION 3: Conveyor belt
# conveyor <- map(1:nperm, ~p_conveyor(dataset = testWeek, mode = "global", shiftMax = 5, idCol = "trackId", dateCol = "dateOnly", timeCol = "timeOnly"))
# save(conveyor, file = "data/conveyor.Rda")
#conveyorRoosts <- map(conveyor, ~vultureUtils::get_roosts_df(df = .x, id = "trackId", quiet = T), .progress = T)
#save(conveyorRoosts, file = "data/conveyorRoosts.Rda")
# conveyorNetworks <- map(conveyor, ~getNetworks(dataset = .x, roosts = testWeekRoosts, roostPolygons = roostPolygons), .progress = T)
# save(conveyorNetworks, file = "data/conveyorNetworks.Rda")
load("data/conveyor.Rda")
load("data/conveyorRoosts.Rda")
load("data/conveyorNetworks.Rda")
conveyorGraphs <- map(conveyorNetworks, ~map(.x, ~makeGraph(mode = "sri", data = .x$sri, weighted = T)))














# Clearly I need help with igraph. But in the meantime, let's do some permutations!
# To make this tractable, I'm just going to deal with one month of data.
map(months, dim)
testMonth <- months[[10]] # this one has a lot of rows.

## TYPE 1: Random day permutation. For each individual, I'll restrict it to only the days when it was actually observed, preserving real gaps. But I'll shuffle day tracks within it. 
randomDays_permuted <- p_randomDays(dataset = testMonth, idCol = "trackId", dateCol = "dateOnly", timeCol = "timeOnly")

## Type 2: Shift tracks back or forwards. For each individual, randomly choose number of days to shift, between -10 and 10, with uniform probability. Then create the new date column based on that.
shiftedDays_permuted <- p_shift(dataset = testMonth, shiftMax = 10, idCol = "trackId", dateCol = "dateOnly", timeCol = "timeOnly")

## Type 3: Conveyor belt. Shift tracks backwards or forwards and loop around if they fall off the edges of the date period. For each individual, randomly choose number of days to shift, where the shift is limited to the number of days in the date range. From -10 to 10.
shiftedDays_conveyor <- p_conveyor(dataset = testMonth, mode = "global", shiftMax = 10, idCol = "trackId", dateCol = "dateOnly", timeCol = "timeOnly")

# Let's visualize the shifted vs. normal days
testMonth %>%
  sf::st_drop_geometry() %>%
  select(trackId, dateOnly) %>%
  distinct()%>%
  ggplot(aes(x = trackId, y = dateOnly))+
  geom_point(size = 1.5)+
  geom_line(linewidth = 0.7)+
  coord_flip()+
  theme_classic()+
  ggtitle("Original data")+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  xlab("Individual")+
  ylab("Actual date (2022)")

shiftedDays_permuted %>%
  sf::st_drop_geometry() %>%
  select(trackId, dateOnly) %>%
  distinct()%>%
  ggplot(aes(x = trackId, y = dateOnly))+
  geom_point(size = 1.5, col = "darkred")+
  geom_line(linewidth = 0.7)+
  coord_flip()+
  theme_classic()+
  ggtitle("Shifted data")+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  xlab("Individual")+
  ylab("Shifted date (2022)")

shiftedDays_permuted_long <- shiftedDays_permuted %>%
  sf::st_drop_geometry() %>%
  pivot_longer(cols = c("dateOnly", "oldDate"), names_to = "dateType", values_to = "date") %>%
  mutate(dateType = case_when(dateType == "oldDate" ~ "Original",
                              dateType == "dateOnly" ~ "Shifted",
                              TRUE ~ NA_character_))

original_and_shifted_10Days <- shiftedDays_permuted_long %>%
  select(trackId, dateType, date) %>%
  distinct() %>%
  ggplot(aes(x = trackId, y = date))+
  geom_line(linewidth = 0.7)+
  geom_point(size = 1.5, aes(col = dateType))+
  theme_classic()+
  ggtitle("Original and shifted data")+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  xlab("Individual")+
  ylab("Date (2022)")+
  coord_flip()+
  facet_wrap(~dateType, scales = "fixed")+
  theme(legend.position = "none")+
  scale_color_manual(values = c("black", "firebrick4"))
ggsave(plot = original_and_shifted_10Days, filename = "fig/original_and_shifted_10Days.png",
       width = 6, height = 9)

# Put all the datasets together
datasets <- list("shuffled" = randomDays_permuted, 
                 "shifted" = shiftedDays_permuted,
                 "orig" = testMonth)
# Now I can actually make networks for this data. Going to start with just regular and shifted.
# flight
# flight <- map(datasets, ~vultureUtils::getFlightEdges(dataset = .x, roostPolygons = roostPolygons, return = "sri"))
# save(flight, file = "data/flight.Rda")
load("data/flight.Rda")

# feeding
# feeding <- map(datasets, ~vultureUtils::getFeedingEdges(dataset = .x, roostPolygons = roostPolygons, return = "sri"))
# save(feeding, file = "data/feeding.Rda")
load("data/feeding.Rda")

# get roosts # XXX this may be a problem--should look at how Marta's algorithm responds to teleportation.
# roosts <- map(datasets, ~vultureUtils::get_roosts_df(df = .x, id = "trackId"))
# save(roosts, file = "data/roosts.Rda")
load("data/roosts.Rda")

# roost (distance)
# roostsD <- map(roosts, ~vultureUtils::getRoostEdges(dataset = .x, mode = "distance", distThreshold = 500, return = "sri"))
# save(roostsD, file = "data/roostsD.Rda")
load("data/roostsD.Rda")

# roost (polygon)
# roostsP <- map(roosts, ~vultureUtils::getRoostEdges(dataset = .x, mode = "polygon", roostPolygons = roostPolygons, return = "sri"))
# save(roostsP, file = "data/roostsP.Rda")
load("data/roostsP.Rda")

# Make graphs
flight_g <- map(flight, ~makeGraph(mode = "sri", data = .x, weighted = T))
feeding_g <- map(feeding, ~makeGraph(mode = "sri", data = .x, weighted = T))
roostD_g <- map(roostsD, ~makeGraph(mode = "sri", data = .x, weighted = T))
roostP_g <- map(roostsP, ~makeGraph(mode = "sri", data = .x, weighted = T))

# Plot
iwalk(flight_g, ~plot(.x, vertex.size = 10, edge.width = 20*E(.x)$weight, edge.color = "black", main = paste("flight", .y)))

iwalk(feeding_g, ~plot(.x, vertex.size = 10, edge.width = 20*E(.x)$weight, edge.color = "black", main = paste("feeding", .y)))

iwalk(roostD_g, ~plot(.x, vertex.size = 10, edge.width = 10*E(.x)$weight, edge.color = "black", main = paste("roostD", .y)))

iwalk(roostP_g, ~plot(.x, vertex.size = 10, edge.width = 10*E(.x)$weight, edge.color = "black", main = paste("roostP", .y)))
