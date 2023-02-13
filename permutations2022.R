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

#testWeekNetworks <- getNetworks(dataset = testWeek, roosts = testWeekRoosts, roostPolygons = roostPolygons)
#save(testWeekNetworks, file = "data/testWeekNetworks.Rda")
load("data/testWeekNetworks.Rda")

# Make graphs
testWeekGraphs <- map(testWeekNetworks, ~makeGraph(mode = "sri", data = .x$sri, weighted = T))
testWeekGraphs0 <- map(testWeekGraphs, ~delete.edges(.x, E(.x)[E(.x)$weight <= 0|is.na(E(.x)$weight)])) # remove 0-weight edges for plotting/calcs

# Make plots
plot(testWeekGraphs0$flight, vertex.size = 15, edge.width = 20*E(testWeekGraphs0$flight)$weight, vertex.color = "lightblue", main = "co-flight", vertex.label.color = "black", vertex.label.cex = 0.8, vertex.label.font = 2, vertex.frame.color = "lightblue", vertex.label.family = "Arial")
plot(testWeekGraphs0$feeding, vertex.size = 15, edge.width = 20*E(testWeekGraphs0$feeding)$weight, vertex.color = "orange", main = "co-feeding", vertex.label.color = "black", vertex.label.cex = 0.8, vertex.label.font = 2, vertex.frame.color = "orange", vertex.label.family = "Arial")
plot(testWeekGraphs0$roostD, vertex.size = 10, edge.width = E(testWeekGraphs0$roostD)$weight, vertex.color = "firebrick2", main = "co-roosting (distance)", vertex.label.color = "black", vertex.label.cex = 0.8, vertex.frame.color = "firebrick2", vertex.label.family = "Arial")
plot(testWeekGraphs0$roostP, vertex.size = 10, edge.width = E(testWeekGraphs0$roostP)$weight, vertex.color = "firebrick2", main = "co-roosting (shared polygons)", vertex.label.color = "black", vertex.label.cex = 0.8, vertex.frame.color = "firebrick2", vertex.label.family = "Arial")

# PERMUTATIONS
nperm <- 100

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
# conveyorRoosts <- map(conveyor, ~vultureUtils::get_roosts_df(df = .x, id = "trackId", quiet = T), .progress = T)
# save(conveyorRoosts, file = "data/conveyorRoosts.Rda")
# conveyorNetworks <- map(conveyor, ~getNetworks(dataset = .x, roosts = testWeekRoosts, roostPolygons = roostPolygons), .progress = T)
# save(conveyorNetworks, file = "data/conveyorNetworks.Rda")
load("data/conveyor.Rda")
load("data/conveyorRoosts.Rda")
load("data/conveyorNetworks.Rda")
conveyorGraphs <- map(conveyorNetworks, ~map(.x, ~makeGraph(mode = "sri", data = .x$sri, weighted = T)))

# CALCULATE METRICS: FLIGHT
flight_real <- testWeekNetworks$flight$sri %>% mutate(type = "observed")
flight_shuffled <- imap_dfr(shuffledNetworks, ~.x[["flight"]]$sri %>% mutate(rep = .y)) %>% mutate(type = "shuffled")
flight_shifted <- imap_dfr(shiftedNetworks, ~.x[["flight"]]$sri %>% mutate(rep = .y)) %>% mutate(type = "shifted")
flight_conveyor <- imap_dfr(conveyorNetworks, ~.x[["flight"]]$sri %>% mutate(rep = .y)) %>% mutate(type = "conveyor")

flightSRI <- bind_rows(flight_real, flight_shuffled, flight_shifted, flight_conveyor) %>%
  mutate(logSRI = log(sri))

## SRI distributions
flightSRI %>%
  mutate(type = factor(type, levels = c("observed", "shuffled", "shifted", "conveyor"))) %>%
  ggplot(aes(x = sri, group = rep))+
  geom_density(aes(col = type), alpha = 0.5)+
  facet_wrap(~type)+
  theme_classic()+
  theme(legend.position = "none")+
  ylab("Density")+
  xlab("SRI")+
  theme(strip.text = element_text(size = 18))

flightSRI %>%
  mutate(type = factor(type, levels = c("observed", "shuffled", "shifted", "conveyor"))) %>%
  ggplot(aes(x = logSRI, group = rep))+
  geom_density(aes(col = type), lwd = 0.2)+
  facet_wrap(~type)+
  theme_classic()+
  theme(legend.position = "none")+
  ylab("Density")+
  xlab("SRI (log-transformed)")+
  theme(strip.text = element_text(size = 18))

flightSRI %>%
  mutate(type = factor(type, levels = c("observed", "shuffled", "shifted", "conveyor"))) %>%
  ggplot(aes(x = type, y = logSRI))+
  geom_violin(aes(fill = type))+
  theme_classic()+
  theme(legend.position = "none", text = element_text(size = 16))+
  ylab("SRI (log-transformed)")+
  xlab("Permutation type")

## Strength distributions
str_flight_real <- strength(testWeekGraphs0$flight) %>% enframe(., name = "trackId", value = "strength") %>% mutate(type = "observed")
str_flight_shuffled <- imap_dfr(shuffledGraphs, ~{
  g <- .x[["flight"]]
  g <- delete.edges(g, E(g)[E(g)$weight <= 0|is.na(E(g)$weight)])
  df <- enframe(strength(g), name = "trackId", value = "strength") %>% mutate(type = "shuffled", rep = .y)
})
str_flight_shifted <- imap_dfr(shiftedGraphs, ~{
  g <- .x[["flight"]]
  g <- delete.edges(g, E(g)[E(g)$weight <= 0|is.na(E(g)$weight)])
  df <- enframe(strength(g), name = "trackId", value = "strength") %>% mutate(type = "shifted", rep = .y)
})

str_flight_conveyor <- imap_dfr(conveyorGraphs, ~{
  g <- .x[["flight"]]
  g <- delete.edges(g, E(g)[E(g)$weight <= 0|is.na(E(g)$weight)])
  df <- enframe(strength(g), name = "trackId", value = "strength") %>% mutate(type = "conveyor", rep = .y)
})

flightStrength <- bind_rows(str_flight_real, str_flight_shuffled, str_flight_shifted, str_flight_conveyor)

### plot of strength distributions
flightStrength %>%
  mutate(type = factor(type, levels = c("observed", "shuffled", "shifted", "conveyor"))) %>%
  ggplot(aes(x = strength, group = rep))+
  geom_density(aes(col = type), lwd = 0.2)+
  facet_wrap(~type)+
  theme_classic()+
  theme(legend.position = "none")+
  theme(strip.text = element_text(size = 18))+
  theme(text = element_text(size = 16))+
  ylab("Density")+
  xlab("Strength")

flightStrength %>%
  mutate(type = factor(type, levels = c("observed", "shuffled", "shifted", "conveyor"))) %>%
  ggplot(aes(x = log(strength), group = rep))+
  geom_density(aes(col = type), lwd = 0.2)+
  facet_wrap(~type)+
  theme_classic()+
  theme(legend.position = "none")+
  theme(strip.text = element_text(size = 18))+
  theme(text = element_text(size = 16))+
  ylab("Density")+
  xlab("Strength (log-transformed)")

flightStrengthWide <- flightStrength %>%
  filter(type != "observed") %>%
  left_join(str_flight_real %>% select(-type) %>% rename("observedStrength" = strength), by = "trackId")


### individuals' strengths under different permutations
flightStrengthWide_nonzero <- flightStrengthWide %>%
  filter(observedStrength > 0)
indOrder <- flightStrengthWide_nonzero %>%
  arrange(-observedStrength) %>%
  pull(trackId) %>%
  unique()
flightStrengthWide_nonzero <- flightStrengthWide_nonzero %>%
  mutate(trackId = factor(trackId, levels = indOrder))

flightStrengthWide_nonzero %>%
  ggplot(aes(x = trackId, y = strength, col = type))+
  geom_boxplot(outlier.size = 1, aes(fill = type))+
  geom_point(aes(x = trackId, y = observedStrength), color = "black")+
  theme_classic()+
  facet_wrap(~type)+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        strip.text = element_text(size = 18),
        axis.ticks.x = element_blank())+
  xlab("Individual")+
  ylab("Strength")

flightStrengthWide_nonzero %>%
  ggplot(aes(x = observedStrength, y = strength, col = type))+
  geom_point(size = 3, alpha = 0.7)+
  geom_smooth(method = "lm")+
  theme_classic()+
  ylab("Strength (permuted)")+
  xlab("Strength (observed)")+
  scale_color_discrete(name = "Permutation")+
  geom_abline(slope = 1, intercept = 0, col = "black", lty = 2)+
  theme(text = element_text(size = 16))


## Degree distributions
deg_flight_real <- degree(testWeekGraphs0$flight) %>% enframe(., name = "trackId", value = "degree") %>% mutate(type = "observed")
deg_flight_shuffled <- imap_dfr(shuffledGraphs, ~{
  g <- .x[["flight"]]
  g <- delete.edges(g, E(g)[E(g)$weight <= 0|is.na(E(g)$weight)])
  df <- enframe(degree(g), name = "trackId", value = "degree") %>% mutate(type = "shuffled", rep = .y)
})
deg_flight_shifted <- imap_dfr(shiftedGraphs, ~{
  g <- .x[["flight"]]
  g <- delete.edges(g, E(g)[E(g)$weight <= 0|is.na(E(g)$weight)])
  df <- enframe(degree(g), name = "trackId", value = "degree") %>% mutate(type = "shifted", rep = .y)
})

deg_flight_conveyor <- imap_dfr(conveyorGraphs, ~{
  g <- .x[["flight"]]
  g <- delete.edges(g, E(g)[E(g)$weight <= 0|is.na(E(g)$weight)])
  df <- enframe(degree(g), name = "trackId", value = "degree") %>% mutate(type = "conveyor", rep = .y)
})

flightDegree <- bind_rows(deg_flight_real, deg_flight_shuffled, deg_flight_shifted, deg_flight_conveyor)
flightDegreeWide <- flightDegree %>%
  filter(type != "observed") %>%
  left_join(deg_flight_real %>% select(-type) %>% rename("observedDegree" = degree), by = "trackId")

### plot of degree distributions
flightDegree %>%
  mutate(type = factor(type, levels = c("observed", "shuffled", "shifted", "conveyor"))) %>%
  ggplot(aes(x = degree, group = rep))+
  geom_density(aes(col = type), lwd = 1)+
  facet_wrap(~type)+
  theme_classic()+
  theme(legend.position = "none")+
  theme(strip.text = element_text(size = 18))+
  theme(text = element_text(size = 16))+
  ylab("Density")+
  xlab("Degree")

flightDegree %>%
  mutate(type = factor(type, levels = c("observed", "shuffled", "shifted", "conveyor"))) %>%
  ggplot(aes(x = log(degree), group = rep))+
  geom_density(aes(col = type), lwd = 1)+
  facet_wrap(~type)+
  theme_classic()+
  theme(legend.position = "none")+
  theme(strip.text = element_text(size = 18))+
  theme(text = element_text(size = 16))+
  ylab("Density")+
  xlab("Degree (log-transformed)")

### individuals' degrees under different permutations
flightDegreeWide_nonzero <- flightDegreeWide %>%
  filter(observedDegree > 0)
indOrder <- flightDegreeWide_nonzero %>%
  arrange(-observedDegree) %>%
  pull(trackId) %>%
  unique()
flightDegreeWide_nonzero <- flightDegreeWide_nonzero %>%
  mutate(trackId = factor(trackId, levels = indOrder))

flightDegreeWide_nonzero %>%
  ggplot(aes(x = trackId, y = degree, col = type))+
  geom_boxplot(outlier.size = 1, aes(fill = type))+
  geom_point(aes(x = trackId, y = observedDegree), color = "black")+
  theme_classic()+
  facet_wrap(~type)+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        strip.text = element_text(size = 18),
        axis.ticks.x = element_blank())+
  xlab("Individual")+
  ylab("Degree")

flightDegreeWide_nonzero %>%
  ggplot(aes(x = observedDegree, y = degree, col = type))+
  #geom_point(size = 3, alpha = 0.7)+
  geom_smooth(method = "lm")+
  theme_classic()+
  ylab("Degree (permuted)")+
  xlab("Degree (observed)")+
  scale_color_discrete(name = "Permutation")+
  geom_abline(slope = 1, intercept = 0, col = "black", lty = 2)+
  theme(text = element_text(size = 16))

### Strength by degree: distinguishes how individuals spread out their connections. E.g. A interacts 3 times with B, vs. A interacts once each with B, C, and D.

head(flightDegreeWide)
head(flightStrengthWide)
#XXX come back to this: need multiple reps

sbd <- left_join(flightDegreeWide, flightStrengthWide, by = c("trackId", "type", "rep")) %>%
  mutate(observedSBD = observedStrength/observedDegree,
         SBD = strength/degree)

sbd %>%
  select(trackId, type, rep, observedSBD, SBD) %>%
  pivot_longer(cols = c("observedSBD", "SBD"), names_to = "type2", values_to = "sbd") %>%
  mutate(type = case_when(type2 == "observedSBD" ~ "observed",
                          TRUE ~ type)) %>%
  select(trackId, type, rep, sbd) %>%
  distinct() %>%
  mutate(type = factor(type, levels = c("observed", "shuffled", "shifted", "conveyor"))) %>%
  ggplot(aes(x = sbd, col = type))+
  geom_density(size = 1.5)+
  theme_classic()+
  ylab("Strength-by-degree (permuted)")+
  xlab("Strength-by-degree (observed)")+
  theme(legend.position = "none",
        text = element_text(size = 16))+
  facet_wrap(~type)

