# Permutations test script: Just 2022 data


# Setup and data loading --------------------------------------------------

library(tidyverse)
library(vultureUtils)
library(igraph)
library(feather)
library(sf)
library(ggraph)
library(tidygraph)
library(wesanderson)
source("funs_datastreamPermutations.R")

# load("data/datAnnotCleaned.Rda")
roostPolygons <- sf::st_read("data/roosts25_cutOffRegion.kml")
# 
# # Goal: year 2022, including December 2021 but excluding December 2022. To get this, we need to pull years 2021 and 2022 and then filter down.
# # Extract only 2021 and 2022
# tt <- datAnnotCleaned %>% filter(lubridate::year(timestamp) %in% c(2021, 2022))
# 
# # make this into an sf object:
# tt <- tt %>%
#   sf::st_as_sf(coords = c("location_long.1", "location_lat.1")) %>%
#   sf::st_set_crs("WGS84") %>%
#   mutate(timeOnly = stringr::str_extract(as.character(timestamp), "[0-9]{2}:[0-9]{2}:[0-9]{2}"))
# roostPolygons <- sf::st_read("data/roosts25_cutOffRegion.kml", quiet = TRUE) %>%
#   sf::st_transform("WGS84") %>%
#   select(-c("Name", "Description"))
# 
# # Breeding: December through June
# # Non-breeding: July through November
# tt <- tt %>%
#   mutate(season = case_when(lubridate::month(timestamp) %in% c(1:6) ~ "breeding_thisyear",
#                             lubridate::month(timestamp) == 12 ~ "breeding_nextyear",
#                             lubridate::month(timestamp) %in% c(7:11) ~ "nonbreeding",
#                             TRUE ~ NA_character_),
#          year = lubridate::year(timestamp),
#          seasonUnique = case_when(season == "breeding_thisyear" ~ paste(year, "breeding", sep = "_"),
#                                   season == "breeding_nextyear" ~ paste(year+1, "breeding", sep = "_"),
#                                   season == "nonbreeding" ~ paste(year, "nonbreeding", sep = "_")))
# 
# # split by season and keep only 21-22 breeding and 22 nonbreeding
# tt <- tt %>%
#   filter(grepl("2022", seasonUnique))
# 
# seasons <- tt %>%
#   group_by(seasonUnique) %>%
#   group_split(.keep = T)

# alternatively, split by month
# months <- tt %>%
#   group_by("month" = lubridate::month(timestamp)) %>%
#   group_split(.keep = T)
# save(months, file = "data/months.Rda")
load("data/months.Rda")

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
# monthsFlight_2022 <- purrr::map(months, ~vultureUtils::getFlightEdges(dataset = .x, roostPolygons = roostPolygons, distThreshold = 1000, return = "both"))
# save(monthsFlight_2022, file = "data/monthsFlight_2022.Rda")
# load("data/monthsFlight_2022.Rda")
# 
# monthsFeeding_2022 <- purrr::map(months, ~vultureUtils::getFeedingEdges(dataset = .x, roostPolygons = roostPolygons, distThreshold = 50, return = "both"))
# save(monthsFeeding_2022, file = "data/monthsFeeding_2022.Rda")
# load("data/monthsFeeding_2022.Rda")
# 
# months_roost_distance <- purrr::map(monthRoosts, ~vultureUtils::getRoostEdges(dataset = .x, mode = "distance", distThreshold = 500, return = "both"))
# save(months_roost_distance, file = "data/months_roost_distance.Rda")
# load("data/months_roost_distance.Rda")
# 
# months_roost_polygon <- purrr::map(monthRoosts, ~vultureUtils::getRoostEdges(dataset = .x, mode = "polygon", roostPolygons = roostPolygons, return = "both"))
# save(months_roost_polygon, file = "data/months_roost_polygon.Rda")
# load("data/months_roost_polygon.Rda")

# Create network graphs for each of these
#allVerts <- unique(tt$trackId)

# g_flight <- map(monthsFlight_2022, ~vultureUtils::makeGraph(mode = "sri", data = .x$sri, weighted = T, vertices = allVerts))
# g_feeding <- map(monthsFeeding_2022, ~vultureUtils::makeGraph(mode = "sri", data = .x$sri, weighted = T))
# g_roost_distance <- map(months_roost_distance, ~vultureUtils::makeGraph(mode = "sri", data = .x$sri, weighted = T))
# g_roost_polygon <- map(months_roost_polygon, ~vultureUtils::makeGraph(mode = "sri", data = .x$sri, weighted = T))

# Make some plots using igraph
# test <- g_flight[[1]]
# plot(test, vertex.size = 10, edge.width = E(test)$weight, edge.color = "black")
# allVerts <- map(g_flight, ~V(.x)) %>% unlist() %>% names() %>% unique()
# flightLayout <- layout_with_fr(graph = g_flight[[1]])
# plot(test, vertex.size = 10, edge.width = E(test)$weight, edge.color = "black", vertices = allVerts)

# Test week: observed networks --------------------------------------------

# Test data for permutations--let's use just one week of data.
testWeek <- months[[1]] %>%
  filter(lubridate::ymd(dateOnly) >= lubridate::ymd("2022-01-01") & lubridate::ymd(dateOnly) <= lubridate::ymd("2022-01-07"))

# Get roosts for this test week
#testWeekRoosts <- vultureUtils::get_roosts_df(df = testWeek, id = "trackId")
#save(testWeekRoosts, file = "data/testWeekRoosts.Rda")
load("data/testWeekRoosts.Rda")

# Get all four types of network
getNetworks <- function(dataset, roosts, roostPolygons){
  testWeekFlight <- getFlightEdges(dataset, roostPolygons, distThreshold = 1000, return = "both")
  testWeekFeeding <- getFeedingEdges(dataset, roostPolygons, distThreshold = 50, return = "both")
  testWeekRoostD <- getRoostEdges(dataset = roosts, mode = "distance", idCol = "trackId", return = "both", distThreshold = 500)
  testWeekRoostP <- getRoostEdges(dataset = roosts, mode = "polygon", roostPolygons = roostPolygons, idCol = "trackId", return = "both")
  outList <- list("flight" = testWeekFlight, "feeding" = testWeekFeeding, "roostD" = testWeekRoostD, "roostP" = testWeekRoostP)
}

# testWeekNetworks <- getNetworks(dataset = testWeek, roosts = testWeekRoosts, roostPolygons = roostPolygons)
# save(testWeekNetworks, file = "data/testWeekNetworks.Rda")
load("data/testWeekNetworks.Rda")

# Make graphs
testWeekGraphs <- map(testWeekNetworks, ~makeGraph(mode = "sri", data = .x$sri, weighted = T))
testWeekGraphs0 <- map(testWeekGraphs, ~delete.edges(.x, E(.x)[E(.x)$weight <= 0|is.na(E(.x)$weight)])) # remove 0-weight edges for plotting/calcs

# PLOTS: OBSERVED NETWORKS
plot(testWeekGraphs0$flight, vertex.size = 15, edge.width = 10*E(testWeekGraphs0$flight)$weight, vertex.color = "lightblue", main = "co-flight", vertex.label.color = "black", vertex.label.cex = 0.8, vertex.label.font = 2, vertex.frame.color = "lightblue", vertex.label.family = "Arial")
plot(testWeekGraphs0$feeding, vertex.size = 15, edge.width = 10*E(testWeekGraphs0$feeding)$weight, vertex.color = "orange", main = "co-feeding", vertex.label.color = "black", vertex.label.cex = 0.8, vertex.label.font = 2, vertex.frame.color = "orange", vertex.label.family = "Arial")
plot(testWeekGraphs0$roostD, vertex.size = 10, edge.width = 5*E(testWeekGraphs0$roostD)$weight, vertex.color = "firebrick2", main = "co-roosting (distance)", vertex.label.color = "black", vertex.label.cex = 0.8, vertex.frame.color = "firebrick2", vertex.label.family = "Arial")
plot(testWeekGraphs0$roostP, vertex.size = 10, edge.width = E(testWeekGraphs0$roostP)$weight, vertex.color = "firebrick2", main = "co-roosting (shared polygons)", vertex.label.color = "black", vertex.label.cex = 0.8, vertex.frame.color = "firebrick2", vertex.label.family = "Arial")

# PERMUTATIONS ------------------------------------------------------------
nperm <- 100
pal <- wes_palette("FantasticFox1")
colshuf <- pal[2]
colshif <- pal[4]
colconv <- pal[3]

# 1: Shuffled -------------------------------------------------------------
# shuffled <- map(1:nperm, ~p_randomDays(dataset = testWeek, idCol = "trackId", dateCol = "dateOnly", timeCol = "timeOnly"))
# save(shuffled, file = "data/shuffled.Rda")
# shuffledRoosts <- map(shuffled, ~vultureUtils::get_roosts_df(df = .x, id = "trackId", quiet = T), .progress = T)
# save(shuffledRoosts, file = "data/shuffledRoosts.Rda")
load("data/shuffled.Rda")
load("data/shuffledRoosts.Rda")
# shuffledNetworks <- map(shuffled, ~getNetworks(dataset = .x, roosts = testWeekRoosts, roostPolygons = roostPolygons))
# save(shuffledNetworks, file = "data/shuffledNetworks.Rda")
load("data/shuffledNetworks.Rda")
shuffledGraphs <- map(shuffledNetworks, ~map(.x, ~makeGraph(mode = "sri", data = .x$sri, weighted = T)))

# 2: Shifted -------------------------------------------------------------
# PERMUTATION 2: Shift
# shifted <- map(1:nperm, ~p_shift(dataset = testWeek, shiftMax = 5, idCol = "trackId", dateCol = "dateOnly", timeCol = "timeOnly"))
# save(shifted, file = "data/shifted.Rda")
# shiftedRoosts <- map(shifted, ~vultureUtils::get_roosts_df(df = .x, id = "trackId", quiet = T), .progress = T)
# save(shiftedRoosts, file = "data/shiftedRoosts.Rda")
load("data/shifted.Rda")
load("data/shiftedRoosts.Rda")
# shiftedNetworks <- map(shifted, ~getNetworks(dataset = .x, roosts = testWeekRoosts, roostPolygons = roostPolygons))
# save(shiftedNetworks, file = "data/shiftedNetworks.Rda")
load("data/shiftedNetworks.Rda")
shiftedGraphs <- map(shiftedNetworks, ~map(.x, ~makeGraph(mode = "sri", data = .x$sri, weighted = T)))

# 3: Conveyor -------------------------------------------------------------
# conveyor <- map(1:nperm, ~p_conveyor(dataset = testWeek, mode = "global", shiftMax = 5, idCol = "trackId", dateCol = "dateOnly", timeCol = "timeOnly"))
# save(conveyor, file = "data/conveyor.Rda")
# conveyorRoosts <- map(conveyor, ~vultureUtils::get_roosts_df(df = .x, id = "trackId", quiet = T), .progress = T)
# save(conveyorRoosts, file = "data/conveyorRoosts.Rda")
load("data/conveyor.Rda")
load("data/conveyorRoosts.Rda")
# conveyorNetworks <- map(conveyor, ~getNetworks(dataset = .x, roosts = testWeekRoosts, roostPolygons = roostPolygons))
# save(conveyorNetworks, file = "data/conveyorNetworks.Rda")
load("data/conveyorNetworks.Rda")
conveyorGraphs <- map(conveyorNetworks, ~map(.x, ~makeGraph(mode = "sri", data = .x$sri, weighted = T)))

# RESULTS: CO-FLIGHT ------------------------------------------------------
flight_real <- testWeekNetworks$flight$sri %>% mutate(type = "observed")
flight_shuffled <- imap_dfr(shuffledNetworks, ~.x[["flight"]]$sri %>% mutate(rep = .y)) %>% mutate(type = "shuffled")
flight_shifted <- imap_dfr(shiftedNetworks, ~.x[["flight"]]$sri %>% mutate(rep = .y)) %>% mutate(type = "shifted")
flight_conveyor <- imap_dfr(conveyorNetworks, ~.x[["flight"]]$sri %>% mutate(rep = .y)) %>% mutate(type = "conveyor")

# SRI --------------------------------------------------------------------
flightSRI <- bind_rows(flight_real, flight_shuffled, flight_shifted, flight_conveyor) %>%
  mutate(type = factor(type, levels = c("observed", "shuffled", "shifted", "conveyor")))

flightSRIMeans <- flightSRI %>%
  group_by(type, rep) %>%
  summarize(mnSRI = mean(sri, na.rm = T),
            sdSRI = sd(sri, na.rm = T),
            minSRI = min(sri, na.rm = T),
            maxSRI = max(sri, na.rm = T))

## Density (raw) ------------------------------------------------------
flightSRI %>%
  mutate(type = factor(type, levels = c("observed", "shuffled", "shifted", "conveyor"))) %>%
  ggplot(aes(x = sri, group = rep))+
  geom_density(aes(col = type), linewidth = 0.2)+
  facet_wrap(~type)+
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_manual(values = c("black", colshuf, colshif, colconv))+
  ylab("Density")+
  xlab("SRI")+
  theme(strip.text = element_text(size = 18))

## Density (log) ------------------------------------------------------
flightSRI %>%
  mutate(type = factor(type, levels = c("observed", "shuffled", "shifted", "conveyor"))) %>%
  ggplot(aes(x = log(sri), group = rep))+
  geom_density(aes(col = type), lwd = 0.2)+
  facet_wrap(~type)+
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_manual(values = c("black", colshuf, colshif, colconv))+
  ylab("Density")+
  xlab("SRI (log-transformed)")+
  theme(strip.text = element_text(size = 18))

## Mean Density (raw) ------------------------------------------------------
flightSRIMeans %>%
  filter(type != "observed") %>%
  ggplot(aes(x = mnSRI))+
  geom_density(aes(col = type, fill = type), lwd = 1.5, alpha = 0.3)+
  geom_vline(aes(xintercept = flightSRIMeans$mnSRI[flightSRIMeans$type == "observed"]), lwd = 1, lty = 2)+
  scale_color_manual(values = c(colshuf, colshif, colconv))+
  scale_fill_manual(values = c(colshuf, colshif, colconv))+
  theme_classic()+
  ylab("Density")+
  xlab("Mean SRI value") 

## StD Density (raw) ------------------------------------------------------
flightSRIMeans %>%
  filter(type != "observed") %>%
  ggplot(aes(x = sdSRI))+
  geom_density(aes(col = type, fill = type), lwd = 1.5, alpha = 0.3)+
  geom_vline(aes(xintercept = flightSRIMeans$sdSRI[flightSRIMeans$type == "observed"]), lwd = 1, lty = 2)+
  scale_color_manual(values = c(colshuf, colshif, colconv))+
  scale_fill_manual(values = c(colshuf, colshif, colconv))+
  theme_classic()+
  ylab("Density")+
  xlab("Std Deviation of SRI")

# Strength --------------------------------------------------------------------
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

flightStrength <- bind_rows(str_flight_real, str_flight_shuffled, str_flight_shifted, str_flight_conveyor) %>%
  mutate(type = factor(type, levels = c("observed", "shuffled", "shifted", "conveyor")))

flightStrengthMeans <- flightStrength %>%
  group_by(type, rep) %>%
  summarize(mnStrength = mean(strength, na.rm = T),
            sdStrength = sd(strength, na.rm = T),
            minStrength = min(strength, na.rm = T),
            maxStrength = max(strength, na.rm = T))

## Density (raw) ------------------------------------------------------
flightStrength %>%
  mutate(type = factor(type, levels = c("observed", "shuffled", "shifted", "conveyor"))) %>%
  ggplot(aes(x = strength, group = rep))+
  geom_density(aes(col = type), lwd = 0.2)+
  facet_wrap(~type)+
  theme_classic()+
  theme(legend.position = "none")+
  theme(strip.text = element_text(size = 18))+
  scale_color_manual(values = c("black", colshuf, colshif, colconv))+
  theme(text = element_text(size = 16))+
  ylab("Density")+
  xlab("Strength")

## Density (log) ------------------------------------------------------
flightStrength %>%
  mutate(type = factor(type, levels = c("observed", "shuffled", "shifted", "conveyor"))) %>%
  ggplot(aes(x = log(strength), group = rep))+
  geom_density(aes(col = type), lwd = 0.2)+
  facet_wrap(~type)+
  theme_classic()+
  theme(legend.position = "none")+
  theme(strip.text = element_text(size = 18))+
  scale_color_manual(values = c("black", colshuf, colshif, colconv))+
  theme(text = element_text(size = 16))+
  ylab("Density")+
  xlab("Strength (log-transformed)")

flightStrengthWide <- flightStrength %>%
  filter(type != "observed") %>%
  left_join(str_flight_real %>% select(-type) %>% rename("observedStrength" = strength), by = "trackId")

## Mean Density (raw) ------------------------------------------------------
flightStrengthMeans %>%
  filter(type != "observed") %>%
  ggplot(aes(x = mnStrength))+
  geom_density(aes(col = type, fill = type), lwd = 1.5, alpha = 0.3)+
  geom_vline(aes(xintercept = flightStrengthMeans$mnStrength[flightStrengthMeans$type == "observed"]), lwd = 1, lty = 2)+
  scale_color_manual(values = c(colshuf, colshif, colconv))+
  scale_fill_manual(values = c(colshuf, colshif, colconv))+
  theme_classic()+
  ylab("Density")+
  xlab("Mean strength value") 

## StD Density (raw) ------------------------------------------------------
flightStrengthMeans %>%
  filter(type != "observed") %>%
  ggplot(aes(x = sdStrength))+
  geom_density(aes(col = type, fill = type), lwd = 1.5, alpha = 0.3)+
  geom_vline(aes(xintercept = flightStrengthMeans$sdStrength[flightStrengthMeans$type == "observed"]), lwd = 1, lty = 2)+
  scale_color_manual(values = c(colshuf, colshif, colconv))+
  scale_fill_manual(values = c(colshuf, colshif, colconv))+
  theme_classic()+
  ylab("Density")+
  xlab("Std Deviation of strength")


## Ind boxplots (raw) ------------------------------------------------------
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
  scale_color_manual(values = c(colshuf, colshif, colconv))+
  scale_fill_manual(values = c(colshuf, colshif, colconv))+
  theme_classic()+
  facet_wrap(~type)+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        strip.text = element_text(size = 18),
        axis.ticks.x = element_blank())+
  xlab("Individual")+
  ylab("Strength")+
  coord_flip()


## Permuted vs. Observed Regression (raw) ----------------------------------
flightStrengthWide_nonzero %>%
  ggplot(aes(x = jitter(observedStrength, factor = 10), y = strength, col = type))+
  geom_point(size = 2, alpha = 0.5)+
  geom_smooth(method = "lm")+
  theme_classic()+
  scale_color_manual(name = "Permutation", values = c(colshuf, colshif, colconv))+
  ylab("Strength (permuted)")+
  xlab("Strength (observed)")+
  geom_abline(slope = 1, intercept = 0, col = "black", lty = 2)+
  theme(text = element_text(size = 16))

# Degree ------------------------------------------------------------------

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

flightDegree <- bind_rows(deg_flight_real, deg_flight_shuffled, deg_flight_shifted, deg_flight_conveyor) %>%
  mutate(type = factor(type, levels = c("observed", "shuffled", "shifted", "conveyor")))

flightDegreeMeans <- flightDegree %>%
  group_by(type, rep) %>%
  summarize(mnDegree = mean(degree, na.rm  =T),
            sdDegree = sd(degree, na.rm = T),
            minDegree = min(degree, na.rm = T),
            maxDegree = max(degree, na.rm = T))

flightDegreeWide <- flightDegree %>%
  filter(type != "observed") %>%
  left_join(deg_flight_real %>% select(-type) %>% rename("observedDegree" = degree), by = "trackId")


## Mean Density (raw) ------------------------------------------------------
flightDegreeMeans %>%
  filter(type != "observed") %>%
  ggplot(aes(x = mnDegree))+
  geom_density(aes(col = type, fill = type), lwd = 1.5, alpha = 0.3)+
  geom_vline(aes(xintercept = flightDegreeMeans$mnDegree[flightDegreeMeans$type == "observed"]), lwd = 1, lty = 2)+
  scale_color_manual(values = c(colshuf, colshif, colconv))+
  scale_fill_manual(values = c(colshuf, colshif, colconv))+
  theme_classic()+
  ylab("Density")+
  xlab("Mean degree") 

## StD Density (raw) ------------------------------------------------------
flightDegreeMeans %>%
  filter(type != "observed") %>%
  ggplot(aes(x = sdDegree))+
  geom_density(aes(col = type, fill = type), lwd = 1.5, alpha = 0.3)+
  geom_vline(aes(xintercept = flightDegreeMeans$sdDegree[flightDegreeMeans$type == "observed"]), lwd = 1, lty = 2)+
  scale_color_manual(values = c(colshuf, colshif, colconv))+
  scale_fill_manual(values = c(colshuf, colshif, colconv))+
  theme_classic()+
  ylab("Density")+
  xlab("Std Deviation of degree")

## Density (raw) ------------------------------------------------------
flightDegree %>%
  mutate(type = factor(type, levels = c("observed", "shuffled", "shifted", "conveyor"))) %>%
  ggplot(aes(x = degree, group = rep))+
  geom_density(aes(col = type), lwd = 0.2)+
  facet_wrap(~type)+
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_manual(values = c("black", colshuf, colshif, colconv))+
  theme(strip.text = element_text(size = 18))+
  theme(text = element_text(size = 16))+
  ylab("Density")+
  xlab("Degree")

## Ind boxplots (raw) ------------------------------------------------------
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
  scale_color_manual(values = c(colshuf, colshif, colconv))+
  scale_fill_manual(values = c(colshuf, colshif, colconv))+
  xlab("Individual")+
  ylab("Degree")+
  coord_flip()

## Permuted vs. Observed Regression (raw) ----------------------------------
flightDegreeWide_nonzero %>%
  ggplot(aes(x = observedDegree, y = degree, col = type))+
  #geom_point(size = 3, alpha = 0.7)+
  geom_smooth(method = "lm")+
  theme_classic()+
  ylab("Degree (permuted)")+
  xlab("Degree (observed)")+
  scale_color_manual(values = c(colshuf, colshif, colconv))+
  geom_abline(slope = 1, intercept = 0, col = "black", lty = 2)+
  theme(text = element_text(size = 16))

# Strength-by-degree ------------------------------------------------------

# Strength by degree: distinguishes how individuals spread out their connections. E.g. A interacts 3 times with B, vs. A interacts once each with B, C, and D.

head(flightDegreeWide)
head(flightStrengthWide)

sbd <- left_join(flightStrength, flightDegree, by = c("trackId", "type", "rep")) %>%
  mutate(sbd = strength/degree)

## Density (raw) -----------------------------------------------------------
sbd %>%
  mutate(type = factor(type, levels = c("observed", "shuffled", "shifted", "conveyor"))) %>%
  ggplot(aes(x = sbd, group = rep))+
  geom_density(aes(col = type), lwd = 0.2)+
  facet_wrap(~type)+
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_manual(values = c("black", colshuf, colshif, colconv))+
  theme(strip.text = element_text(size = 18))+
  theme(text = element_text(size = 16))+
  ylab("Density")+
  xlab("Strength by Degree")

flightSBDMeans <- sbd %>%
  group_by(type, rep) %>%
  summarize(mnSBD = mean(sbd, na.rm = T),
            sdSBD = sd(sbd, na.rm = T),
            minSBD = min(sbd, na.rm = T),
            maxSBD = max(sbd, na.rm = T))

## Mean Density (raw) ------------------------------------------------------
obsMn <- flightSBDMeans %>% filter(type == "observed") %>% pull(mnSBD)
annot <- flightSBDMeans %>%
  filter(type != "observed") %>%
  group_by(type) %>%
  mutate(resid = mnSBD - obsMn) %>%
  summarize(propAbove = sum(resid > 0)/n())

flightSBDMeans %>%
  filter(type != "observed") %>%
  ggplot(aes(x = mnSBD))+
  geom_density(aes(col = type, fill = type), lwd = 1.5, alpha = 0.3)+
  geom_vline(aes(xintercept = obsMn), lwd = 1, lty = 2)+
  scale_color_manual(values = c(colshuf, colshif, colconv))+
  scale_fill_manual(values = c(colshuf, colshif, colconv))+
  theme_classic()+
  ylab("Density")+
  xlab("Mean SBD")+
  geom_text(data = annot, aes(x = 0.45, y = c(6.5, 6, 5.5), col = type, label = paste("p = ", propAbove)), size = 5, fontface = "bold")

## StD Density (raw) -------------------------------------------------------
obsSD <- flightSBDMeans %>% filter(type == "observed") %>% pull(sdSBD)
annot <- flightSBDMeans %>%
  filter(type != "observed") %>%
  group_by(type) %>%
  mutate(resid = sdSBD - obsSD) %>%
  summarize(propAbove = sum(resid > 0)/n(),
            propBelow = sum(resid < 0)/n())

flightSBDMeans %>%
  filter(type != "observed") %>%
  ggplot(aes(x = sdSBD))+
  geom_density(aes(col = type, fill = type), lwd = 1.5, alpha = 0.3)+
  geom_vline(aes(xintercept = obsSD), lwd = 1, lty = 2)+
  scale_color_manual(values = c(colshuf, colshif, colconv))+
  scale_fill_manual(values = c(colshuf, colshif, colconv))+
  theme_classic()+
  ylab("Density")+
  xlab("Std Deviation of SBD")+
  geom_text(data = annot, aes(x = 0.4, y = c(6.5, 6, 5.5), col = type, 
                              label = paste("p = ", propBelow)), size = 4, fontface = "bold")

## Ind boxplots (raw) ------------------------------------------------------
sbd_flight_real <- sbd %>%
  filter(type == "observed") %>%
  select(trackId, sbd)

flightSBDWide <- sbd %>%
  filter(type != "observed") %>%
  left_join(sbd_flight_real %>% rename("observedSBD" = sbd), by = "trackId")

flightSBDWide_nonzero <- flightSBDWide %>%
  filter(observedSBD > 0)
indOrder <- flightSBDWide_nonzero %>%
  arrange(-observedSBD) %>%
  pull(trackId) %>%
  unique()
flightSBDWide_nonzero <- flightSBDWide_nonzero %>%
  mutate(trackId = factor(trackId, levels = indOrder))

flightSBDWide_nonzero %>%
  ggplot(aes(x = trackId, y = sbd, col = type))+
  geom_boxplot(outlier.size = 1, aes(fill = type))+
  geom_point(aes(x = trackId, y = observedSBD), color = "black")+
  theme_classic()+
  facet_wrap(~type)+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        strip.text = element_text(size = 18),
        axis.ticks.x = element_blank())+
  scale_color_manual(values = c(colshuf, colshif, colconv))+
  scale_fill_manual(values = c(colshuf, colshif, colconv))+
  xlab("Individual")+
  ylab("Strength by Degree")+
  coord_flip()
