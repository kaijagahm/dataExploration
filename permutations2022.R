# Permutations test script: Just 2022 data

library(tidyverse)
library(vultureUtils)
library(igraph)
library(feather)
library(sf)
library(ggraph)
library(tidygraph)

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

# Clearly I need help with igraph. But in the meantime, let's do some permutations!
# To make this tractable, I'm just going to deal with one month of data.
map(months, dim)
testMonth <- months[[10]] # this one has a lot of rows.
testMonthSimple <- testMonth %>%
  select(trackId, location_lat, location_long, dateOnly, timeOnly, season, year, seasonUnique, month, geometry)

## TYPE 1: Random day permutation. For each individual, I'll restrict it to only the days when it was actually observed, preserving real gaps. But I'll shuffle day tracks within it. 
randomDays_permuted <- testMonthSimple %>%
  group_by(trackId) %>%
  group_split(.keep = T) %>%
  map_dfr(~{
    days <- unique(.x$dateOnly)
    daysShuffled <- sample(days, size = length(days), replace = F)
    daysDF <- bind_cols("dateOnly" = days, "newDate" = daysShuffled)
    .x <- .x %>%
      left_join(daysDF, by = "dateOnly") %>%
      mutate(timestamp = lubridate::ymd_hms(paste(newDate, timeOnly)))
    return(.x)
  })
# Of course, will have to do this multiple times. Probably need to create some permutation functions. Even add them to the package?

## Type 2: Shift tracks back or forwards. For each individual, randomly choose number of days to shift, between -10 and 10, with uniform probability. Then create the new date column based on that.
shiftedDays_permuted <- testMonthSimple %>%
  group_by(trackId) %>%
  group_split(.keep = T) %>%
  map_dfr(~{
    shift <- sample(-10:10, size = 1)
    .x <- .x %>%
      mutate(newDate = dateOnly + shift,
             timestamp = lubridate::ymd_hms(paste(newDate, timeOnly)))
    return(.x)
  })

# Let's visualize the shifted vs. normal days
testMonthSimple %>%
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
  select(trackId, newDate) %>%
  distinct()%>%
  ggplot(aes(x = trackId, y = newDate))+
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
  pivot_longer(cols = c("dateOnly", "newDate"), names_to = "dateType", values_to = "date") %>%
  mutate(dateType = case_when(dateType == "dateOnly" ~ "Original",
                          dateType == "newDate" ~ "Shifted",
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




