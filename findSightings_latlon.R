

rm(list=ls())

library(lubridate)
library(RODBC)
library(tidyverse)



lat.min <- 33.68056
lat.max <- 33.78111
lon.min <- -118.2917
lon.max <- -118.0267

if (lon.min > 0) lon.min <- -1 * lon.min
if (lon.max > 0) lon.max <- -1 * lon.max

if (lat.min > lat.max) {
  tmp <- lat.min
  lat.min <- lat.max
  lat.max <- tmp
}

if (lon.min > lon.max) {
  tmp <- lon.min
  lon.min <- lon.max
  lon.max <- tmp
}

# get the turtle database
turtle <- odbcConnect(dsn = 'Turtle', uid = '', pwd = '')
turtle.sightings <- sqlQuery(turtle, 'select * from tbl_Sighting') %>%
  select(ID, Date_Observed, Species_ID, Latitude, Longitude)

odbcClose(turtle)

# get the common connection - called SWFSCcommon but it actually is common
SWFSCcommon <- odbcConnect(dsn = "common", uid = "", pwd = "")

# get the species table
sp.table <- sqlQuery(SWFSCcommon, "select * from tblSpecies")

odbcClose(SWFSCcommon)

# select just a few fields
sp.table %>% select(ID, Genus, Species, CommonName) %>%
  mutate(Species_ID = ID) %>%
  select(-ID) -> species.code

# select those within lat lon limits
turtle.sightings %>% filter(Latitude > lat.min &
                              Latitude < lat.max &
                              Longitude > lon.min &
                              Longitude < lon.max) -> filtered.sightings

# merge them.
filtered.sightings %>% left_join(species.code, by = "Species_ID") %>%
  select(ID, Date_Observed, CommonName, Latitude, Longitude) -> filtered.sightings

write.csv(filtered.sightings,
          file = paste0("data/sightings_query_", Sys.Date(), ".csv"),
          quote = FALSE, row.names = FALSE)


