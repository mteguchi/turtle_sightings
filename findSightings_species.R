
# Modified in Feb 2020 to extract data for Megan Hanna's paper.

rm(list=ls())

library(lubridate)
library(RODBC)
library(tidyverse)


Gn <- c("Chelonia")
# get the turtle database
turtle <- odbcConnect(dsn = 'Turtle', uid = '', pwd = '')
turtle.sightings <- sqlQuery(turtle, 'select * from tbl_Sighting') %>%
  select(-c(Observation_Description, ts, Record_Creation_Date, Edit_User_ID, Edit_Date))

odbcClose(turtle)

# get the common connection - called SWFSCcommon but it actually is common
SWFSCcommon <- odbcConnect(dsn = "common", uid = "", pwd = "")

# get the species table
sp.table <- sqlQuery(SWFSCcommon, "select * from tblSpecies")
county.table <- sqlQuery(SWFSCcommon, "select * from tblCounty")
state.table <- sqlQuery(SWFSCcommon, "select * from tblState")
city.table <- sqlQuery(SWFSCcommon, "select * from tblCity")

odbcClose(SWFSCcommon)

# species table
sp.table %>% select(ID, Genus, Species, CommonName) %>%
  #filter(Species == sp) %>%
  mutate(Species_ID = ID) %>%
  select(-ID) -> species.code

species.code %>% filter(Genus == "Chelonia" |
                          Genus ==   "Caretta" |
                          Genus == "Lepitochelys" |
                          Genus == "Eretmochelys" |
                          Genus == "Dermochelys" |
                          CommonName == "Unidentified sea turtle") -> turtle.sp.code

state.table %>% select(ID, TwoLetterID, Name) %>%
  mutate(State_ID = ID,
         State_name = Name) %>%
  select(-c(ID, Name)) -> state.code

county.table %>% select(ID, Name) %>%
  mutate(County_ID = ID,
         County_name = Name) %>%
  select(-c(ID, Name)) -> county.code

# CA is stateID = 5
# for southern california, we need San Diego, Orange, Los Angeles, Ventura, and Santa Barbara
county.table %>% filter(StateID == 5) -> county.CA.code
county.CA.code %>% filter(Name == "San Diego" |
                            Name == "Orange" |
                            Name == "Los Angeles" |
                            Name == "Ventura" |
                            Name == "Santa Barbara") %>%
  mutate(CountyID = ID, County_Name = Name) %>%
  select(CountyID, County_Name) -> county.SoCal.code

city.table %>% select(ID, Name) %>%
  mutate(City_ID = ID,
         City_name = Name) %>%
  select(-c(ID, Name)) -> city.code

city.table %>% right_join(county.SoCal.code, by = "CountyID") -> city.SoCal.code


# merge them.
turtle.sightings %>% left_join(species.code, by = "Species_ID") %>%
  left_join(county.code, by = "County_ID") %>%
  left_join(state.code, by = "State_ID") %>%
  left_join(city.code, by = "City_ID") %>%
  select(-c( "County_ID", "State_ID", "City_ID", "Country_ID", "Island_ID",
            "Observed_By", "Cruise_Number", "Observer_Number", "Effort", "Water_Temperature",
            "Bearing", "Distance", "Animal_Count", "JFR", "Reticle", "Captured",
            "Diagnostic_Features", "Behavior", "Injuries_Description",
            "Entanglement_Description", "Estimated_Size", "Length_Of_Time",
            "Species_Verified_By", "Biopsy", "Age", "Sex", "Tail_Beyond_Carapace",
            "Tags_Description", "Tag_Scars_Description", "Final_Disposition",
            "Locality_Details")) -> all.sightings

all.sightings %>% filter(Genus == Gn) %>%
  filter(County_name == "San Diego"|
           County_name == "Orange" |
           County_name == "Los Angeles" |
           County_name == "Ventura" |
           County_name == "Santa Barbara") -> filtered.sightings

write.csv(filtered.sightings,
          file = paste0("data/sightings_query_", Gn, "_", Sys.Date(), ".csv"),
          quote = FALSE, row.names = FALSE)


