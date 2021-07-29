# knitter
library(tidyverse)
library(data.table)
library(sf)
library(maps)
library(mapdata)
library(viridis)

select <- dplyr::select

this_group <- "Arrowtooth_flounder"

# CPUE from cpue.RData and hauls for zero cathces, CPUE from DFO original data and hauls

# AKFIN CPUE (species already mapped to Atlantis groups, will need to add zero catches)

load("C:/Users/arove/Documents/GOA/SDM/sdmTMB_forGemma_May2021/catch_to_CPUE_AKFIN/cpue.Rdata")
akfin_data <- cpue
rm(cpue)

akfin_data <- akfin_data %>% filter(CN == "Arrowtooth_flounder")

# rename columns
fields <- c("YEAR",
            "HAULJOIN",
            "LAT", 
            "LON", 
            "DEPTHR", 
            "ATLANTIS_GROUP", 
            "CN",
            "BIOM_KGKM2", 
            "NUM_KM2")

akfin_data <- akfin_data %>% select(all_of(fields)) %>% set_names(c(
  "year",
  "hauljoin",
  "lat",
  "lon",
  "depth",
  "species_code",
  "name",
  "biom_kgkm2",
  "num_km2"))

# add empty hauls
hauls <- read.csv("C:/Users/arove/Documents/GOA/SDM/sdmTMB_forGemma_May2021/catch_to_CPUE_AKFIN/Haul Descriptions.csv", fileEncoding = "UTF-8-BOM")

data_hauls <- levels(factor(akfin_data$hauljoin))
zero_hauls <- setdiff(levels(factor(hauls$Haul.Join.ID)), data_hauls) # assuming that if there are no records from a haul, the catch in that haul was 0 for this species

# make a data frame to bind by row
zero_catches <- hauls %>% filter(Haul.Join.ID %in% zero_hauls) %>% 
  select(Year, Haul.Join.ID, Ending.Latitude..dd., Ending.Longitude..dd., Bottom.Depth..m.) %>% 
  mutate(species_code = rep(NA, length(Year)),
         name = rep(NA, length(Year)),
         biom_kgkm2 = rep(0, length(Year)),
         num_km2 = rep(0, length(Year))) %>%
  set_names(names(akfin_data))

# attach by row to race_data
akfin_data <- rbind(akfin_data, zero_catches)
# ditch hauls with empty lat or lon
akfin_data <- akfin_data %>% filter(!is.na(lat) | !is.na(lon))
# and with NA depths
akfin_data <- akfin_data %>% filter(!is.na(depth))

# DFO CPUE

path_to_surveys <- "C:/Users/arove/Documents/GOA/SDM/Canada/data/surveys"
all_surveys <- dir(path_to_surveys)

# drop WCHG
all_surveys <- setdiff(all_surveys, "WCHG")

get_catch <- function(survey_area){
  sa <- survey_area
  sa_path <- paste(path_to_surveys,sa,sep="/")
  catch <- read.csv(paste(sa_path,paste(sa,"catch.csv",sep="_"),sep="/"))
  catch$Survey.area <- sa
  return(catch)
}
catch_all <- rbindlist(lapply(all_surveys,FUN = get_catch))

# join Atlantis group lookup information
atlantis_key <- read.csv("C:/Users/arove/Documents/GOA/SDM/Canada/data/dfo_species_atlantis.csv")
atlantis_key <- atlantis_key %>% filter(Atlantis.group != "?")
atlantis_groups <- read.csv("C:/Users/arove/Documents/GOA/SDM/Canada/data/GOA_Groups.csv", fileEncoding = "UTF-8-BOM")
atlantis_groups <- atlantis_groups %>% select(Code,Name,LongName)
#join into one key with long names
atlantis_key <- atlantis_key %>% left_join(atlantis_groups, by = c("Atlantis.group"="Code"))

#join catch data with Atlantis key by scientific name, which seems to be unique to each record in the DFO data
catch_all <- catch_all %>% left_join(atlantis_key %>% select(Scientific.name,Atlantis.group,Name), by = "Scientific.name")

# drop the NA in the groups - it means it is one of the undecided
catch_all <- catch_all %>% filter(!is.na(Atlantis.group))

# concatenate effort information from the three survey areas
get_effort <- function(survey_area){
  sa <- survey_area
  sa_path <- paste(path_to_surveys,sa,sep="/")
  effort <- read.csv(paste(sa_path,paste(sa,"effort.csv",sep="_"),sep="/"))
  return(effort)
}
effort_all <- rbindlist(lapply(all_surveys,FUN = get_effort)) # 4533 sets as of May 2021
# how many sets are deeper than 1000 m?
# nrow(effort_all %>% filter(Bottom.depth..m. > 1000))/nrow(effort_all)*100 # less than 1%


#This chunk will be looped for each Atlantis group. 

ag <- catch_all %>% select(Atlantis.group) %>% distinct() %>% pull()

dfo_cpue <- function(this_group) {
  this_group <- this_group
  
  species_catch <- catch_all %>% filter(Name==this_group)
  species_catch <- species_catch %>% 
    group_by(Survey.area,Survey.Year,Trip.identifier,Set.number,Atlantis.group,Name) %>% 
    summarise(Catch.weight.kg = sum(Catch.weight..kg.), Catch.count = sum(Catch.count..pieces.)) %>%
    ungroup()
  
  species_all_hauls <- species_catch %>% full_join(effort_all) # Joining, by = c("Survey.Year", "Trip.identifier", "Set.number")
  
  # add one column with a combination of trip and set, and one for the name of the area
  species_all_hauls <- species_all_hauls %>% mutate(hauljoin = paste(Trip.identifier,Set.number,sep="_"))
  
  # calculate area swept in km2, and CPUE for weight and numbers as biom/aream/1000
  species_all_hauls <- species_all_hauls %>% mutate(area_sweptkm2 = Distance.towed..m.*Trawl.door.spread..m./1e+6,
                                                    cpue_kgkm2 = Catch.weight.kg/area_sweptkm2,
                                                    cpue_numkm2 = Catch.count/area_sweptkm2)
  
  # extract month here - RACE data is 6,7,8, but these seem to start in May. Come back to this and try with overlapping months only
  # species_all_hauls <- species_all_hauls %>% mutate(month = month(ymd(Set.date))) %>% filter(month %in% 6:8)
  
  #assume that NA catches are 0, since they come from sets with no catch
  species_all_hauls$cpue_kgkm2[which(is.na(species_all_hauls$cpue_kgkm2))] <- 0
  
  #filter out NA depth
  species_all_hauls <- species_all_hauls %>% filter(!is.na(Bottom.depth..m.))
  
  # drop some columns and rename as for the RACE bottom trawl surveys
  species_all_hauls <- species_all_hauls %>% select(Survey.Year, hauljoin, Start.latitude, Start.longitude, Bottom.depth..m.,Atlantis.group,Name,cpue_kgkm2,cpue_numkm2) %>%
    set_names(c(
      "year",
      "hauljoin",
      "lat",
      "lon",
      "depth",
      "species_code",
      "name",
      "biom_kgkm2",
      "num_km2"))
  
  return(species_all_hauls)
}

dfo_data <- dfo_cpue(this_group)

# subset to same years

years <- intersect(akfin_data %>% select(year) %>% distinct() %>% pull(), dfo_data %>% select(year) %>% distinct() %>% pull())

akfin_data <- akfin_data %>% filter(year %in% years)
dfo_data <- dfo_data %>% filter(year %in% years)

# move this on to the Rmd now
