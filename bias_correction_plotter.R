# This code reads AFSC and DFO bottom trawl data, and reads/calculates CPUE for all Atlantis functional groups. It then subsets to
# data points in the SE GOA and N BC, between 54 and 55 N. The two surveys meet approximately at 54.5N. 
# It then calculates average CPUE per species over the 2005-2019 period. The goal is to identify consistent between-survey bias in the
# catchability of some species. Once such bias (i.e. difference in mean CPUE) is identified, we can apply it to correct the DFO data.
# We correct the DFO data no matter where the skew is because we have DFO data for fewer years and smaller area than GOA data.
# Some observations:
# 1. An attempt was made to characterize bias by year. This was dropped because the sample size for the AFSC is too small.
# 2. In general, sample size is different, i.e. there are a lot more tows for the DFO sets than for AFSC.
# 3. Initially I excluded the WCHG data, since we do not use those for sdmTMB (for now). However, that data contains shelf and slope information
# for the BC part of the model. Hecate Strait is a large flat (mud? sand? rock?), and as such likely a different habitat from the GOA shelf. For this
# reason, WCHG is actually useful to have here. Since we are not considering years for the purposes of this, it does not mater that that is the only
# area sampled in even years.
# 4. A proper depth structure is difficult to achieve. FOr example, one would like to have bias specific to depth bins. However, AFSC data points 
# are not enough for this. For this reason, we use 200 m as arbitrary cutoff between "shallow" and "deep" areas, to keep at least some depth
# structure in the bias correction.
# 5. Points 1 and 4 mean that we assume that: (a) the bias does not change over time; (b) the bias may change at different depths.
# get back to this after reading about differences in the gear setup (e.g. height above bottom). Fishing protocol is similar (3 knots,
# ~15 minutes tows, skipper decides whether a place can be towed or not, etc.). So what could cause a bias then? Is there a reason to believe that
# differences are due to sampling bias and not to differences in habitat?

library(tidyverse)
library(data.table)
library(sf)
library(maps)
library(mapdata)
library(viridis)

select <- dplyr::select

#############################################################################################
# READ data. Out of function, do once.
# CPUE from cpue.RData and hauls for zero cathces, CPUE from DFO original data and hauls
# AKFIN CPUE (species already mapped to Atlantis groups, will need to add zero catches)

load("C:/Users/arove/Documents/GOA/SDM/sdmTMB_forGemma_May2021/catch_to_CPUE_AKFIN/cpue.Rdata")
akfin_data <- cpue
rm(cpue)

# add empty hauls
hauls <- read.csv("C:/Users/arove/Documents/GOA/SDM/sdmTMB_forGemma_May2021/catch_to_CPUE_AKFIN/Haul Descriptions.csv", fileEncoding = "UTF-8-BOM")

# DFO CPUE

path_to_surveys <- "C:/Users/arove/Documents/GOA/SDM/Canada/data/surveys"
all_surveys <- dir(path_to_surveys)

# drop WCHG
#all_surveys <- setdiff(all_surveys, "WCHG") # see notes from chat with Sean
#all_surveys <- "WCHG" # to look at data on the slope only

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
#######################################################################

# function that gets data for each Atlantis functional group

get_bias_correction <- function(this_group){
  akfin_data <- akfin_data %>% filter(CN == this_group)
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
  
  #DFO
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
  dfo_data <- species_all_hauls %>% select(Survey.Year, hauljoin, Start.latitude, Start.longitude, Bottom.depth..m.,Atlantis.group,Name,cpue_kgkm2,cpue_numkm2) %>%
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
  
  # subset to same years
  #years <- intersect(akfin_data %>% select(year) %>% distinct() %>% pull(), dfo_data %>% select(year) %>% distinct() %>% pull())
  years <- seq(2005,2019,1) # just capture everything since the inception of the DFO surveys (except 2003, because they did not sample up there)
  
  akfin_data <- akfin_data %>% filter(year %in% years)
  dfo_data <- dfo_data %>% filter(year %in% years)
  
  # add survey factor
  akfin_data <- akfin_data %>% mutate(survey = "AFSC")
  dfo_data <- dfo_data %>% mutate(survey = "DFO")
  
  # stitch them
  all_data <- rbind(akfin_data, dfo_data) # CAUTION: assuming that the projection is the same - go check
  all_data <- all_data %>% filter(lat>=54 & lat <= 55 & lon > -135)
  # all_data <- all_data %>% filter(year != 2003)
  
  # add depth factor, either as depth bins or as shallow/deep
  max_depth <- max(all_data$depth)
  #all_data <- all_data %>% mutate(depth_bin = cut(all_data$depth, breaks = seq(0,max(all_data$depth),100), labels = seq(100,max(all_data$depth),100)))
  all_data <- all_data %>% mutate(depth_bin = ifelse(all_data$depth<max_depth/2,"Shallow","Deep"))
  # alternative for data including WCHG, which is very deep. Use break at 200 m instead (one of our Atlantis breaks)
  all_data <- all_data %>% mutate(depth_bin = ifelse(all_data$depth<200,"Shallow (<200m)","Deep (>200m)"))
  
  mean_cpues <- all_data %>% group_by(survey,depth_bin) %>%# group_by(survey,year,depth_bin) %>%
    summarise(mean_cpue = mean(biom_kgkm2), se_cpue = sd(biom_kgkm2)/sqrt(length(biom_kgkm2)), data_points = length(biom_kgkm2))
  
  p <- ggplot(mean_cpues)+
    geom_bar(aes(x = survey, y = mean_cpue, fill = survey), position = "dodge", stat = "identity")+
    geom_errorbar(aes(x = survey, y = mean_cpue, ymin = mean_cpue, ymax = mean_cpue+se_cpue), 
                  position = "dodge", stat = "identity")+
    geom_text(aes(x = survey, y = mean_cpue-(mean_cpue*0.15), label = data_points), 
              position = "dodge", size = 3)+
    theme_minimal()+
    labs(title = this_group)+
    facet_wrap(~depth_bin)
  p
  
  correction_shallow <- mean_cpues %>% filter(depth_bin == "Shallow (<200m)" & survey == "AFSC") %>% select(mean_cpue) %>% pull()/
    mean_cpues %>% filter(depth_bin == "Shallow (<200m)" & survey == "DFO") %>% select(mean_cpue) %>% pull()
  
  correction_deep <- mean_cpues %>% filter(depth_bin == "Deep (>200m)" & survey == "AFSC") %>% select(mean_cpue) %>% pull()/
    mean_cpues %>% filter(depth_bin == "Deep (>200m)" & survey == "DFO") %>% select(mean_cpue) %>% pull()
  
  corrs <- data.frame("group" = this_group, "shallow" = correction_shallow, "deep" = correction_deep)
  
  ggsave(paste("C:/Users/arove/Documents/GOA/SDM/Bias_correction/bias_images3/", this_group, ".png", sep = ""), p,
         width = 4.2, height = 3, dpi = 300, units = "in")
  
  return(corrs)
}

#######################################################
# identify the groups in common
groups_akfin <- akfin_data %>% select(CN) %>% distinct() %>% pull()
groups_dfo <- catch_all %>% select(Name) %>% distinct() %>% pull()

ag <- intersect(groups_akfin,groups_dfo) # these will be the groups in common

bias_corrections <- lapply(ag,get_bias_correction)

corr_factors <- rbindlist(bias_corrections)

write.csv(corr_factors,"correction_factors.csv",row.names = FALSE) # CPUE values in boxes >91 to be multiplied by these factors
