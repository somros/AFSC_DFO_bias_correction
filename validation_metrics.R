# code to stitch together the validation metrics for AFSC and DFO
library(tidyverse)
library(data.table)

akfin_files <- list.files("../sdmTMB_stages/output/validation_tables/", full.names = TRUE)
dfo_files <- list.files("../Canada_stages/outputs/validation_tables/", full.names = TRUE)

read_and_name <- function(x){
  y <- read.csv(x)
  y <- y %>% mutate(Filename = x) %>% distinct()
}

akfin_val <- rbindlist(lapply(akfin_files, read_and_name))

dfo_val <- rbindlist(lapply(dfo_files, read_and_name))

#write out
write.csv(akfin_val,"akfin_validation_stages.csv",row.names = FALSE)
write.csv(dfo_val,"dfo_validation_stages.csv",row.names = FALSE)
