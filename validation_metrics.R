# code to stitch together the validation metrics for AFSC and DFO
library(tidyverse)
library(data.table)

akfin_files <- list.files("../sdmTMB_forGemma_May2021/output/validation_tables/", full.names = TRUE)
dfo_files <- list.files("../Canada/outputs/knitted_output_afterSean/validation_tables/", full.names = TRUE)

akfin_val <- rbindlist(lapply(akfin_files, read.csv))

dfo_val <- rbindlist(lapply(dfo_files, read.csv))

#write out

write.csv(akfin_val,"akfin_validation.csv",row.names = FALSE)
write.csv(dfo_val,"dfo_validation.csv",row.names = FALSE)
