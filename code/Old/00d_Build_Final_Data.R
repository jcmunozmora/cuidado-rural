# RURAL CARE 
# MUNICIPALITIES AND PROJECTS RISK 

rm(list = ls())
# PACKAGES
library(dplyr)
library(ggplot2)  
library(readxl)
library(tidyr)
library(stringr)
library(stringi)
library(readxl)

#  LOAD DATA
ptotal_2 <- readRDS("C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/data/derived/01_data_cleaned.rds")
ptotal_3 <- ptotal_2[ptotal_2$viable_2 != "No viable", ]
#SAVE FINAL DATA
saveRDS(ptotal_2, file = "C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/data/derived/01_final_data.rds")


table(ptotal_2$viable_2)

