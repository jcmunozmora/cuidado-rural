# =======================
# CUOTAS + MUESTREO 
# =======================
rm(list = ls())

# ---- Packages ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readxl,
  janitor,
  dplyr,
  tidyr,
  forcats,
  stringr,
  ggplot2,
  scales,
  sf,
  writexl
)

# ---- Include maps ----
source("code/aux_maps.R")

# ---- Paths ----
inputs   <- "data/inputs"
outputs  <- "data/derived"
maps     <- "data/inputs/Colombia_Division"
maps_out <- "img"

# ------- Data -------
load(file.path(outputs, "ds_riesgo_bajomedio.rda")) 
set.seed(2025)

# ----- Final Data ----
data <- ds_end %>%
  filter(!is.na(`departamento.x`) & `departamento.x` != "") %>%
  distinct(no, .keep_all = TRUE)  

nrow(data %>% distinct(mpio))

# ----- Mpios data base----
