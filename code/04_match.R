# ============================================
# RURAL CARE — PSM
# ============================================
rm(list = ls())
library(dplyr)
library(stringr)
library(MatchIt)
library(writexl)

# -------------
set.seed(123)
# ---- Paths ----
inputs   <- "C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/data/inputs"
outputs  <- "C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/data/derived"
maps     <- "C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/data/inputs/Colombia_Division"
maps_out <- "C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/img"
# ------- Data -------
load(file.path(outputs, "data_mediobajo_10.rda"))  
load(file.path(outputs,"selected_sample.rda"))
total <- data_final3
tratados <- best_sample
rm(data_final3, best_sample)

# ----- Treatments vs controls -----
project <- total %>%
  mutate(treated = as.integer(no %in% tratados$no))
# ---- Cleaning data ------
vars_keep <- c("no","sector_de_la_actividad_productiva","departamento.x", "municipio",
               "numero_de_mujeres_organizacion_pct", "mujeres_transgenero_pct", "otro_genero_pct" ,
               "mujeres_transgenero_pc","total_personas","orientacion_sexual_hetero_pct",
               "orientacion_gay_pct", "orientacion_bisexual_pct", "orientacion_otro_pct", 
               "orientacion_no_informa_pct", "personas_entre_los_18_28_anos_pct", "personas_mayores_de_60_anos_pct",
               "indigenas_pct", "rrom_pct", "raizales_pct",  "palenqueras_pct", "negras_afrocolombianas_pct",
               "sin_reconocimiento_etnico_pct","size_etnia_total_pct", "personas_sisben_a_pct",
               "personas_sisben_b_pct", "personas_sisben_c_pct" , "persona_sisben_d_pct",
               "personas_reincroporadas_pct", "personas_lideres_pct", "victimas_pct", "codigo_dane", "nivel_riesgo", "mpio","treated")

data_2 <- project %>% select(any_of(vars_keep))

# ----- Cleaning Data ----- 
Data_3 <- data_2 %>%
  mutate(
    sector_de_la_actividad_productiva       = ifelse(is.na(sector_de_la_actividad_productiva), "NO INFO", sector_de_la_actividad_productiva),
    numero_de_mujeres_organizacion_pct      = ifelse(is.na(numero_de_mujeres_organizacion_pct), 0, numero_de_mujeres_organizacion_pct),
    mujeres_transgenero_pct                 = ifelse(is.na(mujeres_transgenero_pct), 0, mujeres_transgenero_pct),
    total_personas                          = ifelse(is.na(total_personas), 0, total_personas),
    orientacion_sexual_hetero_pct           = ifelse(is.na(orientacion_sexual_hetero_pct),0, orientacion_sexual_hetero_pct),
    orientacion_gay_pct                     = ifelse(is.na(orientacion_gay_pct), 0 , orientacion_gay_pct),
    orientacion_bisexual_pct                = ifelse(is.na(orientacion_bisexual_pct), 0 ,orientacion_bisexual_pct),
    personas_entre_los_18_28_anos_pct       = ifelse(is.na(personas_entre_los_18_28_anos_pct), 0 , personas_entre_los_18_28_anos_pct),
    personas_mayores_de_60_anos_pct         = ifelse(is.na(personas_mayores_de_60_anos_pct),0 , personas_mayores_de_60_anos_pct),
    indigenas_pct                           = ifelse(is.na(indigenas_pct),0, indigenas_pct),
    rrom_pct                                = ifelse(is.na(rrom_pct), 0, rrom_pct),
    raizales_pct                            = ifelse(is.na(raizales_pct), 0 ,raizales_pct),
    palenqueras_pct                         = ifelse(is.na(palenqueras_pct), 0 ,palenqueras_pct),
    negras_afrocolombianas_pct              = ifelse(is.na(negras_afrocolombianas_pct), 0 , negras_afrocolombianas_pct),
    sin_reconocimiento_etnico_pct           = ifelse(is.na(sin_reconocimiento_etnico_pct), 0 , sin_reconocimiento_etnico_pct),
    personas_sisben_a_pct                   = ifelse(is.na(personas_sisben_a_pct), 0 ,personas_sisben_a_pct),
    personas_sisben_b_pct                   = ifelse(is.na(personas_sisben_b_pct), 0 , personas_sisben_b_pct),
    personas_sisben_c_pct                   = ifelse(is.na(personas_sisben_c_pct),0, personas_sisben_c_pct),
    persona_sisben_d_pct                    = ifelse(is.na(persona_sisben_d_pct), 0 , persona_sisben_d_pct),
    personas_reincroporadas_pct             = ifelse(is.na(personas_reincroporadas_pct), 0 , personas_reincroporadas_pct),
    personas_lideres_pct                    = ifelse(is.na(personas_lideres_pct),0, personas_lideres_pct),
    victimas_pct                            = ifelse(is.na(victimas_pct),0,victimas_pct)
    )

  # PSC
 f_psm <- treated ~ sector_de_la_actividad_productiva + 
  total_personas  + orientacion_gay_pct + indigenas_pct + 
  personas_sisben_a_pct + personas_sisben_b_pct 
 
 psm_model <- matchit(
   formula     = f_psm,
   data        = Data_3,
   method      = "nearest",
   distance    = "logit",
   ratio       = 1,
   replace     = FALSE,
   caliper     = 0.2,
   std.caliper = TRUE,
   estimand    = "ATT"
 )
 summary(psm_model)
             
# export 
 matched_data <- match.data(psm_model)
 matched_treated  <- matched_data %>% filter(treated == 1 & weights > 0)
 matched_control  <- matched_data %>% filter(treated == 0 & weights > 0)
 pairs_tbl <- matched_data %>% filter(!is.na(subclass)) %>% select(subclass, no, treated)
 
 excel_path <- file.path(outputs, "matched_projects_Data3.xlsx")
 
 writexl::write_xlsx(
   list(
     matched_all      = matched_data,
     matched_treated  = matched_treated,
     matched_controls = matched_control,
     pairs            = pairs_tbl
   ),
   path = excel_path
 )



