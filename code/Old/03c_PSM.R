# RURAL CARE 
# (PSM) TO IDENTIFY CONTROL PROJECTS
rm(list = ls())
# PACKAGES
library(MatchIt)
library(dplyr)
set.seed(123)
# LOAD DATA
source("C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/code/02b_SamplingRegions.R")

projects_4 <- projects_4 %>%
  mutate(
    treated = ifelse(id_proyecto %in% best_sample$id_proyecto, 1, 0)
  )



# COVARIATES
projects_5 <- projects_4 %>%
  mutate(
    area_def = ifelse(is.na(area_def), 0, area_def),
    tipo_ben_homologado = ifelse(is.na(tipo_ben_homologado), "NO INFO", tipo_ben_homologado),
    tipo_produccion_homologado = ifelse(is.na(tipo_produccion_homologado), "NO INFO", tipo_produccion_homologado),
    prestamo_tot_proy = ifelse(is.na(prestamo_tot_proy), 0, prestamo_tot_proy)
  )

# RUN PSM
psm_model <- matchit(treated ~ region + propuesta_inicial + carta_interes + 
                       meses + A1_restauracion + A2_psa + A3_agrop_agroi + A4_negverdes_noagr +
                       area_def + beneficiarios+ indigenas,
                     data = projects_5,
                     method = "nearest",
                     ratio = 1
)
# GET MATCHED DATA
matched_data <- match.data(psm_model)

# EXPORT RESULTS
writexl::write_xlsx(matched_data, "C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/data/derived/matched_projects.xlsx")




