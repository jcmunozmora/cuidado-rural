################################################################################
# Proyecto: Cuidado Rural - Intervención IDB 2025
# Archivo: 00b_Build_Datasets.R
# Descripción: Construcción de datasets combinados a partir de los archivos RDS generados.
#              Se combinan los datasets de Proyectos FCS, Evaluación y TCR para producir 
#              un dataset final que incluye indicadores de si la información de evaluación 
#              y la viabilidad TCR están presentes.
# Autor: Juan Carlos Muñoz
# Fecha: 03/06/2025
################################################################################

# Cargar librerías necesarias para la manipulación de datos
library(dplyr)
library(readxl)
library(janitor)    

#### --- Abrir Archivos -------------------------------------------
# Cargar el dataset de Proyectos FCS
proy_fcs_ds  <- readRDS("data/derived/00_ds_proyectos_FCS.rds")
nrow(proy_fcs_ds) # 201 Proyectos (confirmar número de observaciones)

# Cargar el dataset de Evaluación de Impacto
proy_eval_ds  <- readRDS("data/derived/00_ds_proyecto_evaluacion.rds")
nrow(proy_eval_ds) # Número de registros en evaluación

# Cargar el dataset de Proyectos TCR
proy_tcr_ds   <- readRDS("data/derived/00_ds_proyecto_tcr.rds")
nrow(proy_tcr_ds) # Número de registros en TCR

### -- Crear el dataset combinado -------------------------------------------
# Se realizan dos operaciones de merge:
# 1. Se combinan proy_fcs y proy_eval usando la clave 'id_proyecto', 
#    aplicando un merge completo (all.x = TRUE, all.y = TRUE) para conservar todos los registros.
#    Además, se crean dos nuevas variables:
#       - eval_muestra: Indica "Incluido" cuando 'muestra_eval' no es NA y "No incluido" en caso contrario.
#       - eval_encue: Indica "Muestra Eval" cuando 'tratamiento' no es NA y "Excluido Eval" caso contrario.
# 2. Se unen los datos anteriores con proy_tcr usando la clave 'contrato' para agregar la 
#    información de viabilidad del TCR, y se crea la variable 'tcr_incluido', que indica 
#    "Con IC-TCR" cuando viabilidad es "viable" y "Sin IC-TCR" en caso contrario.

ds_fcs <- merge(proy_fcs_ds, proy_eval_ds, by = "id_proyecto", all.x = TRUE, all.y = TRUE) %>%
    merge(proy_tcr_ds, by = "contrato", all.x = TRUE, all.y = TRUE) %>%
    mutate(
        eval_muestra = if_else(!is.na(muestra_eval), "Incluido", "No incluido"),
        eval_encue   = if_else(!is.na(tratamiento), "Muestra Eval", "Excluido Eval")
    ) %>%
    #### Integrar la información de los Proyectos TCR a partir del contrato
     mutate(
        tcr_incluido = if_else(viabilidad == "viable", "Con IC-TCR", "Sin IC-TCR"),
        tcr_incluido = if_else(!is.na(viabilidad), tcr_incluido, "Sin IC-TCR")
    ) 

# Last chekcs
nrow(ds_fcs) # Verificar el número de registros en el dataset final
table(ds_fcs$eval_encue)
table(ds_fcs$tcr_incluido)
table(ds_fcs$eval_encue,ds_fcs$tcr_incluido) # Verificar


# Guardar el dataset final en formato RDS para uso posterior
saveRDS(ds_fcs, file = "data/derived/01_ds_completa.rds")
