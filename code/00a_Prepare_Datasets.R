################################################################################
# Proyecto: Cuidado Rural - Intervención IDB 2025
# Archivo: 00a_Prepare_Datasets.R
# Descripción: Preparación de datasets a partir de archivos de entrada Excel; 
#              limpieza de nombres de columnas, selección de variables clave, 
#              filtrado y generación de archivos RDS para uso posterior.
# Autor: Juan Carlos Muñoz
# Fecha: 03/06/2025
# Notas:
#  - Se utilizan las librerías tidyverse, readxl y janitor.
#  - Cada sección procesa un dataset diferente: Proyectos FCS, Evaluación de Impacto,
#    y Proyectos Evaluados (TCR).
################################################################################

# Cargar librerías necesarias para la manipulación y lectura de datos
library(tidyverse)
library(readxl)
library(janitor)

#### --- Proyectos FCS -------------------------------------------
# Cargar el archivo de Proyectos Totales FCS desde Excel.
file1 <- "data/inputs/Proyectos_Totales_FCS.xlsx"

# Leer datos, limpiar nombres de columnas y seleccionar variables de interés.
proy_fcs <- read_excel(file1, sheet = "Hoja1", skip = 1) %>% 
    clean_names() %>% 
    select(
        id_proyecto    = codigo_del_proyecto_radicado_art,
        contrato       = no_contrato_4,
        macro_region   = macro_region,
        subregion_pdet = subregion_pdet,
        depto          = departamento,
        mpio           = nombre_del_os_municipio_s,
        mpio_n         = numero_municipios,
        estado         = estado_del_proyecto,
        benefi_tipo    = tipo_de_beneficiario_seleccionar_el_mas_representativo,
        benefi_total   = no_total_de_beneficiarios,
        benefi_hombres = no_de_hombres_beneficiarias,
        benefi_mujeres = no_de_mujeres_beneficiarias,
        tipo_proyecto  = tipologia_de_proyectos_areas_tematicas_a3_y_a4,
        producto       = producto_principal
    ) %>% 
    filter(!is.na(id_proyecto))  # Eliminar registros sin id_proyecto

# Visualizar los nombres de las columnas para verificar el proceso de limpieza.
names(proy_fcs)
nrow(proy_fcs)  # Verificar el número de filas

# Guardar el dataset Proyectos FCS en formato RDS para uso posterior
saveRDS(proy_fcs, file = "data/derived/00_ds_proyectos_FCS.rds")


#### --- Proyectos Evaluación de Impacto -------------------------
# Cargar archivo de Evaluación de Impacto desde Excel.
file2 <- "data/inputs/Muestra_Evaluacion_FCS.xlsx"

# Leer datos, limpiar nombres y seleccionar las variables relevantes.
proy_eval <- read_excel(file2, sheet = "muestra con datos contactos") %>% 
    clean_names() %>% 
    select(
        id_proyecto  = id_proyecto,
        muestra_eval = estado_final,
        tratamiento  = tratamiento,
        mpio1_cod    = municipio1,
        mpio1_nm     = nombre_mun1,
        mpio2_cod    = municipio2,
        mpio2_nm     = nombre_mun2,
        mpio3_cod    = municipio3,
        mpio3_nm     = nombre_mun3
    )

# Visualizar los nombres de las columnas para confirmar el proceso.
names(proy_eval)
nrow(proy_eval)  # Verificar el número de filas

# Guardar el dataset Evaluación de Impacto en formato RDS.
saveRDS(proy_eval, file = "data/derived/00_ds_proyecto_evaluacion.rds")


#### --- Proyectos Evaluados - TCR -------------------------------
# Cargar archivo de Proyectos Evaluados (Infraestructura Cuidado) desde Excel.
file3 <- "data/inputs/Proyectos_Infraestructura_Cuidado.xlsx"

# Leer la primera hoja, correspondiente a propuestas activos evaluadas.
proy_tcr0 <- read_excel(file3, sheet = "Propuestas activos evaluadas ", skip = 1) %>% 
    clean_names() %>% 
    select(
        contrato   = x1,
        viabilidad = x2
    )

# Leer la segunda hoja, correspondiente a propuestas activos TCR a noviembre.
proy_tcr1 <- read_excel(file3, sheet = "Propuestas activos TCR a nov. ") %>% 
    clean_names() %>% 
    select(
        contrato = numero_contrato,
        activos  = activos_a_15_noviembre
    )

# Unir ambos datasets por el campo 'contrato'
proy_tcr <- merge(proy_tcr0, proy_tcr1, by = "contrato", all.x = TRUE)

# Visualizar los nombres de las columnas resultantes para confirmar la unión.
names(proy_tcr)
nrow(proy_tcr)  # Verificar el número de filas

# Guardar el dataset Proyectos Evaluados (TCR) en formato RDS.
saveRDS(proy_tcr, file = "data/derived/00_ds_proyecto_tcr.rds")
