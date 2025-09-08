    # RURAL CARE 
    # CLEANING PROJECTS DATA 
    
    rm(list = ls())
    # PACKAGES
    library(dplyr)
    library(ggplot2)  
    library(readxl)
    library(tidyr)
    library(stringr)
    library(stringi)
    library(writexl)
    
    # DEFINE FILE PATHS
    inputs_path <- "C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/data/inputs/"
    outputs_path <- "C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/data/output/"
    #LOAD PROJECT DATA FROM EXCEL
    excel_sheets(paste0(inputs_path,"Proyectos_Totales_FCS.xlsx"))
    ptotal <- read_excel(paste0(inputs_path,"Proyectos_Totales_FCS.xlsx"), sheet = "Hoja1")
    # CLEAN COLUMN NAMES (SET FIRST ROW AS HEADER)
    colnames(ptotal) <- as.character(unlist(ptotal[1, ]))
    ptotal <- ptotal[-1, ]
    #RISK
    excel_sheets(paste0(inputs_path,"Matriz_riesgos_revisada.xlsx"))
    riesgos <- read_excel(paste0(inputs_path, "Matriz_riesgos_revisada.xlsx"), sheet = "Matriz")
    #REMOVE UNNECESSARY VARIABLES (columns)
    drop <- c(
      "Total de Hectáreas",
      "Restauración (Has)",
      "Avances en restauración",
      "PSA (Has)",
      "Avances en PSA",
      "Area acuerdos cero deforestacion (Has)",
      "Seguimiento al cumplimiento de Acuerdos cero deforestacion \r\n",
      "Incremento Area cobertura natural (Has)",
      "Seguimiento al cumplimiento  Incremento Area cobertura natural",
      "Familias que implementan prácticas productivas sostenibles",
      "No. Hombres Campesinos",
      "No. Mujeres Campesinos",
      "No. Afro-Hombres",
      "No. Afro-Mujeres",
      "No. Indi-Hombres",
      "No. Indi-Mujeres",
      "Victima",
      "Discapacitado",
      "UPAs que alcanzan su meta de productividad",
      "Unidades de Producción Agropecuarias (UPA) que adoptan sistemas productivos sostenibles",
      "Área bajo paisajes productivos sostenibles",
      "Estado alerta proceso implementacion",
      "Observaciones estado actual del proyecto",
      "Número de Informes Trimestrales entregados por la EEE",
      "Periodo ultimo informe trimestral entregado por la EEE",
      "Informes trimestrales Períodos pendientes",
      "Fecha estimada del proximo desembolso",
      "Acciones por desarrollar para la implementacion de los proyectos",
      "Aspectos relevantes de la gestión ambiental y social",
      "Avances en el cumplimiento de politicas de salvaguardas BID",
      "Compromisos y pendientes sobre la gestión ambiental y social",
      "FECHA LIQUIDACION CONTRATO",
      "proyectos evaluados viables",
      "Coordinador", 
      "Supervisor", 
      "Teléfono", 
      "No. Iniciativas PDET",
      "ESTADO DEL PROYECTO",
      "ENLACE AL PROYECTO ESTRUCTURADO",
      "Nombre de la Empresa Comercializadora",
      "Nombre de la(s) Asociación(es)",
      "Nombre del Proponente",
      "Nombre/Sigla de la Firma estructuradora",
      "Nombre de la Entidad Ejecutora Elegible (EEE)",
      "Otros Actores",
      "Recursos Cooperación Técnica - Emprendiemiento",
      "Recursos del Préstamo 4424/OC-CO - COL$",
      "Adición de Recursos del Préstamo 4424/OC-CO - COL$",
      "Recursos Préstamo/beneficiarios (Para Componente 2)",
      "Recursos Visión Amazonia",
      "Presupuesto planificadores prediales (preliminar)",
      "GPS (preliminar)", 
      "Total aportes Visión Amazonia",
      "Codigo SEPA",
      "Fecha firma contrato",
      "Fecha inicio\r\n(Acta de inicio)",
      "Fecha terminacion",
      "Polizas garantia",
      "Fecha aprobacion garantias",
      "Datos de la EEE",
      "Email de la EEE",
      "Fecha suscripcion otrosí",
      "Desembolso Recursos del Préstamo (1)", 
      "Fecha Desembolso 1",
      "Desembolso Recursos del Préstamo (2)",
      "Fecha Desembolso 2",
      "Desembolso Recursos del Préstamo (3)",
      "Fecha Desembolso 3",
      "Desembolso Adición de Recursos del Préstamo (1)",
      "Fecha Desembolso adición 1",
      "Desembolso Adición de Recursos del Préstamo (2)",
      "Fecha Desembolso (2)",
      "Desembolso visión amazonía (3)",
      "Fecha Desembolso (3)",
      "Desembolso Emprendimiento (1)", 
      "Desembolso Emprendimiento (2)", 
      "Desembolso Emprendimiento (3)", 
      "Total Desembolsado RECURSOS PRESTAMOS",
      "Total Desembolsado ADICIÓN RECURSOS PRESTAMO",
      "Pendiente por  desembolsar  Préstamo 4424/OC-CO - COL$",
      "Pendiente por desembolsar ADICIÓN RECURSOS PRESTAMO", 
      "Total desembolsado VISIÓN AMAZONÍA",
      "Pendiente por  desembolsar VISIÓN AMAZONÍA",
      "Total desembolsado COOPERACIÓN TÉCNICA",
      "Pendiente por  desembolsar EMPRENDIMIENTO",
      "Total  Ejecución\r\nRecursos",
      "SALDO DISPONIBLE (Desembolsos - Ejecutado)",
      "OBSERVACIONES \r\n (Solo aplica  para proyectos terminados en fase de liquidación indicando si hay recursos a reintegrar: Fecha y monto de   recursos a reintegrar al FCP)",
      "% Ejecución\r\nAportes contrapartida", 
      "Avance Fisico logrado en el mes (%)", 
      "% Fisico Acumulado", 
      "% Financiero",
      "Calificación Capacidad Institucional\r\nEEE  (*)",
      "Fecha Desembolso adición 2",
      "Desembolso visión amazonía (1)",
      "Fecha Desembolso (1)",
      "Desembolso visión amazonía (2)",
      "Subregión PDET"
    )
    ptotal_2 <- ptotal[, !(colnames(ptotal) %in% drop | is.na(colnames(ptotal)))]
    # REMOVE EMPTY IDPROJ
    ptotal_2 <- ptotal_2[!is.na(ptotal_2$No.contrato) & ptotal_2$No.contrato != "", ]
    #CREATE INDICATORS 
    ptotal_2$`proyectos con propuesta inicial` <- ifelse(is.na(ptotal_2$`proyectos con propuesta inicial`) | ptotal_2$`proyectos con propuesta inicial` == "", 0, 1)
    ptotal_2$`proyectos con carta de interés` <- ifelse(is.na(ptotal_2$`proyectos con carta de interés`)| ptotal_2$`proyectos con carta de interés` == "", 0,1)
    # SANITIZE COLUMN NAMESs
    colnames(ptotal_2) <- make.names(colnames(ptotal_2), unique = TRUE)
    # DROP EXTRA FIELDS
    drop <- c("No.contrato.1","Objetivos","Duracion.en.meses","Contrapartida.1","Ejecutado.C")
    ptotal_2 <- ptotal_2[,!(colnames(ptotal_2) %in% drop)]
    ptotal_2 <- ptotal_2[!(ptotal_2$No.contrato %in% c("846-2020", "732-2020")), ]
    # STANDARDIZE DEPARTMENT NAMES
    ptotal_2$Departamento <- gsub("\r\n", "-", ptotal_2$Departamento)
    ptotal_2$Departamento <- gsub("NORTE DE SANTANDER", "NORTE_DE_SANTANDER", ptotal_2$Departamento)
    ptotal_2$Departamento <- gsub("NORTE SANTANDER", "NORTE_DE_SANTANDER", ptotal_2$Departamento)
    ptotal_2$Departamento <- gsub("VALLE DEL CAUCA", "VALLE_DEL_CAUCA", ptotal_2$Departamento)
    ptotal_2$Departamento <- trimws(ptotal_2$Departamento)
    ptotal_2 <- separate(ptotal_2, Departamento, into = c("Departamento_1", "Departamento_2"), sep = "-|–|/| ", extra = "merge", fill = "right")
    ptotal_2$Departamento_1 <- stri_trans_general(ptotal_2$Departamento_1, "Latin-ASCII")
    ptotal_2$Departamento_2 <- stri_trans_general(ptotal_2$Departamento_2, "Latin-ASCII")
    ptotal_2$Departamento_1 <- toupper(ptotal_2$Departamento_1)
    ptotal_2$Departamento_2 <- toupper(ptotal_2$Departamento_2)
    ptotal_2$Departamento_1 <- gsub("BOLIVAR|BOLIVAR ", "BOLIVAR", ptotal_2$Departamento_1)
    ptotal_2$Departamento_1 <- gsub("^NORTE$", "NORTE_DE_SANTANDER", ptotal_2$Departamento_1)
    ptotal_2$Departamento_1 <- gsub("VALLE DEL CAUCA", "VALLE_DEL_CAUCA", ptotal_2$Departamento_1)
    ptotal_2$Departamento_2 <- gsub("-NARINO", "NARINO", ptotal_2$Departamento_2)
    # COMPONENTS 
    ptotal_2$Componentes <- gsub("C2-C1|C1 - C2", "C1-C2", ptotal_2$Componentes)
    # DROP EXTRA FIELDS
    drop <- c("C1","C1_PPAL","C2","Restauración..","PSA..","Incremento.Area.cobertura.natural...Has.","Contrapartida","Valor.Total.del.proyecto...Emprendiemiento..Prestamo...Contrapartida...Visión.Amazonia.")
    ptotal_2 <- ptotal_2[,!(colnames(ptotal_2) %in% drop)]
    
    
    # ================================
    # 1. LIMPIEZA DE MUNICIPIOS EN ptotal_2
    # ================================
    
    ptotal_2$muni <- ptotal_2$Nombre.del.os..Municipio.s.
    
    ptotal_2 <- ptotal_2 %>% 
      mutate(
        muni = str_replace_all(muni, ".*:\\s*", "") , # Borra todo antes del ":"
        muni = str_replace_all(muni, "\\r\\n|\\n|\\r|-|/| Y ", ","),  # Unifica separadores
        muni = str_replace_all(muni, "\\s+", " ")                     # Normaliza espacios
      ) %>%
      separate(muni, into = paste0("mun", 1:11), sep = ",", fill = "right", extra = "drop") %>%
      mutate(across(starts_with("mun"), ~ trimws(.))) %>%
      mutate(across(starts_with("mun"), ~ stri_trans_general(., "Latin-ASCII") %>% toupper())) %>%
      mutate(across(starts_with("mun"), ~ str_remove(., "\\s*\\([^\\)]+\\)"))) %>%
      mutate(across(starts_with("mun"), ~ ifelse(. == "" | . == "NARINO", NA, .)))
    
    # Corrección manual para ALGECIRAS
    ptotal_2 <- ptotal_2 %>%
      mutate(Departamento_1 = ifelse(mun1 == "ALGECIRAS", "HUILA", Departamento_1))
    # correccion de la guajira - guajira
    riesgos <- riesgos %>% 
      mutate(Departamento = ifelse(Departamento == "LA GUAJIRA", "GUAJIRA", Departamento))
    # Correcciones de la base de municipios
    ptotal_2 <- ptotal_2 %>%
      mutate(across(
        .cols = c(mun1, mun2, mun3, mun4, mun5, mun6, mun7, mun8, mun9, mun10, mun11),
        .fns = ~ ifelse(. == "LA GUAJIRA", "GUAJIRA",
                        ifelse(. == "RIOBLANCO TOLIMA", "RIOBLANCO",
                               ifelse(. == "VALLE DEL GAMUEZ", "VALLE DEL GUAMUEZ",
                                      ifelse(. == "MONETELIBANO", "MONTELIBANO",.))))
      ))
    
    
    
    
    
    # ================================
    # 2. PROCESAMIENTO DE LA BASE DE RIESGOS
    # ================================
    
    riesgos <- riesgos %>%
      rename(
        riesgo_2 = `...8`,
        tipo_riesgo = `...9`,
        notas = `...10`,
        riesgo = `Nivel Riesgo`
      ) %>%
      filter(!is.na(Municipio)) %>%
      fill(Departamento, .direction = "down") %>%
      mutate(
        Departamento = Departamento %>%
          str_replace_all("\\s+", " ") %>%
          str_trim() %>%
          stri_trans_general("Latin-ASCII") %>%
          toupper(),
        
        Municipio = Municipio %>%
          str_replace_all("\\s+", " ") %>%
          str_trim() %>%
          stri_trans_general("Latin-ASCII") %>%
          toupper()
      )
    
    # ================================
    # 3. FUNCIONES AUXILIARES
    # ================================
    
    # Limpieza profunda: acentos, invisibles, espacios, mayúsculas
    clean_key <- function(x) {
      x %>%
        stringi::stri_trans_general("Latin-ASCII") %>%             # Quita acentos
        stringi::stri_replace_all_regex("\\p{C}", "") %>%          # Borra invisibles
        iconv(from = "", to = "ASCII//TRANSLIT") %>%               # Reforzar limpieza
        stringr::str_replace_all("\\s+", " ") %>% 
        stringr::str_trim() %>%
        toupper()
    }
    
    # Normaliza espacios solamente
    normalize_spaces <- function(x) {
      x %>%
        stringr::str_replace_all("\\s+", " ") %>%
        stringr::str_trim()
    }
    
    # ================================
    # 4. CREACIÓN DE CLAVES AUXILIARES EN ambas bases
    # ================================
    
    # Normalizar departamentos
    ptotal_2 <- ptotal_2 %>%
      mutate(
        Departamento_1 = clean_key(Departamento_1),
        Departamento_2 = clean_key(Departamento_2)
      )
    
    # Crear claves en ptotal_2
    for (i in 1:11) {
      mun_col <- paste0("mun", i)
      aux1 <- paste0("aux_mun", i)
      aux2 <- paste0("aux2_mun", i)
      
      ptotal_2 <- ptotal_2 %>%
        mutate(
          !!aux1 := clean_key(paste(Departamento_1, .data[[mun_col]], sep = " - ")),
          !!aux2 := clean_key(paste(Departamento_2, .data[[mun_col]], sep = " - "))
        )
    }
    
    # Crear y limpiar clave en riesgos
    riesgos <- riesgos %>%
      mutate(aux_key = clean_key(paste(Departamento, Municipio, sep = " - ")))
    
    # ================================
    # 5. Reforzar limpieza antes del merge
    # ================================
    
    for (i in 1:11) {
      aux1 <- paste0("aux_mun", i)
      aux2 <- paste0("aux2_mun", i)
      
      ptotal_2 <- ptotal_2 %>%
        mutate(
          !!aux1 := normalize_spaces(as.character(.data[[aux1]])),
          !!aux2 := normalize_spaces(as.character(.data[[aux2]]))
        )
    }
    
    riesgos <- riesgos %>%
      mutate(aux_key = normalize_spaces(as.character(aux_key)))
    
    # ================================
    # 6. MERGE AUTOMÁTICO POR CADA MUNICIPIO
    # ================================
    
    for (i in 1:11) {
      mun_col <- paste0("mun", i)
      aux1 <- paste0("aux_mun", i)
      aux2 <- paste0("aux2_mun", i)
      
      ptotal_2 <- ptotal_2 %>%
        left_join(riesgos %>% select(aux_key, riesgo1 = riesgo), by = setNames("aux_key", aux1)) %>%
        left_join(riesgos %>% select(aux_key, riesgo2 = riesgo), by = setNames("aux_key", aux2)) %>%
        mutate(!!paste0("riesgo_", mun_col) := coalesce(riesgo1, riesgo2)) %>%
        select(-riesgo1, -riesgo2)
    }

# proof 
  aux <- ptotal_2 %>%
    select(Departamento_1, Departamento_2, mun1, mun2, mun3, mun4, mun5, mun6, mun7, mun8, mun9,mun10,mun11, riesgo_mun1, riesgo_mun2, riesgo_mun3, riesgo_mun4, riesgo_mun5, riesgo_mun6, riesgo_mun7, riesgo_mun8, riesgo_mun9, riesgo_mun10, riesgo_mun11)
# Selected Projects
  ptotal_2 <- ptotal_2 %>%
    mutate(
      viable = if_else(
        if_any(riesgo_mun1:riesgo_mun11, ~
                 replace_na(str_to_upper(str_trim(as.character(.))) == "ALTO", FALSE)
        ),
        "No viable",
        "Viable"
      )
    )
  
  ptotal_2 <- ptotal_2 %>%
    rowwise() %>%
    mutate(
      pct_alto = {
        # selecciona los valores de riesgo
        valores <- c_across(riesgo_mun1:riesgo_mun11)
        # normaliza y limpia
        valores_limpios <- str_to_upper(str_trim(as.character(valores)))
        # cuenta "Alto"
        n_alto <- sum(valores_limpios == "ALTO", na.rm = TRUE)
        # cuenta columnas no NA
        n_validos <- sum(!is.na(valores_limpios))
        # calcula porcentaje
        if (n_validos == 0) NA_real_ else n_alto / n_validos * 100
      }
    ) %>%
    ungroup()
  
  ptotal_2 <- ptotal_2 %>%
    mutate(
      viable_2 = if_else(
        pct_alto <= 60, 
        "Viable", 
        "No viable",
        missing = NA_character_
      )
    )

# RENAME AND CREATE AREA THEMATIC VARIABLES
ptotal_2 <- ptotal_2 %>%
  rename(
    A1_restauracion = "Area.Temática..A1...Restauración",
    A2_psa = "Area.Temática..A2...PSA",
    A3_agrop_agroi = "Area.Temática..A3...Proyectos.Agropecuarios.Agroindustriales",
    A4_negverdes_noagr = "Area.Temática..A4...Negocios.Verdes.No.Agropecuarios"
  )
ptotal_2 <- ptotal_2 %>%
  mutate(
    A1_restauracion = ifelse(!is.na(A1_restauracion), 1, 0),
    A2_psa = ifelse(!is.na(A2_psa), 1, 0),
    A3_agrop_agroi = ifelse(!is.na(A3_agrop_agroi), 1, 0),
    A4_negverdes_noagr = ifelse(!is.na(A4_negverdes_noagr), 1, 0)
  )
# TO NUMERIC
ptotal_2$Area.acuerdos.cero.deforestacion.C1..Has....ó.de.cero.deforestacion..ha..C2 <- 
  as.numeric(gsub(",", ".", gsub("[^0-9\\.]", "", ptotal_2$Area.acuerdos.cero.deforestacion.C1..Has....ó.de.cero.deforestacion..ha..C2)))
cols_to_numeric <- c("No..Total.de.Beneficiarios", "No..de.Mujeres.Beneficiarias", "No..de.Hombres.Beneficiarias")
ptotal_2[cols_to_numeric] <- lapply(ptotal_2[cols_to_numeric], as.numeric)
# CLEAN AND STANDARDIZE BENEFICIARY TYPE
ptotal_2 <- ptotal_2 %>%
  rename(tipo_ben = Tipo.de.Beneficiario..Seleccionar.el.más.representativo.)
ptotal_2 <- ptotal_2 %>%
  mutate(
    tipo_ben = tipo_ben %>%
      str_replace_all("[\r\n]", " ") %>%             
      str_squish() %>%                                
      str_to_upper() %>%                              
      stri_trans_general("Latin-ASCII")               
  )
ptotal_2 <- ptotal_2 %>%
  mutate(
    tipo_ben_homologado = case_when(
      str_detect(tipo_ben, "AFRODESCENDIENTE") ~ "AFRODESCENDIENTE",
      
      str_detect(tipo_ben, "CAMPESINO.*AFRO.*INDIGENA.*VICTIMA.*DISCAPACIDAD") ~ "CAMPESINO - AFROCOLOMBIANOS - INDIGENAS - VICTIMAS - CON DISCAPACIDAD",
      str_detect(tipo_ben, "CAMPESINO.*AFRO.*INDIGENA") ~ "CAMPESINO - AFROCOLOMBIANOS - INDIGENAS",
      str_detect(tipo_ben, "CAMPESINO.*INDIGENA.*VICTIMA") ~ "CAMPESINO - INDIGENAS - VICTIMAS",
      str_detect(tipo_ben, "CAMPESINO.*INDIGENA") ~ "CAMPESINO - INDIGENAS",
      str_detect(tipo_ben, "CAMPESINO.*VICTIMA") ~ "CAMPESINO - VICTIMAS",
      str_detect(tipo_ben, "CAMPESINA") ~ "CAMPESINOS",
      str_detect(tipo_ben, "CAMPESINO") ~ "CAMPESINOS",
      
      str_detect(tipo_ben, "INDIGENA") ~ "INDIGENAS",
      str_detect(tipo_ben, "REINCORPORADO") ~ "REINCORPORADO",
      
      TRUE ~ NA_character_
    )
  )
# INDICATOR IF PROJECT IS LINKED TO INDIGENOUS COMMUNITIES
ptotal_2 <- ptotal_2 %>%
  mutate(
    indigenas = if_else(
      tipo_ben_homologado %in% c(
        "CAMPESINO - AFROCOLOMBIANOS - INDIGENAS",
        "CAMPESINO - INDIGENAS",
        "CAMPESINO - INDIGENAS - VICTIMAS",
        "INDIGENAS"
      ),
      1, 0
    )
  )
#TYPE OF PRODUCTION
ptotal_2 <- ptotal_2 %>%
  rename(tipo_produccion = Tipología.de.proyectos..Áreas.Temáticas.A3.y.A4.)
ptotal_2 <- ptotal_2 %>%
  mutate(
    tipo_produccion = str_to_upper(tipo_produccion),  
    tipo_produccion_homologado = case_when(
      tipo_produccion %in% c("ACUÍCOLA", "ACUÍCULTURA") ~ "ACUICULTURA",
      tipo_produccion %in% c("AGRÍCOLA") ~ "AGRICOLA",
      tipo_produccion %in% c("AGROINDUSTRIAL") ~ "AGROINDUSTRIAL",
      tipo_produccion %in% c("APICULTURA") ~ "APICULTURA",
      tipo_produccion %in% c("ECOTURISMO", "TURISMO DE LA NATURALEZA") ~ "ECOTURISMO",
      tipo_produccion %in% c("FORESTAL", "PRODUCTO MADERABLE") ~ "FORESTAL",
      tipo_produccion %in% c("NEGOCIO VERDE NO AGROPECUARIO") ~ "NEGOCIO VERDE NO AGROPECUARIO",
      tipo_produccion %in% c("PECUARIO") ~ "PECUARIO",
      TRUE ~ tipo_produccion
    )
  )
# CATEGORIZE PRODUCTIVE ACTIVITY
ptotal_2 <- ptotal_2 %>%
  mutate(
    categ_act_prod = case_when(
      Categorización.proyectos.según.actividad.productiva %in% c("CULTIVO PERMANENTE", "CULTIVOS PERMANENTES") ~ "Cultivos Permanentes",
      Categorización.proyectos.según.actividad.productiva %in% c("CULTIVOS TRANSITORIOS", "CULTIVOS TRANSITORIOS Y VARIOS") ~ "Cultivos Transitorios",
      Categorización.proyectos.según.actividad.productiva == "CULTIVOS PERMANENTES Y SEMIPERMANENTES" ~ "Cultivos Permanentes y Semipermanentes",
      Categorización.proyectos.según.actividad.productiva %in% c("GANADERIA SOSTENIBLE", "GANADERÍA SOSTENIBLE") ~ "Ganadería Sostenible",
      Categorización.proyectos.según.actividad.productiva %in% c("PECUARIO DIFERENTE A GANADERÍA", "PECUARIOS DIFERENTES A GANADERÍA") ~ "Pecuarios No Ganadería",
      Categorización.proyectos.según.actividad.productiva == "FORESTAL: CONSERVACIÓN Y USO SOSTENIBLE DE BOSQUES" ~ "Forestal: Conservación y Uso Sostenible",
      Categorización.proyectos.según.actividad.productiva == "BIOCOMERCIO" ~ "Biocomercio",
      TRUE ~ Categorización.proyectos.según.actividad.productiva
    )
  )
# Loan / Total Projects
ptotal_2 <- ptotal_2 %>%
  mutate(X..Recursos.Préstamo.Vr.Total.Proyecto = as.numeric(X..Recursos.Préstamo.Vr.Total.Proyecto))
#DROP EXTRA VARIABLES 
drop <- c("Categorización.proyectos.según.actividad.productiva", "Modificaciones.contractuales","Producto.Principal","tipo_produccion")
ptotal_2 <- ptotal_2[,!(colnames(ptotal_2) %in% drop)]
#RENAME
ptotal_2 <- ptotal_2 %>%
  rename(
    propuesta_inicial  = "proyectos.con.propuesta.inicial" ,
    carta_interes = "proyectos.con.carta.de.interés",
    contrato = "No.contrato",
    id_proyecto = "Código.del.proyecto....Radicado.ART.",
    region = "Macro.Regíon",
    meses = "Duración.Ejecución...Meses.",
    nombre = "Nombre.del.Proyecto.Objeto",
    objetivo = "Objetivo.s..del.Proyecto",
    municipios = "Nombre.del.os..Municipio.s.",
    num_mpios = "Número.Municipios",
    veredas = "Nombre.de.la.s..Vereda.s.",
    num_veredas = "Número.de.Veredas",
    area_def = "Area.acuerdos.cero.deforestacion.C1..Has....ó.de.cero.deforestacion..ha..C2",
    beneficiarios = "No..Total.de.Beneficiarios",
    beneficiarios_mujeres = "No..de.Mujeres.Beneficiarias",
    beneficiarios_hombres = "No..de.Hombres.Beneficiarias",
    cat_PGAS = "Categoría.Salvaguardas.Ambientales.y.Sociales..PGAS.",
    prestamo_tot_proy = "X..Recursos.Préstamo.Vr.Total.Proyecto"
    )
# SAVE DATA "01_DATA_CLEANED"
saveRDS(ptotal_2, file = "C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/data/derived/01_data_cleaned.rds")
# Data To Excel 
write_xlsx(ptotal_2, "final_data.xlsx")
