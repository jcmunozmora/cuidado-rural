# Rural Care
# Maps

rm(list = ls())
# PACKAGES
library(ggplot2)
library(sf)       
library(dplyr)
library(viridis)  
library(tmap)     
library(readr)
library(readxl)
library(tidyr)
library(stringi)


# Functions 
tipo_moda <- function(x) {
  freq <- table(x)
  if (length(freq) == 0) {
    return(NA)  
  } else {
    return(names(freq)[which.max(freq)])
  }
}


output_path <- "C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/img/maps/"

# Data -  Shape
path <- "C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/data/inputs/Colombia_Division/COL_adm1.shp"
# Load 
deptos <- st_read(path)
plot(deptos$geometry)
# Data
clean_data <- readRDS("C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/data/derived/01_data_cleaned.rds")
risk_data1 <- readRDS("C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/data/derived/01_final_data.rds")
final_data <- read_excel("C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/data/derived/matched_projects.xlsx")


# Depto
clean_data$num_mpios <- as.numeric(clean_data$num_mpios)
clean_data$num_veredas <- as.numeric(clean_data$num_veredas)
clean_data$area_def <- as.numeric(clean_data$area_def)
clean_data$beneficiarios <- as.numeric(clean_data$beneficiarios)
clean_data$beneficiarios_hombres <- as.numeric(clean_data$beneficiarios_hombres)
clean_data$beneficiarios_mujeres <- as.numeric(clean_data$beneficiarios_mujeres)

departamento_summary <- clean_data %>%
  group_by(Departamento_1) %>%
  summarise(
    n_prop_inicial = sum(propuesta_inicial, na.rm = TRUE),
    n_proyectos = n(),  
    n_carta_interes = sum(carta_interes, na.rm = TRUE),
    max_meses = max(meses, na.rm = TRUE),
    suma_num_mpios = sum(num_mpios, na.rm = TRUE),
    max_num_mpios = max(num_mpios, na.rm = TRUE),
    max_num_veredas = max(num_veredas, na.rm = TRUE),
    suma_area_def = sum(area_def, na.rm = TRUE),
    suma_beneficiarios = sum(beneficiarios, na.rm = TRUE),
    suma_beneficiarios_muj = sum(beneficiarios_mujeres, na.rm = TRUE),
    sume_bemeficiarios_hom = sum(beneficiarios_hombres, na.rm = TRUE),
    tipo_ben_mas_frec = tipo_moda(tipo_ben_homologado),
    tipo_prod_mas_frec = tipo_moda(tipo_produccion_homologado)
  )
departamento_summary <- departamento_summary %>%
  filter(Departamento_1 != "0")

departamento_summary <- departamento_summary %>%
  mutate(ID_ESP = case_when(
    Departamento_1 == "ANTIOQUIA" ~ 5,
    Departamento_1 == "ARAUCA" ~ 81,
    Departamento_1 == "BOLIVAR" ~ 13,
    Departamento_1 == "CAQUETA" ~ 18,
    Departamento_1 == "CAUCA" ~ 19,
    Departamento_1 == "CESAR" ~ 20,
    Departamento_1 == "CHOCO" ~ 27,
    Departamento_1 == "CORDOBA" ~ 23,
    Departamento_1 == "GUAVIARE" ~ 95,
    Departamento_1 == "HUILA" ~ 41,
    Departamento_1 == "MAGDALENA" ~ 47,
    Departamento_1 == "META" ~ 50,
    Departamento_1 == "NARINO" ~ 52,
    Departamento_1 == "NORTE_DE_SANTANDER" ~ 54,
    Departamento_1 == "PUTUMAYO" ~ 86,
    Departamento_1 == "SUCRE" ~ 70,
    Departamento_1 == "TOLIMA" ~ 73,
    Departamento_1 == "VALLE_DEL_CAUCA" ~ 76
  ))
# Merge
map_data <- left_join(deptos, departamento_summary, by = c("ID_ESP" = "ID_ESP" ))

# Maps
# Numero de proyectos

ggplot(map_data) +
  geom_sf(aes(fill = n_proyectos), color = "white", size = 0.2) +
  geom_sf_text(aes(label = n_proyectos), size = 3, color = "black") +
  scale_fill_gradient(
    low = "#529985",  
    high = "#C26B51", 
    na.value = "grey90",  
    name = " "
  ) +
  theme_void() +  
  labs(
    title = " ",
    fill = " "
  )

ggsave(filename = paste0(output_path, "proyectos_por_departamento.png"),
       plot = last_plot(),
       width = 8, height = 6, dpi = 300)

# Numero de propuestas iniciales
ggplot(map_data) +
  geom_sf(aes(fill = n_prop_inicial), color = "white", size = 0.2) +
  geom_sf_text(aes(label = n_prop_inicial), size = 3, color = "black") +
  scale_fill_gradient(
    low = "#529985",  
    high = "#C26B51", 
    na.value = "grey90",  
    name = " "
  ) +
  theme_void() +  
  labs(
    title = " ",
    fill = " "
  )

ggsave(filename = paste0(output_path, "propuestas_iniciales.png"),
       plot = last_plot(),
       width = 8, height = 6, dpi = 300)

# Numero de propuestas iniciales
ggplot(map_data) +
  geom_sf(aes(fill = n_carta_interes), color = "white", size = 0.2) +
  geom_sf_text(aes(label = n_carta_interes), size = 3, color = "black") +
  scale_fill_gradient(
    low = "#529985",  
    high = "#C26B51", 
    na.value = "grey90",  
    name = " "
  ) +
  theme_void() +  
  labs(
    title = " ",
    fill = " "
  )

ggsave(filename = paste0(output_path, "cartas_interes.png"),
       plot = last_plot(),
       width = 8, height = 6, dpi = 300)

# Numero de municipios
ggplot(map_data) +
  geom_sf(aes(fill = suma_num_mpios), color = "white", size = 0.2) +
  geom_sf_text(aes(label =suma_num_mpios), size = 3, color = "black") +
  scale_fill_gradient(
    low = "#529985",  
    high = "#C26B51", 
    na.value = "grey90",  
    name = " "
  ) +
  theme_void() +  
  labs(
    title = " ",
    fill = " "
  )

ggsave(filename = paste0(output_path, "num_municipios.png"),
       plot = last_plot(),
       width = 8, height = 6, dpi = 300)

# Numero de municipios
ggplot(map_data) +
  geom_sf(aes(fill = max_num_veredas), color = "white", size = 0.2) +
  geom_sf_text(aes(label =max_num_veredas), size = 3, color = "black") +
  scale_fill_gradient(
    low = "#529985",  
    high = "#C26B51", 
    na.value = "grey90",  
    name = " "
  ) +
  theme_void() +  
  labs(
    title = " ",
    fill = " "
  )

ggsave(filename = paste0(output_path, "max_num_veredas.png"),
       plot = last_plot(),
       width = 8, height = 6, dpi = 300)

# Suma de area
ggplot(map_data) +
  geom_sf(aes(fill = suma_area_def), color = "white", size = 0.2) +
  geom_sf_text(aes(label =suma_area_def), size = 3, color = "black") +
  scale_fill_gradient(
    low = "#529985",  
    high = "#C26B51", 
    na.value = "grey90",  
    name = " "
  ) +
  theme_void() +  
  labs(
    title = " ",
    fill = " "
  )

ggsave(filename = paste0(output_path, "area.png"),
       plot = last_plot(),
       width = 8, height = 6, dpi = 300)

# Beneficiarios totales
ggplot(map_data) +
  geom_sf(aes(fill = suma_beneficiarios), color = "white", size = 0.2) +
  geom_sf_text(aes(label =suma_beneficiarios), size = 3, color = "black") +
  scale_fill_gradient(
    low = "#529985",  
    high = "#C26B51", 
    na.value = "grey90",  
    name = " "
  ) +
  theme_void() +  
  labs(
    title = " ",
    fill = " "
  )

ggsave(filename = paste0(output_path, "beneficiarios.png"),
       plot = last_plot(),
       width = 8, height = 6, dpi = 300)

# Beneficiarios totales
ggplot(map_data) +
  geom_sf(aes(fill = suma_beneficiarios_muj), color = "white", size = 0.2) +
  geom_sf_text(aes(label =suma_beneficiarios_muj), size = 3, color = "black") +
  scale_fill_gradient(
    low = "#529985",  
    high = "#C26B51", 
    na.value = "grey90",  
    name = " "
  ) +
  theme_void() +  
  labs(
    title = " ",
    fill = " "
  )

ggsave(filename = paste0(output_path, "beneficiarios_mujeres.png"),
       plot = last_plot(),
       width = 8, height = 6, dpi = 300)

# Beneficiarios hombres
ggplot(map_data) +
  geom_sf(aes(fill = sume_bemeficiarios_hom), color = "white", size = 0.2) +
  geom_sf_text(aes(label =sume_bemeficiarios_hom), size = 3, color = "black") +
  scale_fill_gradient(
    low = "#529985",  
    high = "#C26B51", 
    na.value = "grey90",  
    name = " "
  ) +
  theme_void() +  
  labs(
    title = " ",
    fill = " "
  )

ggsave(filename = paste0(output_path, "beneficiarios_hombres.png"),
       plot = last_plot(),
       width = 8, height = 6, dpi = 300)

#Beneficiario mas frecuente 
ggplot(map_data) +
  geom_sf(aes(fill = tipo_ben_mas_frec), color = "white", size = 0.2) +
  scale_fill_manual(
    values = c(
      "AFRODESCENDIENTE" = "#529985",
      "CAMPESINO - VICTIMAS"  = "#ACB955",
      "CAMPESINOS" = "#68838B",
      "INDIGENAS" = "goldenrod4",
      "NA"         = "gray90"
    ),
    na.value = "gray90",  
    name = " "
  ) +
  theme_minimal() +
  labs(title = " ") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )
ggsave(filename = paste0(output_path, "beneficiarios_tipo.png"),
       plot = last_plot(),
       width = 8, height = 6, dpi = 300)


#Beneficiario mas frecuente 
ggplot(map_data) +
  geom_sf(aes(fill = tipo_prod_mas_frec), color = "white", size = 0.2) +
  scale_fill_manual(
    values = c(
      "ACUICULTURA" = "#529985",
      "AGRICOLA"  = "#ACB955",
      "PECUARIO" = "#68838B",
      "NA"         = "gray90"
    ),
    na.value = "gray90",  
    name = " "
  ) +
  theme_minimal() +
  labs(title = " ") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )
ggsave(filename = paste0(output_path, "tipo_prod_mas_frec.png"),
       plot = last_plot(),
       width = 8, height = 6, dpi = 300)
################

final_data <- final_data %>%
  mutate(ID_ESP = case_when(
    Departamento_1 == "ANTIOQUIA" ~ 5,
    Departamento_1 == "ARAUCA" ~ 81,
    Departamento_1 == "BOLIVAR" ~ 13,
    Departamento_1 == "CAQUETA" ~ 18,
    Departamento_1 == "CAUCA" ~ 19,
    Departamento_1 == "CESAR" ~ 20,
    Departamento_1 == "CHOCO" ~ 27,
    Departamento_1 == "CORDOBA" ~ 23,
    Departamento_1 == "GUAVIARE" ~ 95,
    Departamento_1 == "HUILA" ~ 41,
    Departamento_1 == "MAGDALENA" ~ 47,
    Departamento_1 == "META" ~ 50,
    Departamento_1 == "NARINO" ~ 52,
    Departamento_1 == "NORTE_DE_SANTANDER" ~ 54,
    Departamento_1 == "PUTUMAYO" ~ 86,
    Departamento_1 == "SUCRE" ~ 70,
    Departamento_1 == "TOLIMA" ~ 73,
    Departamento_1 == "VALLE_DEL_CAUCA" ~ 76
  ))
final_data$aux1 <- 1
# Merge
departamento_summary_2 <- final_data %>%
  group_by(Departamento_1) %>%
  summarise(
    n_prop_inicial = sum(treated, na.rm = TRUE),
    aux_1 = sum(aux1, na.rm = TRUE)

    )

departamento_summary_2 <- departamento_summary_2 %>%
  mutate(ID_ESP = case_when(
    Departamento_1 == "ANTIOQUIA" ~ 5,
    Departamento_1 == "ARAUCA" ~ 81,
    Departamento_1 == "BOLIVAR" ~ 13,
    Departamento_1 == "CAQUETA" ~ 18,
    Departamento_1 == "CAUCA" ~ 19,
    Departamento_1 == "CESAR" ~ 20,
    Departamento_1 == "CHOCO" ~ 27,
    Departamento_1 == "CORDOBA" ~ 23,
    Departamento_1 == "GUAVIARE" ~ 95,
    Departamento_1 == "HUILA" ~ 41,
    Departamento_1 == "MAGDALENA" ~ 47,
    Departamento_1 == "META" ~ 50,
    Departamento_1 == "NARINO" ~ 52,
    Departamento_1 == "NORTE_DE_SANTANDER" ~ 54,
    Departamento_1 == "PUTUMAYO" ~ 86,
    Departamento_1 == "SUCRE" ~ 70,
    Departamento_1 == "TOLIMA" ~ 73,
    Departamento_1 == "VALLE_DEL_CAUCA" ~ 76
  ))

map_data_2 <- left_join(deptos, departamento_summary_2, by = c("ID_ESP" = "ID_ESP" ))

# Tratamiento
ggplot(map_data_2) +
  geom_sf(aes(fill = aux_1), color = "white", size = 0.2) +
  geom_sf_text(aes(label = aux_1), size = 3, color = "black") +
  scale_fill_gradient(
    low = "#529985",  
    high = "#C26B51", 
    na.value = "grey90",  
    name = " "
  ) +
  theme_void() +  
  labs(
    title = " ",
    fill = " "
  )

ggsave(filename = paste0(output_path, "tratamiento_control.png"),
       plot = last_plot(),
       width = 8, height = 6, dpi = 300)



