# =======================
# RIESGO 
# =======================
rm(list = ls())

# ---- Packages ----
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
library(ggplot2)
library(scales)
library(sf)

# ---- Paths ----
inputs   <- "C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/data/inputs"
outputs  <- "C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/data/derived"
maps     <- "C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/data/inputs/Colombia_Division"
maps_out <- "C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/img"

# ---- Data ----
load(file.path(outputs, "data_percent.rda"))  # <- asume objeto 'projects'
risk <- read_excel(file.path(inputs, "Matriz_riesgos_FOMUR.xlsx")) |> clean_names()

# ---- Merge proyectos + riesgos (normalizando municipio) ----
projects_clean <- projects |> mutate(municipio = str_squish(municipio))
risk_clean     <- risk     |> mutate(municipio = str_squish(municipio))

data <- projects_clean %>%
  left_join(risk_clean, by = "municipio") %>%
  filter(!is.na(municipio)) %>%
  mutate(
    codigo_dane = str_pad(as.character(codigo_dane), 5, pad = "0"),
    dpto = substr(codigo_dane, 1, 2),
    mpio = substr(codigo_dane, 3, 5)
  )

# ---- Mapa: departamentos (adm1) ----
map1 <- st_read(file.path(maps, "COL_adm1.shp"), quiet = TRUE) |> st_make_valid()

# sf departamental con geometría y código de 2 dígitos
depto_sf <- map1 %>%
  transmute(
    dpto = str_pad(as.character(ID_ESP), 2, pad = "0"),
    NAME_0, NAME_1, geometry
  )

# ---- Limpiar/estandarizar nivel_riesgo ----
data_clean <- data %>%
  mutate(
    nivel_riesgo = str_to_title(str_squish(nivel_riesgo)),
    nivel_riesgo = fct_na_value_to_level(as.factor(nivel_riesgo), "Sin dato"),
    nivel_riesgo = fct_relevel(nivel_riesgo, "Alto", "Medio", "Bajo", "Sin dato")
  )

# ---- Totales por dpto ----
totales_dpto <- data_clean %>%
  st_drop_geometry() %>%
  group_by(dpto) %>%
  summarise(
    # usa n() si cada fila ya es un proyecto
    n_proyectos    = n_distinct(no),
    total_personas = sum(as.numeric(total_personas), na.rm = TRUE),
    .groups = "drop"
  )

# ---- Proyectos por dpto y nivel de riesgo (Alto/Medio/Bajo) ----
riesgo_wide <- data_clean %>%
  st_drop_geometry() %>%
  filter(nivel_riesgo %in% c("Alto", "Medio", "Bajo")) %>%
  group_by(dpto, nivel_riesgo) %>%
  summarise(proyectos = n_distinct(no), .groups = "drop") %>%
  pivot_wider(
    names_from  = nivel_riesgo,
    values_from = proyectos,
    values_fill = 0,
    names_prefix = "proy_"  # -> proy_Alto, proy_Medio, proy_Bajo
  )

# ---- sf final por dpto (con geometría) ----
base_depto_sf <- depto_sf %>%
  left_join(totales_dpto, by = "dpto") %>%
  left_join(riesgo_wide,  by = "dpto") %>%
  arrange(desc(n_proyectos)) %>%
  relocate(NAME_1, NAME_0, dpto, n_proyectos, total_personas, proy_Alto, proy_Medio, proy_Bajo)

# ======================
# número proyectos
# ======================

base_depto_sf$n_proyectos <- as.numeric(base_depto_sf$n_proyectos)

lab_pts <- base_depto_sf %>%
  st_transform(3116) %>%              
  st_point_on_surface() %>%
  st_transform(4326) %>%
  mutate(lbl = ifelse(is.na(n_proyectos), "0",
                      formatC(n_proyectos, format = "d", big.mark = ",")))
# Mapa 1

p_nproj <- ggplot(base_depto_sf) +
  geom_sf(aes(fill = n_proyectos), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "C", end = 0.95,
                       na.value = "grey80", labels = comma,
                       name = "Proyectos (n)") +
  geom_sf_text(data = lab_pts, aes(label = lbl), size = 3) +
  theme_void(base_size = 12) +
  labs(title = "Número de proyectos por departamento")


p_nproj

ggsave(file.path(maps_out, "mapa_n_proyectos.png"),
       plot = p_nproj, width = 10, height = 8, dpi = 600, bg = "transparent")

# Mapa 2

p_npersonas <- ggplot(base_depto_sf) +
  geom_sf(aes(fill = total_personas), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "C", end = 0.95,
                       na.value = "grey80", labels = comma,
                       name = "Proyectos (n)") +
  geom_sf_text(data = lab_pts, aes(label = lbl), size = 3) +
  theme_void(base_size = 12) +
  labs(title = "Número de Personas por departamento")

p_npersonas
ggsave(file.path(maps_out, "mapa_n_personas.png"),
       plot = p_npersonas, width = 10, height = 8, dpi = 600, bg = "transparent")


# Mapa 3

p_nalto <- ggplot(base_depto_sf) +
  geom_sf(aes(fill = proy_Alto), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "C", end = 0.95,
                       na.value = "grey80", labels = comma,
                       name = "Proyectos (n)") +
  geom_sf_text(data = lab_pts, aes(label = lbl), size = 3) +
  theme_void(base_size = 12) +
  labs(title = "Riesgo Alto")

p_nalto
ggsave(file.path(maps_out, "mapa_n_riesgoalto.png"),
       plot = p_nalto, width = 10, height = 8, dpi = 600, bg = "transparent")


# Mapa 4
p_nmedio <- ggplot(base_depto_sf) +
  geom_sf(aes(fill = proy_Medio), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "C", end = 0.95,
                       na.value = "grey80", labels = comma,
                       name = "Proyectos (n)") +
  geom_sf_text(data = lab_pts, aes(label = lbl), size = 3) +
  theme_void(base_size = 12) +
  labs(title = "Riesgo Medio")

p_nmedio
ggsave(file.path(maps_out, "mapa_n_riesgomedio.png"),
       plot = p_nmedio, width = 10, height = 8, dpi = 600, bg = "transparent")


# Mapa 6
p_nbajo <- ggplot(base_depto_sf) +
  geom_sf(aes(fill = proy_Bajo), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "C", end = 0.95,
                       na.value = "grey80", labels = comma,
                       name = "Proyectos (n)") +
  geom_sf_text(data = lab_pts, aes(label = lbl), size = 3) +
  theme_void(base_size = 12) +
  labs(title = "Riesgo Bajo")

p_nbajo
ggsave(file.path(maps_out, "mapa_n_riesgobajo.png"),
       plot = p_nbajo, width = 10, height = 8, dpi = 600, bg = "transparent")

# Save Data
data_fin <- data %>%
  filter(is.na(nivel_riesgo) | nivel_riesgo != "Alto") %>%
  mutate(nivel_riesgo = fct_drop(nivel_riesgo, only = "Alto"))
save(data_fin, file = file.path(outputs, "data_mediobajo.rda"))
table(data_fin$departamento.x)
table(data_fin$municipio)

# Proyectos con mas de 20 participants 
data_final2 <- data_fin %>%
  mutate(total_personas = readr::parse_number(as.character(total_personas))) %>%
  filter(!is.na(total_personas) & total_personas >= 20)

data_final3 <- data_fin %>%
  mutate(total_personas = readr::parse_number(as.character(total_personas))) %>%
  filter(!is.na(total_personas) & total_personas >= 10)

save(data_final3 , file = file.path(outputs, "data_mediobajo_10.rda"))
table(data_final3$departamento.x)

