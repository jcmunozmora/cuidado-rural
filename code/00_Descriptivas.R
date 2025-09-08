rm(list = ls())

# ---- Packages ----
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(scales)
library(readr)

# ---- Paths ----
inputs  <- "C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/data/inputs"
outputs <- "C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/data/derived"
file_xlsx <- file.path(inputs, "FOMMUR_LINEA1.xlsx")

# ---- Read data ----
projects <- read_excel(file_xlsx, sheet = "Hoja1") |>
  clean_names() 
# ---- Locale for numbers ----
# Locale para números
loc  <- readr::locale(decimal_mark = ".", grouping_mark = ",")
cols <- names(projects)[4:33]

projects <- projects |>
  dplyr::mutate(across(
    all_of(cols),
    ~ if (is.numeric(.x)) .x else suppressWarnings(readr::parse_number(as.character(.x), locale = loc))
  ))

# ---- % ----
num_cols  <- c("numero_de_mujeres_organizacion", "mujeres_transgenero",
               "otro_genero", "no_informa_genero","orientacion_sexual_hetero", "orientacion_gay",
               "orientacion_bisexual", "orientacion_otro","orientacion_no_informa","personas_entre_los_18_28_anos",
               "personas_mayores_de_60_anos", "indigenas", "rrom", "raizales", "palenqueras", "negras_afrocolombianas",
               "sin_reconocimiento_etnico", "size_etnia_total", "personas_sisben_a", "personas_sisben_b", "personas_sisben_c",
               "persona_sisben_d", "personas_no_sisbenizadas", "no_informan_sisben", "sisben_total", 
               "personas_reincroporadas", "personas_lideres", "victimas")  
denom_col <- "total_personas"  
safe_pct <- function(num, den) ifelse(is.na(den) | den == 0, NA_real_, 100 * num / den)
projects <- projects |>
  dplyr::mutate(across(all_of(num_cols),
                       ~ round(safe_pct(.x, .data[[denom_col]]), 2),
                       .names = "{.col}_pct"))
# SAVE DATA
save(projects, file = file.path(outputs, "data_percent.rda"))
# GRAPHS
  projects <- projects |>
    mutate(
      sector_de_la_actividad_productiva      = fct_explicit_na(sector_de_la_actividad_productiva, "Sin sector"),
      departamento = fct_explicit_na(departamento, "Sin departamento"),
      municipio    = fct_explicit_na(municipio, "Sin municipio")
    )

  # ========================
  # Nº de proyectos por sector
  # ========================

  proy_x_sector <- projects |>
    count(sector_de_la_actividad_productiva, name = "n") |>
    arrange(desc(n))
  
  ggplot(proy_x_sector, aes(x = reorder(sector_de_la_actividad_productiva, n), y = n, fill = sector_de_la_actividad_productiva)) +
    geom_col() +
    coord_flip() +
    scale_fill_viridis_d(option = "C", direction = -1, end = 0.95, guide = "none") +
    labs(title = "Número de proyectos por sector", x = "Sector", y = "Proyectos (n)") +
    theme_minimal(base_size = 12)
  ggsave(file.path(outputs, "proyectos_por_sector.png"), width = 10, height = 7, dpi = 300)

  # ========================
  # Municipio
  # ========================
  
  proy_x_municipio <- projects |>
    count(municipio, name = "n") |>
    arrange(desc(n))
  
  ggplot(proy_x_municipio, aes(x = reorder(municipio, n), y = n, fill = municipio)) +
    geom_col() +
    coord_flip() +
    scale_fill_viridis_d(option = "C", direction = -1, end = 0.95, guide = "none") +
    labs(title = "Número de proyectos por Municipio", x = "Sector", y = "Proyectos (n)") +
    theme_minimal(base_size = 12)
  ggsave(file.path(outputs, "municipios.png"), width = 10, height = 7, dpi = 300)
  
  # ========================
  # Departamento
  # ========================
  
  proy_x_departamento <- projects |>
    count(departamento, name = "n") |>
    arrange(desc(n))
  
  ggplot(proy_x_departamento, aes(x = reorder(departamento, n), y = n, fill = departamento)) +
    geom_col() +
    coord_flip() +
    scale_fill_viridis_d(option = "C", direction = -1, end = 0.95, guide = "none") +
    labs(title = "Número de proyectos por Departamento", x = "Sector", y = "Proyectos (n)") +
    theme_minimal(base_size = 12)
  ggsave(file.path(outputs, "departamento.png"), width = 10, height = 7, dpi = 300)
  
  
  
  # ========================
  # Tamaño de los proyectos
  # ========================
  breaks <- c(-Inf, 0, 5, 10, 20, 50, Inf)
  labels <- c("0", "1–5", "6–10", "11–20", "21–50", "51+")
  proy_x_tamano <- projects |>
    mutate(total_cat = cut(total_personas, breaks = breaks, labels = labels, right = TRUE, include.lowest = TRUE)) |>
    count(total_cat, name = "n") |>
    arrange(desc(n))
  
  ggplot(proy_x_tamano, aes(x = reorder(total_cat, n), y = n, fill = total_cat)) +
    geom_col() +
    coord_flip() +
    scale_fill_viridis_d(option = "C", direction = -1, end = 0.95, guide = "none") +
    labs(title = "Tamaño de los proyectos (rangos)", x = "Rango de total de personas", y = "Proyectos (n)") +
    theme_minimal(base_size = 12)
    ggsave(file.path(outputs, "proyectos_por_tamaño.png"), width = 10, height = 7, dpi = 300)
  
    # ========================
    # Sexo
    # ========================
    cat_cols <- c("numero_de_mujeres_organizacion",
                  "mujeres_transgenero",
                  "otro_genero",
                  "no_informa_genero")
    
    umbral <- 1  
    
    df_bar <- projects |>
      summarise(across(all_of(cat_cols), ~ sum(coalesce(.x, 0) >= umbral))) |>
      pivot_longer(everything(), names_to = "categoria", values_to = "proyectos") |>
      mutate(
        categoria = dplyr::recode(categoria,
                                  numero_de_mujeres_organizacion = "Mujeres organización",
                                  mujeres_transgenero             = "Mujeres transgénero",
                                  otro_genero                     = "Otro género",
                                  no_informa_genero               = "No informa género"
        )
      )
    df_bar
    
    # 2) Gráfica de barras
    ggplot(df_bar, aes(x = reorder(categoria, proyectos), y = proyectos, fill = categoria)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_d(option = "C", direction = -1, end = 0.95, guide = "none") +
      labs(title = "Proyectos con ≥1 por  género",
           x = "Categoría", y = "Proyectos (n)") +
      theme_minimal(base_size = 12)
    
    ggsave(file.path(outputs, "proyectos_por_genero.png"), width = 10, height = 7, dpi = 300)
    
    # ========================
    # Orientacion sexual
    # ========================
    cat_cols <- c("orientacion_sexual_hetero",
                  "orientacion_gay",
                  "orientacion_bisexual",
                  "orientacion_otro",
                  "orientacion_no_informa")
    
    umbral <- 1  
    
    df_bar <- projects |>
      summarise(across(all_of(cat_cols), ~ sum(coalesce(.x, 0) >= umbral))) |>
      pivot_longer(everything(), names_to = "categoria", values_to = "proyectos") |>
      mutate(
        categoria = dplyr::recode(categoria,
                                  orientacion_sexual_hetero = "Hetero",
                                  orientacion_gay           = "Gay",
                                  orientacion_bisexual      = "Bisexual",
                                  orientacion_otro          = "Otros",
                                  orientacion_no_informa    = "No Informa"
        )
      )
    df_bar
    
    # 2) Gráfica de barras
    ggplot(df_bar, aes(x = reorder(categoria, proyectos), y = proyectos, fill = categoria)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_d(option = "C", direction = -1, end = 0.95, guide = "none") +
      labs(title = "Proyectos con Orientacion Sexual",
           x = "Categoría", y = "Proyectos (n)") +
      theme_minimal(base_size = 12)
    
    ggsave(file.path(outputs, "sexual.png"), width = 10, height = 7, dpi = 300)

    # ========================
    # Etnia
    # ========================
    cat_cols <- c("indigenas",
                  "rrom",
                  "raizales",
                  "palenqueras",
                  "negras_afrocolombianas",
                  "sin_reconocimiento_etnico")
    
    df_bar <- projects |>
      summarise(across(all_of(cat_cols), ~ sum(coalesce(.x, 0) >= umbral))) |>
      pivot_longer(everything(), names_to = "categoria", values_to = "proyectos") |>
      mutate(
        categoria = dplyr::recode(categoria,
                                  indigenas = "Indigenas",
                                  rrom          = "rrom",
                                  raizales      = "Raizales",
                                  palenqueras          = "Palenqueras",
                                  negras_afrocolombianas    = "Negras Afrocolombianas",
                                  sin_reconocimiento_etnico = "Sin Reconocimiento Etnico"
        )
      )
    df_bar
    
    # 2) Gráfica de barras
    ggplot(df_bar, aes(x = reorder(categoria, proyectos), y = proyectos, fill = categoria)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_d(option = "C", direction = -1, end = 0.95, guide = "none") +
      labs(title = "Proyectos Segun Reconocimiento Etnico",
           x = "Categoría", y = "Proyectos (n)") +
      theme_minimal(base_size = 12)
    
    ggsave(file.path(outputs, "etnia.png"), width = 10, height = 7, dpi = 300)
    
    # ========================
    # Sisben
    # ========================
    cat_cols <- c("personas_sisben_a",
                  "personas_sisben_b",
                  "personas_sisben_c",
                  "persona_sisben_d")
    
    df_bar <- projects |>
      summarise(across(all_of(cat_cols), ~ sum(coalesce(.x, 0) >= umbral))) |>
      pivot_longer(everything(), names_to = "categoria", values_to = "proyectos") |>
      mutate(
        categoria = dplyr::recode(categoria,
                                  personas_sisben_a = "Sisben A",
                                  personas_sisben_b = "Sisben B",
                                  personas_sisben_c = "Sisben C",
                                  persona_sisben_d  = "Sisben D")
                                  )
    df_bar
    
    # 2) Gráfica de barras
    ggplot(df_bar, aes(x = reorder(categoria, proyectos), y = proyectos, fill = categoria)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_d(option = "C", direction = -1, end = 0.95, guide = "none") +
      labs(title = "Sisben",
           x = "Categoría", y = "Proyectos (n)") +
      theme_minimal(base_size = 12)
    
    ggsave(file.path(outputs, "sisben.png"), width = 10, height = 7, dpi = 300)

    # ========================
    # Sisben
    # ========================
    cat_cols <- c("personas_sisben_a",
                  "personas_sisben_b",
                  "personas_sisben_c",
                  "persona_sisben_d")
    
    df_bar <- projects |>
      summarise(across(all_of(cat_cols), ~ sum(coalesce(.x, 0) >= umbral))) |>
      pivot_longer(everything(), names_to = "categoria", values_to = "proyectos") |>
      mutate(
        categoria = dplyr::recode(categoria,
                                  personas_sisben_a = "Sisben A",
                                  personas_sisben_b = "Sisben B",
                                  personas_sisben_c = "Sisben C",
                                  persona_sisben_d  = "Sisben D")
      )
    df_bar
    
    # 2) Gráfica de barras
    ggplot(df_bar, aes(x = reorder(categoria, proyectos), y = proyectos, fill = categoria)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_d(option = "C", direction = -1, end = 0.95, guide = "none") +
      labs(title = "Sisben",
           x = "Categoría", y = "Proyectos (n)") +
      theme_minimal(base_size = 12)
    
    ggsave(file.path(outputs, "sisben.png"), width = 10, height = 7, dpi = 300)
    
    # ========================
    # Sisben
    # ========================
    cat_cols <- c("personas_reincroporadas",
                  "personas_lideres",
                  "victimas")
    
    df_bar <- projects |>
      summarise(across(all_of(cat_cols), ~ sum(coalesce(.x, 0) >= umbral))) |>
      pivot_longer(everything(), names_to = "categoria", values_to = "proyectos") |>
      mutate(
        categoria = dplyr::recode(categoria,
                                  personas_reincroporadas = "Personas Reincroporadas",
                                  personas_lideres = "Lideres",
                                  victimas = "Victimas")
      )
    df_bar
    
    # 2) Gráfica de barras
    ggplot(df_bar, aes(x = reorder(categoria, proyectos), y = proyectos, fill = categoria)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_d(option = "C", direction = -1, end = 0.95, guide = "none") +
      labs(title = "Población Vulnerable",
           x = "Categoría", y = "Proyectos (n)") +
      theme_minimal(base_size = 12)
    
    ggsave(file.path(outputs, "victimas.png"), width = 10, height = 7, dpi = 300)
    
    
    
    