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
  writexl,
  knitr,
  kableExtra,
  RColorBrewer
)

# ---- Include maps ----
source("code/aux_maps.R")

# ---- Paths ----
inputs   <- "data/inputs"
outputs  <- "data/derived"
maps     <- "data/inputs/Colombia_Division"
maps_out <- "img"
excel_path <- file.path(outputs, "matched_projects_Data3.xlsx")

# ---- Cargar etiquetas  ----
labels_path <- file.path("code/aux_labels.R")
source(labels_path)

get_label <- function(var) {
    if (exists("var_labels")) {
        return(var_labels[[var]])
    } else {
        return(var)
    }
}

# ---- Open / create the database  ----
matched <- read_excel(excel_path, sheet = "matched_all")

matched <- matched %>%
        mutate(
        treated = ifelse(treated == 1, "Treated", "Control")) %>%
        select(codigo_dane,departamento.x,municipio,nivel_riesgo, treated,numero_de_mujeres_organizacion_pct:victimas_pct)

# Guardar el resultado en Excel con la fecha de hoy
output_excel <- file.path(outputs, paste0("Escuelas_Cuidado_2025-09-10.xlsx"))
writexl::write_xlsx(matched, output_excel)

# ---- Table - Summary Statistics  ----

# Excluir 'codigo_dane' de la tabla de resumen
numeric_vars <- matched %>% select(where(is.numeric)) %>% colnames()


table_list <- lapply(numeric_vars, function(var) {
    treated_stats <- matched %>% filter(treated == "Treated") %>% summarise(
        n = sum(!is.na(.data[[var]])),
        mean = round(mean(.data[[var]], na.rm = TRUE), 3),
        sd = round(sd(.data[[var]], na.rm = TRUE), 3)
    )
    control_stats <- matched %>% filter(treated == "Control") %>% summarise(
        n = sum(!is.na(.data[[var]])),
        mean = round(mean(.data[[var]], na.rm = TRUE), 3),
        sd = round(sd(.data[[var]], na.rm = TRUE), 3)
    )
    data.frame(
        #Variable = var,
        Label = get_label(var),
        Treated_n = treated_stats$n,
        Treated_mean = treated_stats$mean,
        Treated_sd = treated_stats$sd,
        Control_n = control_stats$n,
        Control_mean = control_stats$mean,
        Control_sd = control_stats$sd
    )
})

summary_table_long <- do.call(rbind, table_list)

# Convertir a formato markdown
md_table <- kable(summary_table_long, format = "markdown", digits = 2, caption = "Summary statistics by group (Treated vs Control)")

# Exportar la tabla al archivo Sampling.md
sampling_md_path <- file.path("Sampling.md")
cat("\n## Summary statistics by group (Treated vs Control)\n", file = sampling_md_path, append = TRUE)
cat(md_table, file = sampling_md_path, append = TRUE)
        
    
# ---- MAPS ----

# (base_depto_sf = viene de code/aux_maps.R)
matched_map <- matched %>% 
            transmute(codigo_dane=as.numeric(codigo_dane),
                    treated)

mpio_end <- mpio_sf %>%
  left_join(matched_map, by = "codigo_dane")

# Mapa profesional de municipios por grupo de tratamiento

mpio_end$treated <- factor(mpio_end$treated, levels = c("Treated", "Control"))

mapa_mpio <- ggplot(mpio_end) +
    geom_sf(aes(fill = treated), color = "grey80", size = 0.1, alpha = ifelse(is.na(mpio_end$treated), 0.3, 1)) +
    scale_fill_manual(
        values = c("Treated" = brewer.pal(9, "Set1")[2], "Control" = brewer.pal(9, "Set1")[1]),
        name = "Group",
        labels = c("Treated", "Control"),
        na.value = "white",
        drop = TRUE
    ) +
    theme_minimal(base_size = 14) +
    labs(title = "Municipalities by Treatment Group",
             subtitle = "Rural Care Schools Experimental Sample",
             caption = "Source: Project Data & Colombia Division Maps") +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()
    )

ggsave(file.path(maps_out, "mapa_municipios_treatment_control.png"),
             plot = mapa_mpio, width = 10, height = 8, dpi = 400, bg = "white")


