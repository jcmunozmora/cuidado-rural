# Cargar librerías necesarias para la manipulación de datos
library(dplyr)
library(readxl)
library(janitor)  
library(ggalluvial)  
library(tidyverse)  


#### --- Abrir Archivos -------------------------------------------
# Cargar el dataset de Proyectos FCS
proy_fcs  <- readRDS("data/derived/01_ds_completa.rds")
nrow(proy_fcs) # 201 Proyectos (confirmar número de observaciones)



#### --- Gráfico 1 -------------------------------------------

# Contar las combinaciones de eval_encue y tcr_incluido para el diagrama alluvial
df_alluvial <- proy_fcs %>% 
    count(eval_encue, tcr_incluido)

# Calcular las cuentas por nivel de eval_encue para modificar los labels de la leyenda
legend_counts <- proy_fcs %>% 
    group_by(eval_encue) %>% 
    summarise(count = n()) %>% 
    ungroup()

# Crear un vector de etiquetas con los números incluidos
legend_labels <- setNames(paste0(legend_counts$eval_encue, " (n=", legend_counts$count, ")"),
                          legend_counts$eval_encue)

# Crear el Sankey Diagram (alluvial plot) con formato modificado: sin ejes ni fondo,
# y la leyenda de eval_encue con los números correspondientes.
ggplot(data = df_alluvial,
       aes(axis1 = eval_encue, axis2 = tcr_incluido, y = n)) +
    # Definir las posiciones de los ejes (sin etiquetas en el eje x)
    scale_x_discrete(limits = c("eval_encue", "tcr_incluido"), expand = c(.1, .05)) +
    geom_alluvium(aes(fill = eval_encue), width = 1/12, alpha = 0.8) +
    geom_stratum(width = 1/12, fill = "grey80", color = "black") +
    # Agregar etiquetas a cada stratum con su cuenta; se muestran dentro de cada bloque
    geom_text(stat = "stratum", aes(label = paste(after_stat(stratum), "\n", "n=", after_stat(count))),
              size = 3) +
    # Actualizar la escala de colores utilizando las etiquetas personalizadas para la leyenda
    scale_fill_discrete(labels = legend_labels) +
    theme_void() +
    theme(legend.title = element_blank(),  # Sin título en la leyenda
          legend.position = "bottom") -> p

# Guardar el gráfico en formato PNG en la carpeta "img"
ggsave("img/00_diagrama_evaluacion.png", plot = p, width = 8, height = 6, dpi = 300)