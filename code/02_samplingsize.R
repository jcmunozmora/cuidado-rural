# ===============================================
# MDE CONTINUO (Cluster RCT) con ajustes por:
#  - Covariables (R2)
#  - Tamaños desiguales de clúster (CV)
#  - Asignación tratamiento-control (phi)
# ===============================================
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


# ------------------ PARÁMETROS ------------------
alpha     <- 0.05                       # Nivel de significancia (bilateral)
power_tar <- 0.80                       # Potencia estadística
icc_grid  <- c(0.01, 0.03, 0.05, 0.10, 0.20, 0.30, 0.40)   # ICC
k_grid    <- seq(6, 60, by = 2)         # k = clústeres totales (T + C) -> pares si phi=0.5
m_grid    <- c(10, 15,20, 25)           # Entrevistas promedio por clúster
R2        <- 0.30                       # Reducción de varianza por covariables (ANCOVA)
CV        <- 0.20                       # Coef. de variación (desigualdad de tamaños de clúster)
phi       <- 0.50                       # Proporción de clústeres en tratamiento (0.5 = balanceado)
sigma_y   <- NA_real_                   # SD del outcome (si la pones, obtienes MDE en unidades)

# ------------------ FUNCIONES -------------------
# Efecto de diseño con tamaños desiguales (aprox. estándar)
deff_clu <- function(mbar, icc, CV = 0) 1 + (mbar - 1) * icc * (1 + CV^2)

# MDE continuo en **SD** (k = clústeres totales, phi = asignación)
mde_cont_clu_cv <- function(k, mbar, icc, CV = 0, alpha = 0.05, power = 0.80, R2 = 0, phi = 0.5){
  z_a  <- qnorm(1 - alpha/2)
  z_b  <- qnorm(power)
  deff <- deff_clu(mbar, icc, CV)
  alloc_factor <- 1 / (phi * (1 - phi))      # = 4 si phi = 0.5
  (z_a + z_b) * sqrt( alloc_factor * deff * (1 - R2) / (k * mbar) )
}

# Clústeres necesarios (k total) para un MDE objetivo en **SD**
k_needed <- function(MDE_target, m, icc, alpha = 0.05, power = 0.80, R2 = 0, CV = 0, phi = 0.5){
  z_a  <- qnorm(1 - alpha/2)
  z_b  <- qnorm(power)
  deff <- deff_clu(m, icc, CV)
  alloc_factor <- 1 / (phi * (1 - phi))
  k_raw <- alloc_factor * deff * (1 - R2) * (z_a + z_b)^2 / (m * MDE_target^2)
  if (abs(phi - 0.5) < 1e-8) 2 * ceiling(k_raw/2) else ceiling(k_raw)  # par si balanceado
}

# ------------ TABLA DE ESCENARIOS ---------------
tabla_mde <- expand_grid(k = k_grid, m = m_grid, icc = icc_grid) |>
  mutate(
    MDE_SD   = mde_cont_clu_cv(k, m, icc, CV = CV, alpha = alpha, power = power_tar, R2 = R2, phi = phi),
    n_total  = k * m,
    MDE_unit = if (is.na(sigma_y)) NA_real_ else MDE_SD * sigma_y
  ) |>
  arrange(MDE_SD)

# ------------ EXPORTAR TABLA --------------------
write_xlsx(tabla_mde, "size_continuous.xlsx")

# ------------- GRÁFICO DE LÍNEA -----------------
icc_star   <- 0.03  # elige el ICC a mostrar
mde_target <- 0.20   # MDE objetivo en SD (línea de referencia)

tabla_aux <- tabla_mde |>
  filter( m == 20, icc == icc_star)

p <- ggplot(tabla_aux, aes(x = n_total, y = MDE_SD)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = mde_target, linetype = 2) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(title = paste0("MDE (SD) vs tamaño muestral  |  ICC = ", icc_star, ", m = 20"),
       x = "Encuestas totales (k * m)",
       y = "MDE (unidades SD)") +
  theme_minimal(base_size = 12)

print(p)
ggsave(file.path(maps_out, "MDEvsSamplingSize_continuous.png"),
       plot = p, width = 8, height = 6, dpi = 300)
# ¿Cuántos clústeres totales (k) necesito para MDE = 0.30 SD, m = 20, ICC = 0.10?
k_needed(MDE_target = 0.20, m = 20, icc = 0.10, R2 = R2, CV = CV, phi = phi)
