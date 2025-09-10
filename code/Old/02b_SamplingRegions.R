# =========================================================
# RANDOM SEARCH UNTIL QUOTAS MATCH EXACTLY (by depto_sel)
# =========================================================
rm(list = ls())
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)

set.seed(123)

# --- 0) Cargar y filtrar elegibles (ajusta a tu ruta) ---
projects <- readRDS("C:/Users/user/OneDrive - Universidad EAFIT/VP - 2025_Intervencion_Cuidado_IDB/cuidado-rural/data/derived/01_final_data.rds")

# Quita no viables
projects_2 <- projects %>% filter(viable_2 != "No viable")

# Anula CAUCA y GUAJIRA en ambas columnas
projects_3 <- projects_2 %>%
  mutate(across(c(Departamento_1, Departamento_2),
                ~ ifelse(str_to_upper(str_squish(as.character(.))) %in% c("CAUCA","GUAJIRA"),
                         NA_character_, .)))

# Un único departamento por proyecto (si hay 2, elige uno al azar)
set.seed(2025)
projects_4 <- projects_3 %>%
  rowwise() %>%
  mutate(
    depto_sel = {
      x <- c(Departamento_1, Departamento_2)
      x <- x[!is.na(x) & x != ""]
      if (length(x) == 0) NA_character_ else sample(x, 1)
    }
  ) %>%
  ungroup() %>%
  filter(!is.na(depto_sel))

# --- 1) Definir N y cuotas por proporción (sin forzar mínimo 1) ---
N <- 25

tab_avail <- projects_4 %>% count(depto_sel, name = "n_avail")
cuotas <- tab_avail %>%
  mutate(prop = n_avail / sum(n_avail),
         n_sample = round(prop * N))

# Ajusta para que sumen N exactamente (restos mayores)
diffN <- N - sum(cuotas$n_sample)
if (diffN != 0) {
  # prioridad según resto (prop*N - floor/round)
  target_real <- cuotas$prop * N
  resto <- target_real - cuotas$n_sample
  ord <- order(resto, decreasing = (diffN > 0))
  for (i in seq_len(abs(diffN))) {
    cuotas$n_sample[ord[i]] <- cuotas$n_sample[ord[i]] + sign(diffN)
  }
}

# Chequeo de factibilidad: ninguna cuota puede exceder n_avail
overfull <- cuotas %>% filter(n_sample > n_avail)
if (nrow(overfull) > 0) {
  stop(
    paste0(
      "Infeasible quotas: ",
      paste0(overfull$depto_sel, " (quota=", overfull$n_sample, ", avail=", overfull$n_avail, ")", collapse = "; "),
      ". Reduce N o ajusta cuotas."
    )
  )
}

# --- 2) Validador de cuotas EXACTAS 
qualifies <- function(sampled_df, cuotas_tbl, N) {
  counts <- sampled_df %>% count(depto_sel, name = "n")
  comparison <- cuotas_tbl %>%
    left_join(counts, by = "depto_sel") %>%
    mutate(n = tidyr::replace_na(n, 0L),
           match = n == n_sample)
  all(comparison$match) && nrow(sampled_df) == N
}

# --- 3) Búsqueda por semillas (aleatoria) ---
max_iterations <- 10000
best_sample <- NULL
best_seed <- NA

eligible_pool <- projects_4 %>% select(id_proyecto, depto_sel, everything())

for (seed in 1:max_iterations) {
  set.seed(seed)
  candidate <- eligible_pool %>% slice_sample(n = N)
  if (qualifies(candidate, cuotas, N)) {
    best_sample <- candidate
    best_seed <- seed
    cat("Found valid sample at seed:", seed, "\n")
    break
  }
}

if (!is.null(best_sample)) {
  write_xlsx(best_sample, "sample_projects_by_depto_RANDOM.xlsx")
  cat("Saved sample with seed:", best_seed, "\n")
} else {
  cat("No valid sample found after", max_iterations, "iterations.\n")
}
