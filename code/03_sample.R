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
  writexl
)

# ---- Include maps ----
source("code/aux_maps.R")

# ---- Paths ----
inputs   <- "data/inputs"
outputs  <- "data/derived"
maps     <- "data/inputs/Colombia_Division"
maps_out <- "img"

# ------- Data -------
load(file.path(outputs, "data_mediobajo_10.rda")) 
set.seed(2025)

# ----- Final Data ----
ex <- c("Amazonas", "Archipiélago de San Andrés", "La Guajira", "Nariño")

data <- data_final3 %>%
  filter(!`departamento.x` %in% ex) %>%
  filter(!is.na(`departamento.x`) & `departamento.x` != "") %>%
  distinct(no, .keep_all = TRUE)  

data_agg <- data %>% 
            group_by(dpto) %>%
            summarise(total_personas = sum(total_personas, na.rm=TRUE),
                      n_proyectos = n_distinct(no),
                      .groups = "drop")

# ---- sf final por dpto (con geometría) ----
# (base_depto_sf = viene de code/aux_maps.R)
base_depto_sf <- depto_sf %>%
  left_join(data_agg, by = "dpto") %>%
  arrange(desc(n_proyectos)) %>%
  relocate(NAME_1, NAME_0, dpto, n_proyectos, total_personas)

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

ggsave(file.path(maps_out, "mapa_n_proyectos_final.png"),
       plot = p_nproj, width = 10, height = 8, dpi = 600, bg = "transparent")


# ----- 1) Cuotas -----
N <- 25

tab_avail <- data %>%
  count(`departamento.x`, name = "n_avail", sort = TRUE) %>%
  mutate(prop = n_avail / sum(n_avail))

cuotas <- tab_avail %>%
  mutate(
    target   = prop * N,
    n_sample = floor(target),
    resto    = target - n_sample
  )

leftover <- N - sum(cuotas$n_sample)
if (leftover > 0) {
  add_idx <- order(cuotas$resto, decreasing = TRUE)[seq_len(leftover)]
  cuotas$n_sample[add_idx] <- cuotas$n_sample[add_idx] + 1L
} else if (leftover < 0) {
  sub_idx <- order(cuotas$resto, decreasing = FALSE)[seq_len(abs(leftover))]
  cuotas$n_sample[sub_idx] <- pmax(0L, cuotas$n_sample[sub_idx] - 1L)
}

# Chequeos de seguridad
stopifnot(all(cuotas$n_sample <= cuotas$n_avail))
stopifnot(sum(cuotas$n_sample) == N)

# (opcional) guarda las cuotas
write_xlsx(cuotas, file.path(outputs, "cuotas_por_departamento.xlsx"))

# ----- 2) Validador EXACTO (como el tuyo) -----
qualifies <- function(sampled_df, cuotas_tbl, N) {
  counts <- sampled_df %>% count(`departamento.x`, name = "n")
  comparison <- cuotas_tbl %>%
    left_join(counts, by = "departamento.x") %>%
    mutate(n = tidyr::replace_na(n, 0L),
           match = n == n_sample)
  all(comparison$match) && nrow(sampled_df) == N
}

# ----- 3) Búsqueda aleatoria  -----
max_iterations <- 10000
best_sample <- NULL
best_seed <- NA_integer_

eligible_pool <- data %>%
  semi_join(cuotas %>% filter(n_sample > 0), by = "departamento.x") %>%
  select(no, `departamento.x`, everything())

for (seed in 1:max_iterations) {
  set.seed(seed)
  candidate <- eligible_pool %>% slice_sample(n = N)  # muestra aleatoria global N
  if (qualifies(candidate, cuotas, N)) {
    best_sample <- candidate
    best_seed <- seed
    cat("Found valid sample at seed:", seed, "\n")
    break
  }
}

# ----- 4) Salida  -----
if (!is.null(best_sample)) {
  write_xlsx(
    list(sample = best_sample,
         cuotas  = select(cuotas, `departamento.x`, n_avail, prop, n_sample)),
    file.path(outputs, "sample_25_by_departamento_RANDOM.xlsx")
  )
  cat("Saved sample with seed:", best_seed, "\n")
} else {
  cat("No valid sample found after", max_iterations, "iterations. Usando respaldo estratificado.\n")
  best_sample <- eligible_pool %>%
    inner_join(select(cuotas, `departamento.x`, n_sample), by = "departamento.x") %>%
    group_by(`departamento.x`) %>%
    slice_sample(n = first(n_sample)) %>%
    ungroup()
  stopifnot(nrow(best_sample) == N)
  write_xlsx(
    list(sample = best_sample,
         cuotas  = select(cuotas, `departamento.x`, n_avail, prop, n_sample)),
    file.path(outputs, "sample_25_by_departamento.xlsx")
  )
}

# ----- 5) Chequeo final -----
best_sample %>%
  count(`departamento.x`, name = "n_muestra") %>%
  left_join(select(cuotas, `departamento.x`, n_sample), by = "departamento.x") %>%
  arrange(desc(n_muestra)) %>%
  print(n = Inf)

write_xlsx(best_sample, paste0(outputs, "selected_sample.xlsx"))
save(best_sample, file = file.path(outputs, "selected_sample.rda"))
