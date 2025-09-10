# ===============================================
# CONTINUOUS OUTCOME - MINIMUM DETECTABLE EFFECT
# Clustered Design with adjustments for:
#   - Covariates (R2)
#   - Unequal cluster sizes (CV)
# ===============================================

rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)

# PARAMETERS -------------------------------------------------------------
alpha     <- 0.05                                   # Significance level
power_tar <- 0.80                                   # Statistical power
icc_grid  <- c(0.01, 0.03, 0.05, 0.10, 0.20, 0.30)  # Possible ICC values
k_grid    <- seq(6, 60, by = 2)         # Total clusters (treatment + control)
m_grid    <- c(20, 25)                  # Interviews per cluster
R2        <- 0.30           # Variance reduction from covariates (ANCOVA)
CV        <- 0.20           # Coefficient of variation for unequal cluster sizes
sigma_y   <- NA_real_       # SD of the outcome (fill if you want MDE in original units)

# FUNCTIONS --------------------------------------------------------------

# MDE for continuous outcome in SD units
mde_cont_clu_cv <- function(k, mbar, icc, CV = 0, alpha = 0.05, power = 0.80, R2 = 0){
  z_a <- qnorm(1 - alpha/2)    # z-value for alpha
  z_b <- qnorm(power)          # z-value for power
  deff <- 1 + (mbar - 1)*icc*(1 + CV^2)  # design effect with unequal sizes
  (z_a + z_b) * sqrt( 2 * deff * (1 - R2) / (k * mbar) )  # returns MDE in SD units
}

# Calculate k given a target MDE
k_needed <- function(MDE_target, m, icc, alpha = 0.05, power = 0.80, R2 = 0, CV = 0){
  z_a <- qnorm(1 - alpha/2)
  z_b <- qnorm(power)
  deff <- 1 + (m - 1)*icc*(1 + CV^2)
  k_raw <- 2 * deff * (1 - R2) * (z_a + z_b)^2 / (m * MDE_target^2)
  2 * ceiling(k_raw/2)  # round up to nearest even number
}

# CREATE SCENARIO TABLE --------------------------------------------------
tabla_mde <- expand_grid(k = k_grid, m = m_grid, icc = icc_grid) |>
  rowwise() |>
  mutate(
    MDE_SD   = mde_cont_clu_cv(k, m, icc, CV, alpha, power_tar, R2),
    n_total  = k * m,
    MDE_unit = if (is.na(sigma_y)) NA_real_ else MDE_SD * sigma_y
  ) |>
  ungroup() |>
  arrange(MDE_SD)


# EXPORT -----------------------------------------------------------------
write_xlsx(tabla_mde, "size_continuous.xlsx")

# GRAPH ------------------------------------------------------------------
icc_star   <- 0.30
mde_target <- 0.30  # in SD

tabla_aux <- tabla_mde |>
  filter(m == 20, icc == icc_star)

ggplot(tabla_aux, aes(x = n_total, y = MDE_SD)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = mde_target, linetype = 2) +
  labs(x = "Total number of surveys (k*m)",
       y = "MDE (SD units)") +
  theme_minimal()

ggsave("MDEvsSamplingSize_continuous.png", width = 8, height = 6, dpi = 300)

# EXAMPLE USAGE ----------------------------------------------------------
# How many clusters needed for MDE = 0.30 SD, m = 20, ICC = 0.10?
k_needed(MDE_target = 0.30, m = 20, icc = 0.10, R2 = R2, CV = CV)




