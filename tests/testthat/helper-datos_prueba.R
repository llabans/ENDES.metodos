# helper-datos_prueba.R
# Datos simulados para tests de ENDES.PE
# Simula estructura tipica de encuesta DHS con variables estandar

set.seed(42)
n <- 200

# --- Datos tipo mujer (unidad = "mujer") ---
datos_mujer <- data.frame(
  # Variables de diseno
  V021 = rep(1:20, each = 10),            # PSU (20 clusters)
  V022 = rep(rep(1:4, each = 5), 10),     # Estrato (4 estratos)
  V005 = sample(800000:1200000, n, replace = TRUE),  # Peso (escala 1e6)
  # Variables demograficas
  V012 = sample(15:49, n, replace = TRUE),  # Edad
  V025 = sample(1:2, n, replace = TRUE),    # Urbano/Rural
  V024 = sample(1:25, n, replace = TRUE),   # Departamento
  V106 = sample(0:3, n, replace = TRUE),    # Nivel educativo
  # Variable binaria para estimacion
  indicador = sample(0:1, n, replace = TRUE, prob = c(0.6, 0.4)),
  # Variable continua
  valor = rnorm(n, mean = 50, sd = 10),
  stringsAsFactors = FALSE
)

# --- Datos tipo hogar (unidad = "hogar") ---
datos_hogar <- data.frame(
  HV021 = rep(1:20, each = 10),
  HV022 = rep(rep(1:4, each = 5), 10),
  HV005 = sample(800000:1200000, n, replace = TRUE),
  HV025 = sample(1:2, n, replace = TRUE),
  HV024 = sample(1:25, n, replace = TRUE),
  indicador = sample(0:1, n, replace = TRUE, prob = c(0.5, 0.5)),
  stringsAsFactors = FALSE
)

# --- Datos tipo hombre (unidad = "hombre") ---
datos_hombre <- data.frame(
  MV021 = rep(1:20, each = 10),
  MV022 = rep(rep(1:4, each = 5), 10),
  MV005 = sample(800000:1200000, n, replace = TRUE),
  indicador = sample(0:1, n, replace = TRUE),
  stringsAsFactors = FALSE
)

# --- Datos de morbilidad infantil (modulo nino) ---
n_ninos <- 150
datos_nino <- data.frame(
  V021 = rep(1:15, each = 10),
  V022 = rep(rep(1:3, each = 5), 10),
  V005 = sample(800000:1200000, n_ninos, replace = TRUE),
  B5  = sample(c(0, 1), n_ninos, replace = TRUE, prob = c(0.05, 0.95)),
  B19 = sample(0:70, n_ninos, replace = TRUE),
  H11 = sample(c(0, 1, 2, 8), n_ninos, replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.1)),
  H22 = sample(c(0, 1, 8), n_ninos, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
  H31B = sample(c(0, 1, 8), n_ninos, replace = TRUE, prob = c(0.5, 0.4, 0.1)),
  H31C = sample(c(1, 2, 3, 8), n_ninos, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.2)),
  stringsAsFactors = FALSE
)

# --- Datos con NAs para pruebas de robustez ---
datos_con_na <- datos_mujer
datos_con_na$indicador[sample(n, 20)] <- NA
datos_con_na$valor[sample(n, 15)] <- NA
