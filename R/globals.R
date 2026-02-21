# Declaracion de variables globales para evitar NOTEs en R CMD check.
# Estas variables son utilizadas en contexto NSE (Non-Standard Evaluation)
# por dplyr y sae::eblupSFH.
utils::globalVariables(c(
  ".", "%>%",
  "UBIGEO", "Pobreza_Total", "Poblacion",
  "Agua_Red_Publica", "Ruralidad",
  "var_directa", "estrato",
  "eblup_espacial", "n_dist", "n_grave",
  "pct_grave", "prev_media"
))
