#' @title endes_vars_morbilidad_infantil
#'
#' @description Genera indicadores estandar DHS de morbilidad infantil:
#'   enfermedad diarreica aguda (EDA), infeccion respiratoria aguda (IRA) y
#'   fiebre en las ultimas 2 semanas.
#'
#' @param datos Data frame con variables del modulo de salud infantil ENDES.
#'
#' @return Data frame original con columnas adicionales:
#'   \code{eda_2sem} (logico, diarrea ultimas 2 semanas),
#'   \code{ira_2sem} (logico, IRA ultimas 2 semanas),
#'   \code{fiebre_2sem} (logico, fiebre ultimas 2 semanas).
#'   Todos definidos sobre el universo de ninos vivos menores de 5 anios.
#'
#' @details
#' El universo de analisis es: ninos vivos (B5==1) menores de 60 meses
#' (B19<60). Los casos fuera de este universo reciben NA.
#'
#' Variables fuente requeridas:
#' \itemize{
#'   \item B5: Nino actualmente vivo (1=si, 0=no)
#'   \item B19: Edad actual en meses
#'   \item H11: Diarrea ultimas 2 semanas (1=si, ultimo 24h; 2=si, ultimo 2 sem)
#'   \item H22: Fiebre ultimas 2 semanas (1=si)
#'   \item H31B: Sintomas respiratorios - dificultad respiratoria (1=si)
#'   \item H31C: Causa de dificultad - problema en pecho (1=solo pecho,
#'     3=ambos)
#' }
#'
#' @examples
#' \dontrun{
#' datos_nino <- ENDES.PE::consulta_endes(
#'   periodo = 2022, codigo_modulo = 69,
#'   base = "REC43", guardar = FALSE
#' )
#' datos_morb <- endes_vars_morbilidad_infantil(datos_nino)
#' }
#'
#' @importFrom rlang abort
#'
#' @export endes_vars_morbilidad_infantil

endes_vars_morbilidad_infantil <- function(datos) {
  # Variables minimas requeridas
  vars_requeridas <- c("B5", "B19")
  vars_faltantes <- vars_requeridas[!vars_requeridas %in% names(datos)]
  if (length(vars_faltantes) > 0) {
    rlang::abort(paste0(
      "Columna(s) requerida(s) no encontrada(s): ",
      paste(vars_faltantes, collapse = ", "), ".\n",
      "Se requieren al menos B5 y B19 para definir el universo de analisis."
    ))
  }

  # Universo: ninos vivos menores de 60 meses
  universo <- datos[["B5"]] == 1 & datos[["B19"]] < 60

  # EDA - Enfermedad Diarreica Aguda
  if (all(c("H11") %in% names(datos))) {
    datos[["eda_2sem"]] <- ifelse(
      universo,
      datos[["H11"]] %in% c(1, 2),
      NA
    )
  }

  # IRA - Infeccion Respiratoria Aguda
  if (all(c("H31B", "H31C") %in% names(datos))) {
    datos[["ira_2sem"]] <- ifelse(
      universo,
      datos[["H31B"]] == 1 & datos[["H31C"]] %in% c(1, 3),
      NA
    )
  }

  # Fiebre
  if ("H22" %in% names(datos)) {
    datos[["fiebre_2sem"]] <- ifelse(
      universo,
      datos[["H22"]] == 1,
      NA
    )
  }

  datos
}

#' @title endes_vars_anemia
#'
#' @description Genera el indicador de anemia en ninos y mujeres segun
#'   los recortes estandar de la OMS/DHS.
#'
#' @param datos Data frame con variables del modulo de biomarcadores (REC43 o REC91).
#' @param poblacion Caracter. Tipo de poblacion: \code{"nino"} o \code{"mujer"}.
#'
#' @return Data frame original con la columna adicional:
#'   \code{anemia} (logico, presencia de anemia [leve, moderada o severa]).
#'
#' @details
#' Para ninos (6 a 59 meses):
#' \itemize{
#'   \item HW57: Nivel de hemoglobina ajustado por altitud.
#'   \item Cut-off: < 11.0 g/dL (o < 110 g/L).
#' }
#'
#' Para mujeres (15 a 49 anios):
#' \itemize{
#'   \item V456: Nivel de hemoglobina ajustado por altitud (o HA57 para hogar).
#'   \item Cut-off: < 12.0 g/dL para no embarazadas, < 11.0 g/dL para embarazadas (V213).
#' }
#'
#' Los valores faltantes o negaciones (e.g. 999) se codifican como NA.
#'
#' @examples
#' \dontrun{
#' datos_nino <- ENDES.PE::consulta_endes(
#'   periodo = 2022, codigo_modulo = 74,
#'   base = "REC43", guardar = FALSE
#' )
#' datos_anemia <- endes_vars_anemia(datos_nino, poblacion = "nino")
#' }
#'
#' @importFrom rlang abort
#'
#' @export endes_vars_anemia
#'
endes_vars_anemia <- function(datos, poblacion = "nino") {
  poblacion <- tolower(poblacion)
  if (!poblacion %in% c("nino", "mujer")) {
    rlang::abort("Poblacion no reconocida. Use: 'nino' o 'mujer'.")
  }

  if (poblacion == "nino") {
    vars_requeridas <- c("HW57")
    vars_faltantes <- vars_requeridas[!vars_requeridas %in% names(datos)]
    if (length(vars_faltantes) > 0) {
      rlang::abort("Falta variable HW57 (Hemoglobina ajustada en ninos).")
    }

    # HW57 viene en g/L (o un decimal corrido). DHS estandar: < 110 es anemia.
    # Excluimos valores especiales como 999.
    datos[["anemia"]] <- ifelse(
      datos[["HW57"]] < 900, # Filtro de valores especiales
      datos[["HW57"]] < 110,
      NA
    )
  }

  if (poblacion == "mujer") {
    # Suponiendo V456 como hemoglobina ajustada y V213 como estado de embarazo
    vars_requeridas <- c("V456", "V213")
    vars_faltantes <- vars_requeridas[!vars_requeridas %in% names(datos)]
    if (length(vars_faltantes) > 0) {
      rlang::abort("Falta variable V456 (Hemoglobina) o V213 (Embarazo).")
    }

    # Cutoffs OMS: Embarazada (<110), No embarazada (<120)
    datos[["anemia"]] <- ifelse(
      datos[["V456"]] < 900,
      ifelse(datos[["V213"]] == 1, datos[["V456"]] < 110, datos[["V456"]] < 120),
      NA
    )
  }

  datos
}
