#' @title endes_diseno_encuesta
#'
#' @description Crea un objeto survey.design a partir de datos ENDES/DHS con
#'   auto-deteccion de variables de diseno (PSU, estrato, peso) segun la unidad
#'   de analisis.
#'
#' @param datos Data frame con los datos de la encuesta.
#' @param unidad Caracter. Unidad de analisis: \code{"mujer"} (default),
#'   \code{"hogar"}, \code{"hombre"} o \code{"nino"}.
#' @param psu Caracter o NULL. Nombre de la variable PSU. Si es NULL, se
#'   detecta automaticamente segun \code{unidad}.
#' @param estrato Caracter o NULL. Nombre de la variable de estratificacion.
#'   Si es NULL, se detecta automaticamente.
#' @param peso Caracter o NULL. Nombre de la variable de ponderacion. Si es
#'   NULL, se detecta automaticamente.
#' @param escala Numerico. Factor de escala para el peso muestral (default:
#'   1e6, estandar DHS).
#'
#' @return Objeto \code{survey.design2} del paquete \pkg{survey}.
#'
#' @details
#' El mapeo automatico de variables segun unidad sigue el estandar DHS:
#' \itemize{
#'   \item \code{"mujer"}: V021 (PSU), V022 (estrato), V005 (peso)
#'   \item \code{"hogar"}: HV021 (PSU), HV022 (estrato), HV005 (peso)
#'   \item \code{"hombre"}: MV021 (PSU), MV022 (estrato), MV005 (peso)
#'   \item \code{"nino"}: V021 (PSU), V022 (estrato), V005 (peso)
#' }
#'
#' La funcion aplica \code{options(survey.lonely.psu = "adjust")} para manejar
#' PSUs solitarios (Lumley, 2004).
#'
#' @examples
#' \dontrun{
#' datos <- ENDES.PE::consulta_endes(periodo = 2022, codigo_modulo = 414,
#'                                    base = "CSALUD01", guardar = FALSE)
#' diseno <- endes_diseno_encuesta(datos, unidad = "mujer")
#' }
#'
#' @importFrom survey svydesign
#' @importFrom rlang abort
#'
#' @export endes_diseno_encuesta

endes_diseno_encuesta <- function(datos,
                                   unidad = "mujer",
                                   psu = NULL,
                                   estrato = NULL,
                                   peso = NULL,
                                   escala = 1e6) {

  # Mapeo de variables segun unidad de analisis
  mapeo <- list(
    mujer  = list(psu = "V021",  estrato = "V022",  peso = "V005"),
    hogar  = list(psu = "HV021", estrato = "HV022", peso = "HV005"),
    hombre = list(psu = "MV021", estrato = "MV022", peso = "MV005"),
    nino   = list(psu = "V021",  estrato = "V022",  peso = "V005")
  )

  unidad <- tolower(unidad)
  if (!unidad %in% names(mapeo)) {
    rlang::abort(paste0(
      "Unidad '", unidad, "' no reconocida. ",
      "Use: 'mujer', 'hogar', 'hombre' o 'nino'."
    ))
  }

  # Usar mapeo automatico si el usuario no especifica
  psu_var     <- psu     %||% mapeo[[unidad]]$psu
  estrato_var <- estrato %||% mapeo[[unidad]]$estrato
  peso_var    <- peso    %||% mapeo[[unidad]]$peso

  # Validar que las columnas existan
  vars_requeridas <- c(psu_var, estrato_var, peso_var)
  vars_faltantes <- vars_requeridas[!vars_requeridas %in% names(datos)]
  if (length(vars_faltantes) > 0) {
    rlang::abort(paste0(
      "Columna(s) no encontrada(s) en los datos: ",
      paste(vars_faltantes, collapse = ", "), ".\n",
      "Verifique los nombres o especifique manualmente con psu/estrato/peso."
    ))
  }

  # Escalar peso
  datos[["__peso_escalado__"]] <- datos[[peso_var]] / escala

  # Configurar manejo de PSUs solitarios
  options(survey.lonely.psu = "adjust")

  # Construir formulas
  f_psu     <- stats::as.formula(paste0("~", psu_var))
  f_estrato <- stats::as.formula(paste0("~", estrato_var))

  # Crear diseno
  survey::svydesign(
    ids     = f_psu,
    strata  = f_estrato,
    weights = ~`__peso_escalado__`,
    data    = datos,
    nest    = TRUE
  )
}
