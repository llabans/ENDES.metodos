#' @title endes_subpoblacion
#'
#' @description Subconjunto seguro de un diseno de encuesta. Utiliza
#'   \code{survey::subset()} para mantener la estructura completa del diseno,
#'   lo cual es esencial para la correcta estimacion de varianzas.
#'
#' @param diseno Objeto \code{survey.design2} creado con
#'   \code{\link{endes_diseno_encuesta}} o \code{survey::svydesign}.
#' @param condicion Expresion logica no evaluada (non-standard evaluation) que
#'   define el subconjunto. Se evalua en el contexto de los datos del diseno.
#'
#' @return Objeto \code{survey.design2} subconjunto.
#'
#' @details
#' Nunca filtre filas de un data frame antes de crear el diseno de encuesta.
#' Use esta funcion despues de crear el diseno para obtener estimaciones
#' correctas de varianza en subpoblaciones (Lumley, 2004).
#'
#' @examples
#' \dontrun{
#' diseno <- endes_diseno_encuesta(datos, unidad = "mujer")
#' diseno_rural <- endes_subpoblacion(diseno, V025 == 2)
#' }
#'
#' @importFrom rlang enquo eval_tidy abort
#'
#' @export endes_subpoblacion

endes_subpoblacion <- function(diseno, condicion) {

  if (!inherits(diseno, "survey.design")) {
    rlang::abort(
      "El argumento 'diseno' debe ser un objeto survey.design. Use endes_diseno_encuesta() primero."
    )
  }

  condicion_quo <- rlang::enquo(condicion)
  condicion_logica <- rlang::eval_tidy(condicion_quo, data = diseno$variables)

  if (!is.logical(condicion_logica)) {
    rlang::abort("La condicion debe evaluar a un vector logico.")
  }

  subset(diseno, condicion_logica)
}
