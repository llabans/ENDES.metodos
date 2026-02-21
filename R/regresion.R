#' @title endes_glm_encuesta
#'
#' @description Regresion GLM ponderada por diseno de encuesta con salida
#'   estandarizada (tibble). Soporta exponenciacion para Odds Ratios o Risk
#'   Ratios.
#'
#' @param diseno Objeto \code{survey.design2}.
#' @param formula Formula del modelo (e.g., \code{indicador ~ V012 + V025}).
#' @param familia Familia de distribucion. Default:
#'   \code{stats::quasibinomial()}.
#' @param exponenciar Logico. Si TRUE (default), exponencia los coeficientes
#'   (e.g., OR para logistica).
#' @param nivel Numerico. Nivel de confianza (default: 0.95).
#'
#' @return Tibble con columnas: \code{termino}, \code{estimacion}, \code{se},
#'   \code{ci_inf}, \code{ci_sup}, \code{estadistico}, \code{p_valor}.
#'
#' @details
#' Utiliza \code{survey::svyglm()} para ajustar el modelo y
#' \code{broom::tidy()} para estandarizar la salida. Se recomienda usar
#' \code{quasibinomial()} en lugar de \code{binomial()} para datos de
#' encuesta (Lumley, 2004).
#'
#' @examples
#' \dontrun{
#' diseno <- endes_diseno_encuesta(datos, unidad = "mujer")
#' endes_glm_encuesta(diseno, indicador ~ V012 + V025)
#' }
#'
#' @importFrom survey svyglm
#' @importFrom broom tidy
#' @importFrom stats quasibinomial
#' @importFrom rlang abort
#'
#' @export endes_glm_encuesta

endes_glm_encuesta <- function(diseno,
                                formula,
                                familia = stats::quasibinomial(),
                                exponenciar = TRUE,
                                nivel = 0.95) {

  if (!inherits(diseno, "survey.design")) {
    rlang::abort(
      "El argumento 'diseno' debe ser un objeto survey.design."
    )
  }

  if (!inherits(formula, "formula")) {
    rlang::abort("El argumento 'formula' debe ser una formula (e.g., y ~ x1 + x2).")
  }

  modelo <- survey::svyglm(formula, design = diseno, family = familia)

  resultado <- broom::tidy(
    modelo,
    conf.int   = TRUE,
    conf.level = nivel,
    exponentiate = exponenciar
  )

  # Renombrar columnas al espanol
  nombres_esp <- c(
    term      = "termino",
    estimate  = "estimacion",
    std.error = "se",
    statistic = "estadistico",
    p.value   = "p_valor",
    conf.low  = "ci_inf",
    conf.high = "ci_sup"
  )

  nombres_actuales <- names(resultado)
  nombres_nuevos <- ifelse(
    nombres_actuales %in% names(nombres_esp),
    nombres_esp[nombres_actuales],
    nombres_actuales
  )
  names(resultado) <- nombres_nuevos

  # Reordenar columnas
  cols_orden <- c("termino", "estimacion", "se", "ci_inf", "ci_sup",
                  "estadistico", "p_valor")
  cols_presentes <- cols_orden[cols_orden %in% names(resultado)]
  resultado[, cols_presentes]
}
