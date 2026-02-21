#' @title endes_dominio_directo
#'
#' @description Estimacion directa por dominio con varianza directa. Los
#'   resultados sirven como input para modelos de area pequena (SAE).
#'
#' @param diseno Objeto \code{survey.design2}.
#' @param resultado Formula de la variable resultado (e.g., \code{~indicador}).
#' @param dominio Formula del dominio de estimacion (e.g., \code{~V024}).
#'
#' @return Tibble con columnas: nombre del dominio, \code{estimacion},
#'   \code{se}, \code{ci_inf}, \code{ci_sup}, \code{vardir}, \code{cv},
#'   \code{n_sin_pond}.
#'
#' @details
#' Calcula estimaciones directas ponderadas usando \code{survey::svyby()}.
#' La varianza directa (\code{vardir = se^2}) es el input estandar para
#' modelos Fay-Herriot de area pequena.
#'
#' @examples
#' \dontrun{
#' diseno <- endes_diseno_encuesta(datos, unidad = "mujer")
#' endes_dominio_directo(diseno, ~indicador, ~V024)
#' }
#'
#' @importFrom survey svyby svymean
#' @importFrom tibble tibble as_tibble
#' @importFrom stats qnorm
#' @importFrom rlang abort
#'
#' @export endes_dominio_directo

endes_dominio_directo <- function(diseno, resultado, dominio) {

  if (!inherits(diseno, "survey.design")) {
    rlang::abort(
      "El argumento 'diseno' debe ser un objeto survey.design."
    )
  }

  # Estimacion por dominio
  res <- survey::svyby(
    formula = resultado,
    by      = dominio,
    design  = diseno,
    FUN     = survey::svymean,
    na.rm   = TRUE,
    vartype = c("se", "ci")
  )

  res_df <- as.data.frame(res)

  var_nombre <- all.vars(resultado)
  dom_nombre <- all.vars(dominio)

  # Extraer columnas
  dominio_vals <- res_df[[dom_nombre]]
  estimacion   <- res_df[[var_nombre]]
  # svyby puede nombrar la col SE como "se" o "se.<var>"
  se_col <- if ("se" %in% names(res_df)) "se" else paste0("se.", var_nombre)
  se_val <- res_df[[se_col]]

  # CI
  ci_inf_col <- grep("^ci_l", names(res_df), value = TRUE)
  ci_sup_col <- grep("^ci_u", names(res_df), value = TRUE)
  ci_inf <- if (length(ci_inf_col) > 0) res_df[[ci_inf_col[1]]] else estimacion - stats::qnorm(0.975) * se_val
  ci_sup <- if (length(ci_sup_col) > 0) res_df[[ci_sup_col[1]]] else estimacion + stats::qnorm(0.975) * se_val

  # Varianza directa y CV
  vardir <- se_val^2
  cv     <- ifelse(estimacion != 0, se_val / estimacion, NA_real_)

  # Conteo no ponderado por dominio
  datos_dom <- diseno$variables
  var_resp <- datos_dom[[var_nombre]]
  dom_vals <- datos_dom[[dom_nombre]]
  n_sin_pond <- tapply(!is.na(var_resp), dom_vals, sum)
  n_sin_pond <- as.numeric(n_sin_pond[as.character(dominio_vals)])

  resultado_tbl <- tibble::tibble(
    estimacion = estimacion,
    se         = se_val,
    ci_inf     = ci_inf,
    ci_sup     = ci_sup,
    vardir     = vardir,
    cv         = cv,
    n_sin_pond = n_sin_pond
  )

  # Agregar columna de dominio al inicio
  resultado_tbl[[dom_nombre]] <- dominio_vals
  resultado_tbl <- resultado_tbl[, c(dom_nombre, "estimacion", "se", "ci_inf",
                                      "ci_sup", "vardir", "cv", "n_sin_pond")]

  tibble::as_tibble(resultado_tbl)
}
