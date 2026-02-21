#' @title endes_estimar
#'
#' @description Estimaciones ponderadas por diseno de encuesta: media,
#'   proporcion, total o razon, con opcion de estimacion por dominio.
#'
#' @param diseno Objeto \code{survey.design2}.
#' @param formula Formula que especifica la variable de resultado
#'   (e.g., \code{~indicador}).
#' @param por Formula opcional para estimacion por dominio
#'   (e.g., \code{~V025}).
#' @param fun Caracter. Tipo de estimacion: \code{"proporcion"} (default),
#'   \code{"media"}, \code{"total"} o \code{"razon"}.
#' @param denominador Formula. Solo para \code{fun = "razon"}: formula del
#'   denominador (e.g., \code{~total_hogares}).
#' @param nivel Numerico. Nivel de confianza para intervalos (default: 0.95).
#' @param na.rm Logico. Remover NAs antes de estimar (default: TRUE).
#'
#' @return Tibble con columnas: \code{variable}, \code{estimacion}, \code{se},
#'   \code{ci_inf}, \code{ci_sup}, \code{cv}, \code{n_sin_pond}. Si se usa
#'   \code{por}, incluye columna(s) de dominio.
#'
#' @examples
#' \dontrun{
#' diseno <- endes_diseno_encuesta(datos, unidad = "mujer")
#' endes_estimar(diseno, ~indicador, fun = "proporcion")
#' endes_estimar(diseno, ~indicador, por = ~V025, fun = "proporcion")
#' }
#'
#' @importFrom survey svymean svytotal svyratio svyby SE
#' @importFrom tibble tibble as_tibble
#' @importFrom stats confint as.formula coef qnorm
#' @importFrom rlang abort
#'
#' @export endes_estimar

endes_estimar <- function(diseno,
                           formula,
                           por = NULL,
                           fun = "proporcion",
                           denominador = NULL,
                           nivel = 0.95,
                           na.rm = TRUE) {

  if (!inherits(diseno, "survey.design")) {
    rlang::abort(
      "El argumento 'diseno' debe ser un objeto survey.design."
    )
  }

  fun <- tolower(fun)
  funs_validas <- c("media", "proporcion", "total", "razon")
  if (!fun %in% funs_validas) {
    rlang::abort(paste0(
      "Funcion '", fun, "' no reconocida. Use: ",
      paste(funs_validas, collapse = ", "), "."
    ))
  }

  if (fun == "razon" && is.null(denominador)) {
    rlang::abort("Para fun = 'razon', debe especificar el argumento 'denominador'.")
  }

  # Seleccionar funcion de estimacion
  fun_survey <- switch(fun,
    media      = survey::svymean,
    proporcion = survey::svymean,
    total      = survey::svytotal,
    razon      = NULL
  )

  # --- Estimacion por dominio ---
  if (!is.null(por)) {
    if (fun == "razon") {
      rlang::abort("Estimacion por dominio no soportada para fun = 'razon'. Use endes_subpoblacion().")
    }

    resultado <- survey::svyby(
      formula  = formula,
      by       = por,
      design   = diseno,
      FUN      = fun_survey,
      na.rm    = na.rm,
      vartype  = c("se", "ci"),
      level    = nivel
    )

    res_df <- as.data.frame(resultado)

    # Identificar columnas de dominio vs resultados
    var_nombre <- all.vars(formula)
    por_nombres <- all.vars(por)

    # Construir tibble estandarizado
    cols_dominio <- res_df[, por_nombres, drop = FALSE]
    estimacion <- res_df[[var_nombre]]
    # svyby puede nombrar la col SE como "se" o "se.<var>"
    se_col <- if ("se" %in% names(res_df)) "se" else paste0("se.", var_nombre)
    se_val <- res_df[[se_col]]

    # CI puede tener distintos nombres segun version de survey
    ci_inf_col <- grep("^ci_l", names(res_df), value = TRUE)
    ci_sup_col <- grep("^ci_u", names(res_df), value = TRUE)

    ci_inf <- if (length(ci_inf_col) > 0) res_df[[ci_inf_col[1]]] else estimacion - stats::qnorm(1 - (1 - nivel) / 2) * se_val
    ci_sup <- if (length(ci_sup_col) > 0) res_df[[ci_sup_col[1]]] else estimacion + stats::qnorm(1 - (1 - nivel) / 2) * se_val

    cv <- ifelse(estimacion != 0, se_val / estimacion, NA_real_)

    # Conteo no ponderado por dominio
    datos_dom <- diseno$variables
    datos_dom[["__por__"]] <- interaction(datos_dom[, por_nombres, drop = FALSE])
    var_resp <- datos_dom[[var_nombre]]
    n_sin_pond <- tapply(
      !is.na(var_resp),
      datos_dom[["__por__"]],
      sum
    )
    n_sin_pond <- as.numeric(n_sin_pond)

    resultado_tbl <- cbind(
      cols_dominio,
      tibble::tibble(
        variable   = var_nombre,
        estimacion = estimacion,
        se         = se_val,
        ci_inf     = ci_inf,
        ci_sup     = ci_sup,
        cv         = cv,
        n_sin_pond = n_sin_pond
      )
    )

    return(tibble::as_tibble(resultado_tbl))
  }

  # --- Estimacion global ---
  if (fun == "razon") {
    resultado <- survey::svyratio(
      numerator   = formula,
      denominator = denominador,
      design      = diseno,
      na.rm       = na.rm
    )
    estimacion <- as.numeric(coef(resultado))
    se_val <- as.numeric(survey::SE(resultado))
    ci <- as.data.frame(confint(resultado, level = nivel))
    ci_inf <- ci[, 1]
    ci_sup <- ci[, 2]
    var_nombre <- paste0(all.vars(formula), "/", all.vars(denominador))
  } else {
    resultado <- fun_survey(formula, design = diseno, na.rm = na.rm)
    estimacion <- as.numeric(coef(resultado))
    se_val <- as.numeric(survey::SE(resultado))
    ci <- as.data.frame(confint(resultado, level = nivel))
    ci_inf <- ci[, 1]
    ci_sup <- ci[, 2]
    var_nombre <- all.vars(formula)
  }

  cv <- ifelse(estimacion != 0, se_val / estimacion, NA_real_)

  # Conteo no ponderado
  var_resp <- diseno$variables[[all.vars(formula)[1]]]
  n_sin_pond <- sum(!is.na(var_resp))

  tibble::tibble(
    variable   = var_nombre,
    estimacion = estimacion,
    se         = se_val,
    ci_inf     = ci_inf,
    ci_sup     = ci_sup,
    cv         = cv,
    n_sin_pond = n_sin_pond
  )
}
