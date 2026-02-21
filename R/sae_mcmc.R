#' @title endes_sfh_prevalencia
#'
#' @description Implementacion formal del modelo Spatial Fay-Herriot (SFH) para estimar
#' prevalencias (tasas) en areas pequenas (distritos/provincias) mitigando la "Ceguera Espacial"
#' o subcobertura de la ENDES.
#'
#' @param estimacion_directa Vector numerico. Tasa cruda o asintotica de la encuesta.
#' @param vardir Vector numerico. Varianza asintotica del estimador directo.
#' @param matriz_adyacencia Matriz W estandarizada por filas (Row-Standardized proximity matrix)
#' obtenida desde \code{endes_etl_matriz_adyacencia()}.
#' @param covariables Data.frame opcional con covariables de efectos fijos para el modelo mixto.
#' Si es NULL, se asume un modelo Intercept Only (\code{~ 1}).
#' @param method Metodo de estimacion de varianzas de los efectos aleatorios (REML, ML). Default: "REML".
#' @param n_efectivo Vector numerico opcional. Tamano muestral efectivo por area.
#'   Si se provee, se aplica la transformacion Freeman-Tukey Arcsine para estabilizar
#'   varianzas: \eqn{z = \arcsin(\sqrt{p})}, \eqn{D_i = 1/(4 n_{eff,i})}.
#' @param maxiter Entero positivo. Maximo de iteraciones internas para \code{sae::eblupSFH()}
#'   y \code{sae::mseSFH()}. Default: 500.
#' @param precision Numerico positivo. Tolerancia de convergencia interna para \code{sae}.
#'   Default: 1e-4.
#'
#' @return Un objeto data.frame conteniendo las prevlencias suavizadas (EBLUP_Espacial),
#' la varianza del EBLUP, y el diagnostico del parametro autoregresivo rho.
#'
#' @importFrom sae eblupSFH
#' @export
endes_sfh_prevalencia <- function(estimacion_directa, vardir, matriz_adyacencia, covariables = NULL,
                                   method = "REML", n_efectivo = NULL,
                                   maxiter = 500L, precision = 1e-4) {
    if (!requireNamespace("sae", quietly = TRUE)) {
        stop("El paquete 'sae' es necesario para los modelos espaciales (SFH).")
    }

    n_dominios <- length(estimacion_directa)
    if (!is.null(n_efectivo) && length(n_efectivo) != n_dominios) {
        stop("'n_efectivo' debe tener la misma longitud que 'estimacion_directa'.")
    }
    if (!is.numeric(maxiter) || length(maxiter) != 1 || is.na(maxiter) || maxiter < 1) {
        stop("'maxiter' debe ser un entero positivo.")
    }
    if (!is.numeric(precision) || length(precision) != 1 || is.na(precision) || precision <= 0) {
        stop("'precision' debe ser un numerico positivo.")
    }
    maxiter <- as.integer(maxiter)
    precision <- as.numeric(precision)
    matriz_adyacencia <- as.matrix(matriz_adyacencia)
    storage.mode(matriz_adyacencia) <- "double"

    # Validacion topologica matricial
    if (nrow(matriz_adyacencia) != n_dominios || ncol(matriz_adyacencia) != n_dominios) {
        stop("La dimension de la matriz de adyacencia W debe cuadrar exactamente con los inputs m.")
    }
    if (any(!is.finite(matriz_adyacencia))) {
        stop("La matriz de adyacencia contiene valores no finitos.")
    }

    # Transformacion Arcoseno de Freeman-Tukey para Estabilizacion de Varianza
    # (Soluciona las varianzas degeneradas empiricas en clusters n=1 o p=0,1)
    if (!is.null(n_efectivo)) {
        message("--- Aplicando Transformacion Arcoseno (Freeman-Tukey) ---")
        # Prevenimos acin(sqrt(y)) errors bounding a [0, 1]
        y_bound <- pmin(pmax(as.numeric(estimacion_directa), 0), 1)
        z_transformado <- asin(sqrt(y_bound))
        # Varianza estabilizada asintotica independiente de 'p'
        var_transformada <- 1 / (4 * as.numeric(n_efectivo))
    } else {
        z_transformado <- as.numeric(estimacion_directa)
        var_transformada <- as.numeric(vardir)
    }
    var_directa <- as.numeric(var_transformada)
    if (any(!is.finite(var_directa)) || any(var_directa <= 0)) {
        stop("La varianza directa (o transformada) debe ser finita y positiva en todas las areas.")
    }

    # Estructuracion de la Base de Datos Transitoria
    # Forzamos as.numeric() para destruir metadata o dimensiones residuales del objeto svyby
    if (is.null(covariables)) {
        df_modelo <- data.frame(
            y = z_transformado,
            var_directa = var_directa
        )
        formula_modelo <- y ~ 1
    } else {
        covariables <- as.data.frame(covariables, stringsAsFactors = FALSE)
        # Si hay covariables auxiliares (Ej. Pobreza, Luces nocturnas)
        df_modelo <- cbind(data.frame(y = z_transformado, var_directa = var_directa), covariables)
        # Nombres de todas las variables covariantes
        nombres_cov <- setdiff(names(covariables), c("UBIGEO", "DISTRITO"))
        if (length(nombres_cov) == 0) {
            formula_modelo <- y ~ 1
        } else {
            formula_modelo <- as.formula(paste("y ~", paste(nombres_cov, collapse = " + ")))
        }
    }

    traducir_error_sae <- function(e, etapa) {
        msg <- conditionMessage(e)
        if (grepl("comparison \\(>=\\) is possible only for atomic and list types", msg)) {
            stop(
                sprintf(
                    "%s fallo por no convergencia al alcanzar MAXITER=%d en 'sae' (bug conocido de la libreria).",
                    etapa, maxiter
                ),
                call. = FALSE
            )
        }
        stop(sprintf("%s fallo: %s", etapa, msg), call. = FALSE)
    }

    # Ejecucion pura del Spatial Shrinkage
    message("Convergiendo Spatial Fay-Herriot (SFH)...")
    modelo_sfh <- tryCatch(
        sae::eblupSFH(
            formula = formula_modelo,
            vardir = var_directa,
            proxmat = matriz_adyacencia,
            method = method,
            MAXITER = maxiter,
            PRECISION = precision,
            data = df_modelo
        ),
        error = function(e) traducir_error_sae(e, "eblupSFH")
    )

    message("Calculando Mean Squared Error (MSE) de las predicciones espaciales...")
    modelo_mse <- tryCatch(
        sae::mseSFH(
            formula = formula_modelo,
            vardir = var_directa,
            proxmat = matriz_adyacencia,
            method = method,
            MAXITER = maxiter,
            PRECISION = precision,
            data = df_modelo
        ),
        error = function(e) traducir_error_sae(e, "mseSFH")
    )

    # Back-transformation (Si se aplico Freeman-Tukey Arcoseno)
    if (!is.null(n_efectivo)) {
        message("--- Retro-transformando Predicciones EBLUP a proporciones originales ---")
        eblup_final <- sin(as.numeric(modelo_sfh$eblup))^2
        # Aproximacion Delta Method simple para la varianza o MSE
        mse_final <- as.numeric(modelo_mse$mse) * (2 * sin(as.numeric(modelo_sfh$eblup)) * cos(as.numeric(modelo_sfh$eblup)))^2
    } else {
        eblup_final <- as.numeric(modelo_sfh$eblup)
        mse_final <- as.numeric(modelo_mse$mse)
    }

    # Resultados Formateados rigurosamente coercidos para evitar 0-dimensions
    resultados <- data.frame(
        estimacion_directa = as.numeric(estimacion_directa),
        vardir = as.numeric(vardir),
        eblup_espacial = eblup_final,
        mse_eblup = mse_final
    )

    attr(resultados, "sfh_fit") <- modelo_sfh$fit

    return(resultados)
}

#' @title endes_bootstrap_ic
#'
#' @description Implementacion computacional intensiva de remuestreo (Bootstrapping) para encuestas
#' complejas (Rao-Wu-Yue), evadiendo la linearizacion de Taylor para los ICs. Ideal para estadisticos
#' medianos o percentiles donde la asintotica falla.
#'
#' @param diseno Objeto \code{survey.design2}.
#' @param rep Entero. Numero de redisenios bootstrap a simular. Default: 500.
#'
#' @return Un objeto \code{svyrep.design} del tipo bootstrap.
#'
#' @export
endes_bootstrap_ic <- function(diseno, rep = 500) {
    # Utilizamos la transmutacion de la matriz de disenio a Replicate Weights
    # Se respeta el remuestreo de PSUs dentro de  estratos de forma auto-contenida.

    if (!inherits(diseno, "survey.design")) {
        stop("Input debe ser un survey.design valido.")
    }

    diseno_boot <- survey::as.svrepdesign(diseno, type = "bootstrap", replicates = rep)
    return(diseno_boot)
}

#' @title endes_sae_mcmc
#'
#' @description Convolucion MCMC bivariada para propagar la incertidumbre conjunta
#' de la prevalencia (estimador EBLUP o directo) y el denominador poblacional (INEI)
#' hacia una estimacion absoluta de Carga de Enfermedad (YLD).
#'
#' @details
#' El modelo jerarquico bivariado se define como:
#' \deqn{y_i | \theta_i \sim N(\theta_i, mse_i)}{y_i | theta_i ~ N(theta_i, mse_i)}
#' \deqn{P_i \sim N(\hat{P}_i, \sigma^2_{pop_i})}{P_i ~ N(P_hat_i, sigma2_pop_i)}
#' \deqn{YLD_i = \theta_i \times P_i \times DW_c}{YLD_i = theta_i * P_i * DW_c}
#'
#' Donde \code{mse_i} es el MSE del EBLUP espacial (o la varianza directa),
#' \code{sigma2_pop_i} es la varianza de la proyeccion poblacional INEI
#' (no publicada; debe ser estimada o asumida con transparencia),
#' y \code{DW_c} es el Disability Weight de la condicion (GBD/WHO).
#'
#' Al muestrear \code{n_draws} realizaciones independientes de cada distribucion
#' y multiplicarlas, se obtiene la distribucion posterior de YLDs cuya varianza
#' refleja la Incertidumbre Total Integrada (encuesta + demografia).
#'
#' @param prevalencia Vector numerico. Prevalencia estimada (EBLUP o directa) por area.
#' @param mse_prevalencia Vector numerico. MSE o varianza del estimador de prevalencia.
#' @param poblacion Vector numerico. Poblacion proyectada por area (\eqn{\hat{P}_i}).
#' @param var_poblacion Vector numerico. Varianza de la proyeccion poblacional.
#'   Si es un escalar, se recicla para todas las areas.
#'   Si es NULL, se asume \code{(0.05 * poblacion)^2} (5\% CV) con advertencia.
#' @param dw Numerico escalar. Disability Weight de la condicion (ej. 0.004 para anemia leve).
#' @param n_draws Entero. Numero de realizaciones MCMC. Default: 1000.
#' @param seed Entero opcional. Semilla para reproducibilidad.
#'
#' @return Un data.frame con columnas:
#'   \code{prevalencia}, \code{poblacion}, \code{yld_media} (media posterior),
#'   \code{yld_mediana}, \code{yld_q025} (percentil 2.5\%), \code{yld_q975} (percentil 97.5\%),
#'   \code{yld_sd} (desviacion estandar posterior).
#'
#' @examples
#' \dontrun{
#' # Despues de correr endes_sfh_prevalencia():
#' resultado_yld <- endes_sae_mcmc(
#'   prevalencia = res_sfh$eblup_espacial,
#'   mse_prevalencia = res_sfh$mse_eblup,
#'   poblacion = pop_inei$poblacion_0_5,
#'   var_poblacion = (0.05 * pop_inei$poblacion_0_5)^2,
#'   dw = 0.004,
#'   n_draws = 5000,
#'   seed = 42
#' )
#' }
#'
#' @export
endes_sae_mcmc <- function(prevalencia, mse_prevalencia, poblacion, var_poblacion = NULL,
                            dw, n_draws = 1000L, seed = NULL) {

    # ---- Validacion de inputs ----
    n_areas <- length(prevalencia)
    if (length(mse_prevalencia) != n_areas) {
        stop("'mse_prevalencia' debe tener la misma longitud que 'prevalencia'.")
    }
    if (length(poblacion) != n_areas) {
        stop("'poblacion' debe tener la misma longitud que 'prevalencia'.")
    }
    if (!is.numeric(dw) || length(dw) != 1 || dw < 0 || dw > 1) {
        stop("'dw' debe ser un escalar numerico en [0, 1].")
    }
    if (n_draws < 100) {
        stop("'n_draws' debe ser >= 100 para estabilidad de los percentiles.")
    }

    # Manejo de var_poblacion
    if (is.null(var_poblacion)) {
        warning(
            "var_poblacion no especificada. Asumiendo CV=5% (sigma = 0.05*P). ",
            "Esto es un supuesto NO verificado. Ver Seccion 5.1 del documento metodologico."
        )
        var_poblacion <- (0.05 * poblacion)^2
    }
    if (length(var_poblacion) == 1) {
        var_poblacion <- rep(var_poblacion, n_areas)
    }
    if (length(var_poblacion) != n_areas) {
        stop("'var_poblacion' debe ser escalar o tener longitud igual a 'prevalencia'.")
    }

    # Proteccion contra varianzas negativas o cero
    mse_prevalencia <- pmax(as.numeric(mse_prevalencia), 1e-10)
    var_poblacion <- pmax(as.numeric(var_poblacion), 1e-10)

    if (!is.null(seed)) set.seed(seed)

    # ---- Muestreo MCMC (Monte Carlo directo) ----
    message(sprintf("Muestreando %d realizaciones MCMC para %d areas...", n_draws, n_areas))

    # Matrices de draws: filas = draws, columnas = areas
    draws_prev <- matrix(
        stats::rnorm(n_draws * n_areas,
                     mean = rep(as.numeric(prevalencia), each = n_draws),
                     sd = rep(sqrt(mse_prevalencia), each = n_draws)),
        nrow = n_draws, ncol = n_areas
    )
    # Truncar prevalencias a [0, 1]
    draws_prev <- pmin(pmax(draws_prev, 0), 1)

    draws_pop <- matrix(
        stats::rnorm(n_draws * n_areas,
                     mean = rep(as.numeric(poblacion), each = n_draws),
                     sd = rep(sqrt(var_poblacion), each = n_draws)),
        nrow = n_draws, ncol = n_areas
    )
    # Truncar poblacion a >= 0
    draws_pop <- pmax(draws_pop, 0)

    # ---- Convolucion: YLD = Prevalencia * Poblacion * DW ----
    draws_yld <- draws_prev * draws_pop * dw

    # ---- Resumen posterior por area ----
    yld_media   <- colMeans(draws_yld)
    yld_mediana <- apply(draws_yld, 2, stats::median)
    yld_q025    <- apply(draws_yld, 2, stats::quantile, probs = 0.025)
    yld_q975    <- apply(draws_yld, 2, stats::quantile, probs = 0.975)
    yld_sd      <- apply(draws_yld, 2, stats::sd)

    resultado <- data.frame(
        prevalencia = as.numeric(prevalencia),
        poblacion = as.numeric(poblacion),
        yld_media = yld_media,
        yld_mediana = yld_mediana,
        yld_q025 = yld_q025,
        yld_q975 = yld_q975,
        yld_sd = yld_sd
    )

    attr(resultado, "n_draws") <- n_draws
    attr(resultado, "dw") <- dw
    attr(resultado, "draws_yld") <- draws_yld

    message("Convolucion MCMC completada.")
    return(resultado)
}

#' @title endes_sfh_estratificado
#'
#' @description Ajusta modelos Spatial Fay-Herriot separados por estratos de pobreza,
#' replicando la metodologia de Cerda-Hernandez & Sikov (2024). Al estratificar,
#' se reduce la heterogeneidad inter-estrato que infla el parametro autorregresivo
#' rho en un modelo global.
#'
#' @details
#' El paper de referencia (Salud Publica Mex, 2024;66:236-244) estratifica los
#' 1874 distritos en 3 niveles de pobreza monetaria: <30\%, 30-55\%, >55\%.
#' Para cada estrato se ajusta un modelo SFH independiente con su propia matriz
#' de adyacencia W (reconstruida sobre el subgrafo del estrato) y su propio
#' conjunto de covariables significativas.
#'
#' La funcion construye la topologia kNN internamente por estrato usando
#' los centroides del shapefile filtrado.
#'
#' @param estimacion_directa Vector numerico. Estimaciones directas por distrito.
#'   Puede contener NA para distritos no muestreados (prediccion out-of-sample).
#' @param vardir Vector numerico. Varianzas del estimador directo. NA para no muestreados.
#' @param ubigeo Vector character. Codigos UBIGEO de 6 digitos.
#' @param pobreza Vector numerico. Tasa de pobreza monetaria [0,1] por distrito.
#' @param shp Objeto sf. Shapefile distrital con columna UBIGEO.
#' @param k_vecinos Entero. Numero de vecinos para kNN. Default: 4.
#' @param covariables Data.frame con columna UBIGEO y covariables numericas.
#' @param cortes_pobreza Vector numerico de longitud 2. Cortes para estratificacion.
#'   Default: c(0.30, 0.55) produce 3 estratos: <30\%, 30-55\%, >55\%.
#' @param method Metodo de estimacion (REML, ML). Default: "REML".
#' @param n_efectivo Vector numerico opcional. Si se provee, aplica Freeman-Tukey.
#' @param maxiter_sfh Entero positivo. Maximo de iteraciones para cada ajuste SFH
#'   dentro de cada estrato. Default: 500.
#' @param precision_sfh Numerico positivo. Tolerancia de convergencia para cada ajuste
#'   SFH por estrato. Default: 1e-4.
#'
#' @return Un data.frame con columnas: UBIGEO, estrato, estimacion_directa, vardir,
#'   eblup_espacial, mse_eblup. Atributo "sfh_fits" con los ajustes por estrato.
#'
#' @export
endes_sfh_estratificado <- function(estimacion_directa, vardir, ubigeo, pobreza,
                                     shp, k_vecinos = 4L, covariables = NULL,
                                     cortes_pobreza = c(0.30, 0.55),
                                     method = "REML", n_efectivo = NULL,
                                     maxiter_sfh = 500L, precision_sfh = 1e-4) {
    if (!requireNamespace("sae", quietly = TRUE)) {
        stop("El paquete 'sae' es necesario.")
    }
    if (!requireNamespace("sf", quietly = TRUE) || !requireNamespace("spdep", quietly = TRUE)) {
        stop("Los paquetes 'sf' y 'spdep' son necesarios para la topologia espacial.")
    }

    n <- length(estimacion_directa)
    if (length(vardir) != n || length(ubigeo) != n || length(pobreza) != n) {
        stop("Todos los vectores de input deben tener la misma longitud.")
    }
    if (!is.null(n_efectivo) && length(n_efectivo) != n) {
        stop("'n_efectivo' debe tener la misma longitud que 'estimacion_directa'.")
    }
    if (length(cortes_pobreza) != 2 || cortes_pobreza[1] >= cortes_pobreza[2]) {
        stop("'cortes_pobreza' debe ser un vector de longitud 2 con cortes crecientes.")
    }
    if (!is.numeric(maxiter_sfh) || length(maxiter_sfh) != 1 || is.na(maxiter_sfh) || maxiter_sfh < 1) {
        stop("'maxiter_sfh' debe ser un entero positivo.")
    }
    if (!is.numeric(precision_sfh) || length(precision_sfh) != 1 || is.na(precision_sfh) || precision_sfh <= 0) {
        stop("'precision_sfh' debe ser un numerico positivo.")
    }
    maxiter_sfh <- as.integer(maxiter_sfh)
    precision_sfh <- as.numeric(precision_sfh)

    # Asignar estratos
    estrato <- rep(NA_character_, n)
    estrato[pobreza < cortes_pobreza[1]] <- "bajo"
    estrato[pobreza >= cortes_pobreza[1] & pobreza <= cortes_pobreza[2]] <- "medio"
    estrato[pobreza > cortes_pobreza[2]] <- "alto"

    etiquetas <- c("bajo" = sprintf("<%.0f%%", 100 * cortes_pobreza[1]),
                   "medio" = sprintf("%.0f-%.0f%%", 100 * cortes_pobreza[1], 100 * cortes_pobreza[2]),
                   "alto" = sprintf(">%.0f%%", 100 * cortes_pobreza[2]))

    message(sprintf("Estratificacion por pobreza: %s", paste(
        sprintf("%s: %d distritos", etiquetas, table(estrato)[c("bajo", "medio", "alto")]),
        collapse = ", "
    )))

    # Preparar contenedores
    resultados_lista <- vector("list", 3)
    sfh_fits <- list()
    nombres_estratos <- c("bajo", "medio", "alto")

    for (s in nombres_estratos) {
        idx <- which(estrato == s)
        n_s <- length(idx)
        message(sprintf("\n--- Estrato '%s' (%s): %d distritos ---", s, etiquetas[s], n_s))

        if (n_s < 5) {
            warning(sprintf("Estrato '%s' tiene menos de 5 distritos. Se omite el modelo SFH.", s))
            resultados_lista[[s]] <- data.frame(
                UBIGEO = ubigeo[idx], estrato = s,
                estimacion_directa = estimacion_directa[idx],
                vardir = vardir[idx],
                eblup_espacial = estimacion_directa[idx],
                mse_eblup = vardir[idx],
                stringsAsFactors = FALSE
            )
            next
        }

        # Filtrar shapefile para este estrato
        shp_s <- shp[shp$UBIGEO %in% ubigeo[idx], ]
        # Reordenar para match con idx
        ord <- match(ubigeo[idx], shp_s$UBIGEO)
        if (any(is.na(ord))) {
            n_faltantes <- sum(is.na(ord))
            warning(sprintf("Estrato '%s': %d distritos sin geometria en shapefile.", s, n_faltantes))
            # Filtrar los que si tienen geometria
            tiene_geo <- !is.na(ord)
            idx <- idx[tiene_geo]
            ord <- ord[!is.na(ord)]
            n_s <- length(idx)
        }
        shp_s <- shp_s[ord, ]

        # Construir W por estrato
        centroides_s <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(shp_s)))
        k_real <- min(k_vecinos, n_s - 1)
        knn_s <- spdep::knearneigh(centroides_s, k = k_real)
        nb_s <- spdep::knn2nb(knn_s)
        nb_s <- spdep::make.sym.nb(nb_s)
        W_s <- spdep::nb2mat(nb_s, style = "W", zero.policy = TRUE)

        # Preparar covariables del estrato
        covs_s <- NULL
        if (!is.null(covariables)) {
            covs_s <- covariables[match(ubigeo[idx], covariables$UBIGEO), , drop = FALSE]
        }

        # n_efectivo del estrato
        neff_s <- if (!is.null(n_efectivo)) n_efectivo[idx] else NULL

        # Ajustar SFH con reintento numericamente estable.
        ajuste_sfh <- function(W_input, covs_input) {
            endes_sfh_prevalencia(
                estimacion_directa = estimacion_directa[idx],
                vardir = vardir[idx],
                matriz_adyacencia = W_input,
                covariables = covs_input,
                method = method,
                n_efectivo = neff_s,
                maxiter = maxiter_sfh,
                precision = precision_sfh
            )
        }

        estrategia_s <- "completo_w"
        res_s <- tryCatch(ajuste_sfh(W_s, covs_s), error = function(e) e)

        if (inherits(res_s, "error")) {
            warning(sprintf(
                "Estrato '%s': SFH fallo con W original (%s). Reintentando con W escalada para estabilidad numerica.",
                s, conditionMessage(res_s)
            ))
            estrategia_s <- "completo_w_escalada"
            res_s <- tryCatch(ajuste_sfh(W_s * 0.999, covs_s), error = function(e) e)
        }

        if (inherits(res_s, "error") && !is.null(covs_s)) {
            warning(sprintf(
                "Estrato '%s': SFH con covariables no convergio (%s). Reintentando Intercept-Only.",
                s, conditionMessage(res_s)
            ))
            estrategia_s <- "intercept_only_w_escalada"
            res_s <- tryCatch(ajuste_sfh(W_s * 0.999, NULL), error = function(e) e)
        }

        if (inherits(res_s, "error")) {
            warning(sprintf(
                "Estrato '%s': SFH no convergio (%s). Usando estimacion directa como fallback.",
                s, conditionMessage(res_s)
            ))
            sfh_fits[[s]] <- list(spatialcorr = NA_real_, fallback = "directo", estrategia = "directo")
            resultados_lista[[s]] <- data.frame(
                UBIGEO = ubigeo[idx], estrato = s,
                estimacion_directa = estimacion_directa[idx],
                vardir = vardir[idx],
                eblup_espacial = estimacion_directa[idx],
                mse_eblup = vardir[idx],
                stringsAsFactors = FALSE
            )
            next
        }

        sfh_fit_s <- attr(res_s, "sfh_fit")
        if (is.null(sfh_fit_s)) {
            sfh_fit_s <- list(spatialcorr = NA_real_)
        }
        if (is.null(sfh_fit_s$spatialcorr)) {
            sfh_fit_s$spatialcorr <- NA_real_
        }
        sfh_fit_s$estrategia <- estrategia_s
        sfh_fits[[s]] <- sfh_fit_s
        rho_s <- sfh_fit_s$spatialcorr
        rho_txt <- if (is.na(rho_s)) "NA" else sprintf("%.4f", rho_s)
        message(sprintf("rho(%s) = %s", s, rho_txt))

        resultados_lista[[s]] <- data.frame(
            UBIGEO = ubigeo[idx],
            estrato = s,
            estimacion_directa = res_s$estimacion_directa,
            vardir = res_s$vardir,
            eblup_espacial = res_s$eblup_espacial,
            mse_eblup = res_s$mse_eblup,
            stringsAsFactors = FALSE
        )
    }

    resultado_final <- do.call(rbind, resultados_lista)
    rownames(resultado_final) <- NULL
    attr(resultado_final, "sfh_fits") <- sfh_fits
    attr(resultado_final, "cortes_pobreza") <- cortes_pobreza
    attr(resultado_final, "etiquetas") <- etiquetas

    message(sprintf("\nModelo estratificado completado: %d distritos procesados.", nrow(resultado_final)))
    return(resultado_final)
}

#' @title endes_sfh_loo_cv
#'
#' @description Leave-One-Out Cross-Validation para el modelo Spatial Fay-Herriot.
#' Detecta over-smoothing evaluando la capacidad predictiva del modelo
#' al excluir cada area secuencialmente.
#'
#' @details
#' Para cada area i:
#' 1. Se excluye la observacion i del vector de estimaciones directas (se pone NA).
#' 2. Se re-ajusta el modelo SFH con las n-1 areas restantes.
#' 3. Se registra la prediccion out-of-sample para el area i.
#' 4. Se calcula el error de prediccion: e_i = y_i - yhat_i.
#'
#' Las metricas agregadas incluyen RMSE, MAE, sesgo medio, y cobertura
#' empirica del intervalo de prediccion al 95\%.
#'
#' @param estimacion_directa Vector numerico. Estimaciones directas.
#' @param vardir Vector numerico. Varianzas del estimador directo.
#' @param matriz_adyacencia Matriz W de adyacencia.
#' @param covariables Data.frame opcional de covariables.
#' @param method Metodo de estimacion. Default: "REML".
#' @param n_efectivo Vector numerico opcional para Freeman-Tukey.
#'
#' @return Una lista con:
#'   \code{predicciones} (data.frame con y_obs, y_pred, error, mse_pred),
#'   \code{metricas} (RMSE, MAE, sesgo, cobertura_95).
#'
#' @export
endes_sfh_loo_cv <- function(estimacion_directa, vardir, matriz_adyacencia,
                              covariables = NULL, method = "REML",
                              n_efectivo = NULL) {
    n <- length(estimacion_directa)
    # Solo evaluar areas con datos observados (no NA)
    idx_obs <- which(!is.na(estimacion_directa) & !is.na(vardir))
    n_obs <- length(idx_obs)

    message(sprintf("LOO-CV: evaluando %d areas observadas de %d totales.", n_obs, n))

    y_pred <- rep(NA_real_, n)
    mse_pred <- rep(NA_real_, n)
    n_convergidos <- 0
    n_fallidos <- 0

    # Varianza "infinita" para excluir un area sin usar NA
    # (sae::eblupSFH no soporta NA en vardir)
    vardir_infinita <- max(vardir[idx_obs], na.rm = TRUE) * 1e6

    for (j in seq_along(idx_obs)) {
        i <- idx_obs[j]
        if (j %% 50 == 0 || j == 1) {
            message(sprintf("  LOO iteracion %d/%d...", j, n_obs))
        }

        # Excluir area i poniendo varianza infinita (el EBLUP la ignora)
        v_loo <- vardir
        v_loo[i] <- vardir_infinita

        neff_loo <- n_efectivo
        if (!is.null(neff_loo)) {
            # Si Freeman-Tukey, la varianza transformada es 1/(4*n_eff)
            # Para "excluir" el area i, usamos n_eff minimo (=1)
            neff_loo[i] <- 1e-4  # Produce var = 1/(4*1e-4) = 2500, enorme
        }

        # Intentar ajustar modelo sin area i
        res_loo <- tryCatch({
            suppressMessages(
                endes_sfh_prevalencia(
                    estimacion_directa = estimacion_directa,
                    vardir = v_loo,
                    matriz_adyacencia = matriz_adyacencia,
                    covariables = covariables,
                    method = method,
                    n_efectivo = neff_loo
                )
            )
        }, error = function(e) NULL)

        if (!is.null(res_loo)) {
            y_pred[i] <- res_loo$eblup_espacial[i]
            mse_pred[i] <- res_loo$mse_eblup[i]
            n_convergidos <- n_convergidos + 1
        } else {
            n_fallidos <- n_fallidos + 1
        }
    }

    message(sprintf("LOO-CV: %d convergidos, %d fallidos.", n_convergidos, n_fallidos))

    # Calcular metricas sobre areas con prediccion exitosa
    idx_validos <- idx_obs[!is.na(y_pred[idx_obs])]

    if (length(idx_validos) == 0) {
        warning("Ninguna iteracion LOO convergio. Retornando metricas vacias.")
        return(list(
            predicciones = data.frame(y_obs = estimacion_directa, y_pred = y_pred,
                                       error = NA_real_, mse_pred = mse_pred),
            metricas = list(rmse = NA_real_, mae = NA_real_, sesgo = NA_real_,
                           cobertura_95 = NA_real_, n_evaluados = 0, n_fallidos = n_fallidos)
        ))
    }

    errores <- estimacion_directa[idx_validos] - y_pred[idx_validos]

    # Cobertura 95%: proporcion de areas donde |error| < 1.96 * sqrt(MSE_pred)
    se_pred <- sqrt(pmax(mse_pred[idx_validos], 0))
    dentro_ic <- abs(errores) <= 1.96 * se_pred

    metricas <- list(
        rmse = sqrt(mean(errores^2)),
        mae = mean(abs(errores)),
        sesgo = mean(errores),
        cobertura_95 = mean(dentro_ic),
        n_evaluados = length(idx_validos),
        n_fallidos = n_fallidos
    )

    predicciones <- data.frame(
        y_obs = estimacion_directa,
        y_pred = y_pred,
        error = estimacion_directa - y_pred,
        mse_pred = mse_pred
    )

    message(sprintf("Metricas LOO-CV: RMSE=%.4f, MAE=%.4f, Sesgo=%.4f, Cobertura95=%.1f%%",
        metricas$rmse, metricas$mae, metricas$sesgo, 100 * metricas$cobertura_95))

    return(list(predicciones = predicciones, metricas = metricas))
}
