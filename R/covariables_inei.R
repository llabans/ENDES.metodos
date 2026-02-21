#' ETL para Covariables Censales (INEI)
#'
#' Descarga, parsea y une los datasets abiertos del INEI a nivel distrital:
#' Mapa de Pobreza (2018), CPV 2017 Agua por Red Publica y Ruralidad.
#'
#' @details
#' Extrae indicadores estructurales (\eqn{X\beta}) indispensables para asentar
#' el Modelo Fay-Herriot Espacial en estadistica de Salud Publica.
#'
#' Las tres covariables retornadas son proporciones [0, 1]:
#' \itemize{
#'   \item \code{Pobreza_Total}: Tasa de pobreza monetaria total (Mapa de Pobreza INEI 2018).
#'   \item \code{Agua_Red_Publica}: Proporcion de viviendas con agua por red publica (CPV 2017).
#'   \item \code{Ruralidad}: Proporcion de poblacion rural sobre total censado (CPV 2017).
#' }
#'
#' @return Un data.frame con columnas: UBIGEO (chr 6 digitos), Poblacion, Pobreza_Total,
#'   Agua_Red_Publica, Ruralidad. Todas las tasas en escala [0, 1].
#' @export
inei_covariables_distritales <- function() {
    if (!requireNamespace("readxl", quietly = TRUE) || !requireNamespace("dplyr", quietly = TRUE)) {
        stop("Los paquetes 'readxl' y 'dplyr' son requeridos para este tidying.")
    }

    dir_extdata <- system.file("extdata", package = "ENDES.metodos", mustWork = FALSE)
    if (dir_extdata == "") {
        dir_extdata <- "ENDES_metodos/inst/extdata"
    }

    archivo_pobreza <- file.path(dir_extdata, "inei_pobreza_2018.xlsx")
    archivo_agua <- file.path(dir_extdata, "inei_agua_2017.xlsx")
    archivo_ruralidad <- file.path(dir_extdata, "inei_ruralidad_2017.xlsx")

    # ---- 1. POBREZA MONETARIA (Mapa de Pobreza INEI 2018, Anexo 2) ----
    message("1. Parseando Mapa de Pobreza INEI 2018 (Anexo 2)...")
    df_pobreza_raw <- readxl::read_excel(archivo_pobreza, sheet = "Anexo2", skip = 5, col_names = FALSE)
    df_pobreza <- df_pobreza_raw %>%
        dplyr::select(UBIGEO = 1, Poblacion = 4, Pobreza_Total = 5) %>%
        dplyr::filter(!is.na(UBIGEO) & nchar(UBIGEO) == 6 & UBIGEO != "000000") %>%
        dplyr::mutate(Pobreza_Total = as.numeric(Pobreza_Total) / 100)

    # ---- 2. AGUA POR RED PUBLICA (CPV 2017, Anexo II.1) ----
    # Estructura: Col1=UBIGEO, Col5=Total viviendas, Col7=% agua red publica
    message("2. Parseando Acceso a Agua por Red Publica (CPV 2017)...")
    df_agua_raw <- readxl::read_excel(archivo_agua, sheet = "ANEXO N\u00ba II.1.", skip = 5, col_names = FALSE)
    df_agua <- df_agua_raw %>%
        dplyr::mutate(UBIGEO = trimws(as.character(.[[1]]))) %>%
        dplyr::filter(grepl("^[0-9]{6}$", UBIGEO)) %>%
        dplyr::transmute(
            UBIGEO = UBIGEO,
            Agua_Red_Publica = as.numeric(.[[7]]) / 100
        )
    # Sanitizar: valores fuera de [0,1] -> NA
    df_agua$Agua_Red_Publica <- ifelse(
        df_agua$Agua_Red_Publica >= 0 & df_agua$Agua_Red_Publica <= 1,
        df_agua$Agua_Red_Publica, NA_real_
    )
    message(sprintf("   -> %d distritos con dato de Agua valido.", sum(!is.na(df_agua$Agua_Red_Publica))))

    # ---- 3. RURALIDAD (CPV 2017, POB1) ----
    # Estructura anidada: filas "DISTRITO X" con Total (col2), Urbano (col5), Rural (col8).
    # Sin UBIGEO directo; se reconstruye por contexto jerarquico (Dpto -> Prov -> Dist).
    message("3. Parseando Ruralidad CPV 2017 (POB1)...")
    df_rur_raw <- readxl::read_excel(archivo_ruralidad, sheet = "POB1", skip = 3, col_names = FALSE)

    # Extraer filas de distrito y sus totales
    etiqueta <- trimws(as.character(df_rur_raw[[1]]))
    idx_dist <- grep("^DISTRITO ", etiqueta)

    if (length(idx_dist) > 0) {
        nombres_dist <- sub("^DISTRITO ", "", etiqueta[idx_dist])
        total_pob <- suppressWarnings(as.numeric(df_rur_raw[[2]][idx_dist]))
        rural_pob <- suppressWarnings(as.numeric(df_rur_raw[[8]][idx_dist]))
        # Reemplazar "-" (cero censal) por 0
        rural_pob <- ifelse(is.na(rural_pob), 0, rural_pob)
        total_pob <- ifelse(is.na(total_pob) | total_pob == 0, NA_real_, total_pob)

        prop_rural <- rural_pob / total_pob

        # Para asociar UBIGEO: usamos el orden canonico del CPV que coincide con los
        # 1874 distritos del archivo de Agua (ambos son CPV 2017, mismo ordenamiento INEI).
        if (length(idx_dist) == nrow(df_agua)) {
            df_ruralidad <- data.frame(
                UBIGEO = df_agua$UBIGEO,
                Ruralidad = prop_rural,
                stringsAsFactors = FALSE
            )
            message(sprintf("   -> %d distritos pareados por orden canonico CPV.", nrow(df_ruralidad)))
        } else {
            # Fallback: si no coinciden, retornamos NA y advertimos
            warning(sprintf(
                "Ruralidad: %d distritos en CPV vs %d en Agua. Pareado por orden no es seguro. Retornando NA.",
                length(idx_dist), nrow(df_agua)
            ))
            df_ruralidad <- data.frame(
                UBIGEO = df_agua$UBIGEO,
                Ruralidad = NA_real_,
                stringsAsFactors = FALSE
            )
        }
    } else {
        warning("No se encontraron filas 'DISTRITO' en POB1. Ruralidad sera NA.")
        df_ruralidad <- data.frame(
            UBIGEO = df_agua$UBIGEO,
            Ruralidad = NA_real_,
            stringsAsFactors = FALSE
        )
    }

    # ---- 4. CONSOLIDACION MASTER ----
    message("4. Consolidando Master Covariates...")
    master_df <- df_pobreza %>%
        dplyr::left_join(df_agua, by = "UBIGEO") %>%
        dplyr::left_join(df_ruralidad, by = "UBIGEO")

    message(sprintf("   -> Master: %d distritos, %d covariables completas.",
        nrow(master_df),
        sum(stats::complete.cases(master_df[, c("Pobreza_Total", "Agua_Red_Publica", "Ruralidad")]))
    ))

    return(master_df)
}
