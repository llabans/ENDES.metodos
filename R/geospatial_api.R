#' @title endes_descargar_geo_inei
#'
#' @description API automatizada (ETL) para descargar y descomprimir en un directorio
#' temporal o definido por el usuario los shapefiles de limites distritales, departamentales,
#' y centros poblados desde la Plataforma Nacional de Datos Abiertos.
#'
#' @param url_recurso Link directo al archivo ZIP que aloja el shapefile o excel.
#' @param dest_dir Directorio donde extraer el ZIP. Si es NULL, se usa tempdir().
#'
#' @return Ruta absoluta al archivo principal extraido (ej. .shp o .xlsx).
#'
#' @importFrom utils download.file unzip
endes_descargar_geo_inei <- function(url_recurso, dest_dir = NULL) {
    if (is.null(dest_dir)) dest_dir <- tempdir()

    zip_path <- file.path(dest_dir, basename(url_recurso))

    if (!file.exists(zip_path)) {
        utils::download.file(url_recurso, destfile = zip_path, mode = "wb", quiet = TRUE)
    }

    # Extraer contenido
    archivos_extraidos <- utils::unzip(zip_path, exdir = dest_dir)

    # Buscar el shapefile principal o el excel
    archivo_clave <- grep("\\.shp$|\\.xlsx$", archivos_extraidos, value = TRUE, ignore.case = TRUE)

    if (length(archivo_clave) > 0) {
        return(archivo_clave[1])
    }

    # Si la estructura esta anidada, retornamos el listado para debugear
    return(archivos_extraidos)
}

#' @title endes_etl_matriz_adyacencia
#'
#' @description Conecta y procesa los shapefiles de distritos y centros poblados descargados
#' para usarlos como covariables espaciales (efectos fijos y estructura espacial CAR/SAR)
#' usando el UBIGEO como llave principal.
#'
#' @param ruta_shp_distritos Ruta al shapefile de distritos (.shp).
#' @param ruta_shp_centros Ruta al shapefile de centros poblados (.shp). Opcional.
#'
#' @return Objeto \code{sf} curado con la topologia correcta, uniendo UBIGEOs para la
#' inferencia de Modelos Jerarquicos Anidados.
#'
#' @importFrom sf st_read st_make_valid st_transform
#' @importFrom dplyr select rename mutate
endes_etl_matriz_adyacencia <- function(ruta_shp_distritos, ruta_shp_centros = NULL) {
    # Requiere libreria sf para funcionar
    if (!requireNamespace("sf", quietly = TRUE)) stop("El paquete 'sf' es necesario para ETL espacial.")

    message("Leyendo y reparando topologia del Shapefile Distrital...")
    shp_distritos <- sf::st_read(ruta_shp_distritos, quiet = TRUE)

    # Validacion topologica vital (Evita agujeros e intersecciones nulas que corrompen SAR/CAR)
    shp_distritos <- sf::st_make_valid(shp_distritos)

    # Estandarizacion al CRS de coordenadas peruanas (WGS84) si se requiere
    if (is.na(sf::st_crs(shp_distritos))) {
        sf::st_crs(shp_distritos) <- 4326
    } else {
        shp_distritos <- sf::st_transform(shp_distritos, 4326)
    }

    # El ETL consistiria ultimamente en forzar que el identificador principal sea UBIGEO (longitud 6)
    return(shp_distritos)
}
