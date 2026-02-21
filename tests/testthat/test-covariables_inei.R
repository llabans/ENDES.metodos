# test-covariables_inei.R
# Tests de integracion para inei_covariables_distritales()
# Requieren los archivos Excel INEI en inst/extdata/

# Helper: detectar si los archivos INEI existen desde cualquier directorio de trabajo
.inei_extdata_disponible <- function() {
  candidatos <- c(
    system.file("extdata", package = "ENDES.metodos", mustWork = FALSE),
    "ENDES_metodos/inst/extdata",
    "../inst/extdata",
    "../../ENDES_metodos/inst/extdata"
  )
  any(sapply(candidatos, function(d) {
    d != "" && file.exists(file.path(d, "inei_pobreza_2018.xlsx"))
  }))
}

test_that("ETL retorna data.frame con columnas esperadas", {
  skip_if_not_installed("readxl")
  skip_if_not(.inei_extdata_disponible(), "Archivos INEI Excel no disponibles")

  df <- inei_covariables_distritales()

  expect_s3_class(df, "data.frame")
  columnas_req <- c("UBIGEO", "Poblacion", "Pobreza_Total", "Agua_Red_Publica", "Ruralidad")
  expect_true(all(columnas_req %in% names(df)))
})

test_that("UBIGEOs son strings de 6 caracteres", {
  skip_if_not_installed("readxl")
  skip_if_not(.inei_extdata_disponible(), "Archivos INEI Excel no disponibles")

  df <- inei_covariables_distritales()

  expect_true(all(nchar(df$UBIGEO) == 6))
  expect_true(all(!is.na(df$UBIGEO)))
  expect_true(!"000000" %in% df$UBIGEO)
})

test_that("Covariables son proporciones [0, 1] o NA", {
  skip_if_not_installed("readxl")
  skip_if_not(.inei_extdata_disponible(), "Archivos INEI Excel no disponibles")

  df <- inei_covariables_distritales()

  for (col in c("Pobreza_Total", "Agua_Red_Publica", "Ruralidad")) {
    vals <- df[[col]][!is.na(df[[col]])]
    expect_true(all(vals >= 0 & vals <= 1),
      info = sprintf("Columna %s tiene valores fuera de [0,1]", col))
  }
})

test_that("Existen al menos 1800 distritos con datos de pobreza", {
  skip_if_not_installed("readxl")
  skip_if_not(.inei_extdata_disponible(), "Archivos INEI Excel no disponibles")

  df <- inei_covariables_distritales()

  n_pobreza <- sum(!is.na(df$Pobreza_Total))
  expect_true(n_pobreza >= 1800)
})

test_that("No hay UBIGEOs duplicados", {
  skip_if_not_installed("readxl")
  skip_if_not(.inei_extdata_disponible(), "Archivos INEI Excel no disponibles")

  df <- inei_covariables_distritales()

  expect_equal(length(df$UBIGEO), length(unique(df$UBIGEO)))
})
