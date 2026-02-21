test_that("agrega columnas de morbilidad", {
  res <- endes_vars_morbilidad_infantil(datos_nino)

  expect_true("eda_2sem" %in% names(res))
  expect_true("ira_2sem" %in% names(res))
  expect_true("fiebre_2sem" %in% names(res))
})

test_that("codificacion EDA es correcta", {
  # Caso positivo: H11 %in% c(1,2), B5==1, B19<60
  caso <- data.frame(B5 = 1, B19 = 24, H11 = 1, H22 = 0,
                     H31B = 0, H31C = 2, stringsAsFactors = FALSE)
  res <- endes_vars_morbilidad_infantil(caso)
  expect_true(res$eda_2sem)

  # Caso negativo: H11 == 0
  caso$H11 <- 0
  res <- endes_vars_morbilidad_infantil(caso)
  expect_false(res$eda_2sem)

  # Fuera de universo: B19 >= 60
  caso$B19 <- 65
  caso$H11 <- 1
  res <- endes_vars_morbilidad_infantil(caso)
  expect_true(is.na(res$eda_2sem))
})

test_that("codificacion IRA es correcta", {
  # Positivo: H31B==1 y H31C %in% c(1,3)
  caso <- data.frame(B5 = 1, B19 = 12, H11 = 0, H22 = 0,
                     H31B = 1, H31C = 1, stringsAsFactors = FALSE)
  res <- endes_vars_morbilidad_infantil(caso)
  expect_true(res$ira_2sem)

  # H31C == 2 -> negativo
  caso$H31C <- 2
  res <- endes_vars_morbilidad_infantil(caso)
  expect_false(res$ira_2sem)
})

test_that("codificacion fiebre es correcta", {
  caso <- data.frame(B5 = 1, B19 = 36, H11 = 0, H22 = 1,
                     H31B = 0, H31C = 2, stringsAsFactors = FALSE)
  res <- endes_vars_morbilidad_infantil(caso)
  expect_true(res$fiebre_2sem)

  caso$H22 <- 0
  res <- endes_vars_morbilidad_infantil(caso)
  expect_false(res$fiebre_2sem)
})

test_that("NA en universo (nino fallecido)", {
  caso <- data.frame(B5 = 0, B19 = 24, H11 = 1, H22 = 1,
                     H31B = 1, H31C = 1, stringsAsFactors = FALSE)
  res <- endes_vars_morbilidad_infantil(caso)
  expect_true(is.na(res$eda_2sem))
  expect_true(is.na(res$ira_2sem))
  expect_true(is.na(res$fiebre_2sem))
})

test_that("error si faltan B5 y B19", {
  datos_sin <- data.frame(H11 = 1, H22 = 0)
  expect_error(
    endes_vars_morbilidad_infantil(datos_sin),
    "no encontrada"
  )
})

test_that("manejo de NAs en variables fuente", {
  caso <- data.frame(B5 = 1, B19 = 24, H11 = NA, H22 = NA,
                     H31B = NA, H31C = NA, stringsAsFactors = FALSE)
  res <- endes_vars_morbilidad_infantil(caso)
  # NA en H11 debe producir FALSE (NA %in% c(1,2) es FALSE)
  expect_false(res$eda_2sem)
})
