test_that("auto-deteccion funciona para unidad mujer", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")
  expect_s3_class(diseno, "survey.design2")
  expect_true("V021" %in% names(diseno$variables))
})

test_that("auto-deteccion funciona para unidad hogar", {
  diseno <- endes_diseno_encuesta(datos_hogar, unidad = "hogar")
  expect_s3_class(diseno, "survey.design2")
  expect_true("HV021" %in% names(diseno$variables))
})

test_that("auto-deteccion funciona para unidad hombre", {
  diseno <- endes_diseno_encuesta(datos_hombre, unidad = "hombre")
  expect_s3_class(diseno, "survey.design2")
  expect_true("MV021" %in% names(diseno$variables))
})

test_that("auto-deteccion funciona para unidad nino", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "nino")
  expect_s3_class(diseno, "survey.design2")
})

test_that("error si columna no existe", {
  datos_mal <- datos_mujer[, !names(datos_mujer) %in% "V021"]
  expect_error(
    endes_diseno_encuesta(datos_mal, unidad = "mujer"),
    "no encontrada"
  )
})

test_that("error si unidad no reconocida", {
  expect_error(
    endes_diseno_encuesta(datos_mujer, unidad = "invalida"),
    "no reconocida"
  )
})

test_that("parametros manuales sobreescriben auto-deteccion", {
  diseno <- endes_diseno_encuesta(
    datos_mujer,
    psu = "V021", estrato = "V022", peso = "V005"
  )
  expect_s3_class(diseno, "survey.design2")
})

test_that("resultado equivalente a survey::svydesign directo", {
  options(survey.lonely.psu = "adjust")

  datos_test <- datos_mujer
  datos_test$peso_esc <- datos_test$V005 / 1e6

  diseno_manual <- survey::svydesign(
    ids = ~V021, strata = ~V022, weights = ~peso_esc,
    data = datos_test, nest = TRUE
  )
  diseno_auto <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")

  # Comparar media ponderada
  media_manual <- survey::svymean(~indicador, diseno_manual)
  media_auto   <- survey::svymean(~indicador, diseno_auto)
  expect_equal(as.numeric(coef(media_auto)), as.numeric(coef(media_manual)),
               tolerance = 1e-10)
})

test_that("escala personalizada funciona", {
  datos_test <- datos_mujer
  datos_test$V005 <- datos_test$V005 / 1e3  # pesos mas chicos
  diseno <- endes_diseno_encuesta(datos_test, unidad = "mujer", escala = 1e3)
  expect_s3_class(diseno, "survey.design2")
})
