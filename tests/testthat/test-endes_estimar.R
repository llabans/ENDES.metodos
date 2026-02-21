test_that("proporcion global retorna tibble correcto", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")
  res <- endes_estimar(diseno, ~indicador, fun = "proporcion")

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("variable", "estimacion", "se", "ci_inf", "ci_sup", "cv",
                     "n_sin_pond") %in% names(res)))
  expect_equal(nrow(res), 1)
  expect_true(res$estimacion >= 0 && res$estimacion <= 1)
})

test_that("media global equivale a svymean", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")
  res <- endes_estimar(diseno, ~valor, fun = "media")
  ref <- survey::svymean(~valor, diseno)

  expect_equal(res$estimacion, as.numeric(coef(ref)), tolerance = 1e-10)
  expect_equal(res$se, as.numeric(survey::SE(ref)), tolerance = 1e-10)
})

test_that("total global equivale a svytotal", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")
  res <- endes_estimar(diseno, ~valor, fun = "total")
  ref <- survey::svytotal(~valor, diseno)

  expect_equal(res$estimacion, as.numeric(coef(ref)), tolerance = 1e-10)
})

test_that("estimacion por dominio funciona", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")
  res <- endes_estimar(diseno, ~indicador, por = ~V025, fun = "proporcion")

  expect_s3_class(res, "tbl_df")
  expect_true("V025" %in% names(res))
  expect_equal(nrow(res), length(unique(datos_mujer$V025)))
})

test_that("CI tiene nivel correcto", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")
  res_95 <- endes_estimar(diseno, ~valor, fun = "media", nivel = 0.95)
  res_90 <- endes_estimar(diseno, ~valor, fun = "media", nivel = 0.90)

  # CI 90% debe ser mas estrecho que CI 95%
  ancho_95 <- res_95$ci_sup - res_95$ci_inf
  ancho_90 <- res_90$ci_sup - res_90$ci_inf
  expect_lt(ancho_90, ancho_95)
})

test_that("error si fun no valida", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")
  expect_error(
    endes_estimar(diseno, ~indicador, fun = "invalida"),
    "no reconocida"
  )
})

test_that("n_sin_pond es correcto", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")
  res <- endes_estimar(diseno, ~indicador, fun = "proporcion")
  expect_equal(res$n_sin_pond, sum(!is.na(datos_mujer$indicador)))
})

test_that("cv se calcula correctamente", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")
  res <- endes_estimar(diseno, ~valor, fun = "media")
  expect_equal(res$cv, res$se / res$estimacion, tolerance = 1e-10)
})
