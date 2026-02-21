test_that("subpoblacion retorna survey.design", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")
  sub <- endes_subpoblacion(diseno, V025 == 1)
  expect_s3_class(sub, "survey.design2")
})

test_that("subpoblacion reduce observaciones", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")
  sub <- endes_subpoblacion(diseno, V025 == 1)
  expect_lt(nrow(sub$variables), nrow(diseno$variables))
})

test_that("equivalente a survey::subset directo", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")
  sub_auto   <- endes_subpoblacion(diseno, V025 == 1)
  sub_manual <- subset(diseno, V025 == 1)

  media_auto   <- survey::svymean(~indicador, sub_auto)
  media_manual <- survey::svymean(~indicador, sub_manual)
  expect_equal(as.numeric(coef(media_auto)), as.numeric(coef(media_manual)))
})

test_that("error si no es survey.design", {
  expect_error(
    endes_subpoblacion(datos_mujer, V025 == 1),
    "survey.design"
  )
})
