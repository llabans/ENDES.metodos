test_that("dominio directo retorna tibble correcto", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")
  res <- endes_dominio_directo(diseno, ~indicador, ~V025)

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("V025", "estimacion", "se", "ci_inf", "ci_sup",
                     "vardir", "cv", "n_sin_pond") %in% names(res)))
  expect_equal(nrow(res), length(unique(datos_mujer$V025)))
})

test_that("equivalente a svyby directo", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")
  res <- endes_dominio_directo(diseno, ~indicador, ~V025)

  ref <- survey::svyby(~indicador, ~V025, diseno, survey::svymean,
                       na.rm = TRUE, vartype = c("se", "ci"))
  ref_df <- as.data.frame(ref)

  expect_equal(res$estimacion, ref_df$indicador, tolerance = 1e-10)
  expect_equal(res$se, ref_df$se, tolerance = 1e-10)
})

test_that("vardir es se al cuadrado", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")
  res <- endes_dominio_directo(diseno, ~indicador, ~V025)

  expect_equal(res$vardir, res$se^2, tolerance = 1e-12)
})

test_that("cv se calcula correctamente", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")
  res <- endes_dominio_directo(diseno, ~indicador, ~V025)

  cv_esperado <- ifelse(res$estimacion != 0, res$se / res$estimacion, NA_real_)
  expect_equal(res$cv, cv_esperado, tolerance = 1e-10)
})

test_that("error si no es survey.design", {
  expect_error(
    endes_dominio_directo(datos_mujer, ~indicador, ~V025),
    "survey.design"
  )
})

test_that("funciona con multiples dominios", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")
  res <- endes_dominio_directo(diseno, ~indicador, ~V024)
  expect_equal(nrow(res), length(unique(datos_mujer$V024)))
})
