test_that("glm retorna tibble con columnas en espanol", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")
  res <- endes_glm_encuesta(diseno, indicador ~ V012 + V025)

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("termino", "estimacion", "se", "ci_inf", "ci_sup",
                     "p_valor") %in% names(res)))
})

test_that("coeficientes equivalen a svyglm + broom directo", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")

  # Via endes_glm_encuesta (sin exponenciar para comparar)
  res <- endes_glm_encuesta(diseno, indicador ~ V012, exponenciar = FALSE)

  # Via survey directo
  modelo_ref <- survey::svyglm(indicador ~ V012, design = diseno,
                                family = stats::quasibinomial())
  ref <- broom::tidy(modelo_ref, conf.int = TRUE, conf.level = 0.95,
                     exponentiate = FALSE)

  expect_equal(res$estimacion, ref$estimate, tolerance = 1e-10)
  expect_equal(res$se, ref$std.error, tolerance = 1e-10)
})

test_that("exponenciacion produce OR", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")

  res_exp  <- endes_glm_encuesta(diseno, indicador ~ V012, exponenciar = TRUE)
  res_log  <- endes_glm_encuesta(diseno, indicador ~ V012, exponenciar = FALSE)

  # exp(coef_log) == coef_exp
  expect_equal(exp(res_log$estimacion), res_exp$estimacion, tolerance = 1e-8)
})

test_that("error si no es survey.design", {
  expect_error(
    endes_glm_encuesta(datos_mujer, indicador ~ V012),
    "survey.design"
  )
})

test_that("error si formula invalida", {
  diseno <- endes_diseno_encuesta(datos_mujer, unidad = "mujer")
  expect_error(
    endes_glm_encuesta(diseno, "indicador ~ V012"),
    "formula"
  )
})
