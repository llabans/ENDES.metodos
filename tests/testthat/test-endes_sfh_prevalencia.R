# test-endes_sfh_prevalencia.R
# Tests unitarios para endes_sfh_prevalencia() y endes_bootstrap_ic()

# --- Datos simulados para SAE ---
set.seed(123)
n_dom <- 10

# Prevalencias simuladas con variabilidad realista
prev_simulada <- runif(n_dom, 0.15, 0.85)
vardir_simulada <- runif(n_dom, 0.001, 0.05)

# Matriz de adyacencia simetrica k-NN simulada (row-standardized)
W_sim <- matrix(0, n_dom, n_dom)
for (i in 1:n_dom) {
  vecinos <- setdiff(c(max(1, i - 1), min(n_dom, i + 1), max(1, i - 2), min(n_dom, i + 2)), i)
  W_sim[i, vecinos] <- 1 / length(vecinos)
}

# N efectivo para Freeman-Tukey
n_eff_simulado <- runif(n_dom, 10, 200)

# ---- Tests: endes_sfh_prevalencia ----

test_that("SFH Intercept-Only converge y retorna estructura correcta", {
  skip_if_not_installed("sae")

  resultado <- endes_sfh_prevalencia(
    estimacion_directa = prev_simulada,
    vardir = vardir_simulada,
    matriz_adyacencia = W_sim,
    covariables = NULL,
    method = "REML"
  )

  expect_s3_class(resultado, "data.frame")
  expect_true(all(c("estimacion_directa", "vardir", "eblup_espacial", "mse_eblup") %in% names(resultado)))
  expect_equal(nrow(resultado), n_dom)
  expect_true(all(!is.na(resultado$eblup_espacial)))
})

test_that("SFH con Freeman-Tukey Arcsine retorna proporciones [0,1]", {
  skip_if_not_installed("sae")

  resultado <- endes_sfh_prevalencia(
    estimacion_directa = prev_simulada,
    vardir = vardir_simulada,
    matriz_adyacencia = W_sim,
    covariables = NULL,
    method = "REML",
    n_efectivo = n_eff_simulado
  )

  expect_true(all(resultado$eblup_espacial >= 0 & resultado$eblup_espacial <= 1))
  expect_true(all(resultado$mse_eblup >= 0))
})

test_that("SFH con covariables auxiliares funciona", {
  skip_if_not_installed("sae")

  # Aumentar dominios para evitar singularidad matricial con covariables
  n_cov <- 20
  set.seed(999)
  prev_cov <- runif(n_cov, 0.15, 0.85)
  var_cov <- runif(n_cov, 0.005, 0.03)

  W_cov <- matrix(0, n_cov, n_cov)
  for (i in 1:n_cov) {
    vecinos <- setdiff(c(max(1, i - 1), min(n_cov, i + 1)), i)
    W_cov[i, vecinos] <- 1 / length(vecinos)
  }

  df_cov <- data.frame(
    Pobreza_Total = runif(n_cov, 0.05, 0.75)
  )

  resultado <- endes_sfh_prevalencia(
    estimacion_directa = prev_cov,
    vardir = var_cov,
    matriz_adyacencia = W_cov,
    covariables = df_cov,
    method = "REML"
  )

  expect_s3_class(resultado, "data.frame")
  expect_true(all(!is.na(resultado$eblup_espacial)))
  expect_true(!is.null(attr(resultado, "sfh_fit")))
})

test_that("SFH rechaza dimension incorrecta de W", {
  skip_if_not_installed("sae")

  W_mal <- matrix(0, n_dom + 1, n_dom + 1)
  expect_error(
    endes_sfh_prevalencia(prev_simulada, vardir_simulada, W_mal),
    "dimension"
  )
})

test_that("SFH valida maxiter y precision", {
  skip_if_not_installed("sae")

  expect_error(
    endes_sfh_prevalencia(prev_simulada, vardir_simulada, W_sim, maxiter = 0),
    "maxiter"
  )
  expect_error(
    endes_sfh_prevalencia(prev_simulada, vardir_simulada, W_sim, precision = 0),
    "precision"
  )
})

test_that("SFH maneja prevalencias extremas (p~0, p~1) sin NaN", {
  skip_if_not_installed("sae")

  prev_extrema <- c(0.001, 0.01, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.99, 0.999)
  vardir_ext <- rep(0.01, n_dom)

  resultado <- endes_sfh_prevalencia(
    estimacion_directa = prev_extrema,
    vardir = vardir_ext,
    matriz_adyacencia = W_sim,
    n_efectivo = rep(50, n_dom)
  )

  expect_true(all(!is.nan(resultado$eblup_espacial)))
  expect_true(all(!is.infinite(resultado$eblup_espacial)))
})

# ---- Tests: endes_bootstrap_ic ----

test_that("Bootstrap genera svyrep.design valido", {
  options(survey.lonely.psu = "adjust")
  diseno <- survey::svydesign(
    id = ~V021, strata = ~V022,
    weights = ~I(V005 / 1e6),
    data = datos_mujer, nest = TRUE
  )

  diseno_boot <- endes_bootstrap_ic(diseno, rep = 50)
  expect_s3_class(diseno_boot, "svyrep.design")
})

test_that("Bootstrap rechaza input no-survey", {
  expect_error(
    endes_bootstrap_ic(data.frame(x = 1:10)),
    "survey.design"
  )
})

# ---- Tests: endes_sae_mcmc ----

test_that("MCMC bivariado retorna estructura correcta", {
  set.seed(77)
  n <- 5
  res <- endes_sae_mcmc(
    prevalencia = runif(n, 0.1, 0.8),
    mse_prevalencia = rep(0.01, n),
    poblacion = sample(1000:50000, n),
    var_poblacion = rep(100^2, n),
    dw = 0.004,
    n_draws = 500,
    seed = 42
  )

  expect_s3_class(res, "data.frame")
  cols_req <- c("prevalencia", "poblacion", "yld_media", "yld_mediana",
                "yld_q025", "yld_q975", "yld_sd")
  expect_true(all(cols_req %in% names(res)))
  expect_equal(nrow(res), n)
  expect_true(all(res$yld_media >= 0))
  expect_true(all(res$yld_q025 <= res$yld_mediana))
  expect_true(all(res$yld_mediana <= res$yld_q975))
})

test_that("MCMC con var_poblacion NULL genera warning y resultado valido", {
  expect_warning(
    res <- endes_sae_mcmc(
      prevalencia = c(0.3, 0.5),
      mse_prevalencia = c(0.01, 0.02),
      poblacion = c(10000, 20000),
      var_poblacion = NULL,
      dw = 0.1,
      n_draws = 200,
      seed = 1
    ),
    "var_poblacion no especificada"
  )
  expect_equal(nrow(res), 2)
  expect_true(all(res$yld_media > 0))
})

test_that("MCMC rechaza inputs dimensionalmente inconsistentes", {
  expect_error(
    endes_sae_mcmc(c(0.3, 0.5), c(0.01), c(1000, 2000), dw = 0.1),
    "misma longitud"
  )
  expect_error(
    endes_sae_mcmc(c(0.3, 0.5), c(0.01, 0.02), c(1000), dw = 0.1),
    "misma longitud"
  )
})

test_that("MCMC rechaza DW fuera de rango", {
  expect_error(
    endes_sae_mcmc(0.5, 0.01, 1000, dw = 1.5),
    "escalar numerico"
  )
})

test_that("MCMC es reproducible con seed", {
  args <- list(
    prevalencia = c(0.3, 0.6),
    mse_prevalencia = c(0.01, 0.02),
    poblacion = c(5000, 10000),
    var_poblacion = c(250^2, 500^2),
    dw = 0.05,
    n_draws = 300,
    seed = 123
  )

  res1 <- do.call(endes_sae_mcmc, args)
  res2 <- do.call(endes_sae_mcmc, args)

  expect_equal(res1$yld_media, res2$yld_media)
  expect_equal(res1$yld_q025, res2$yld_q025)
})

# ---- Tests: endes_sfh_estratificado ----

test_that("SFH estratificado produce resultados por estrato", {
  skip_if_not_installed("sae")
  skip_if_not_installed("sf")
  skip_if_not_installed("spdep")

  set.seed(42)
  n_e <- 30
  prev_e <- runif(n_e, 0.10, 0.90)
  var_e <- runif(n_e, 0.005, 0.03)
  ubigeo_e <- sprintf("%06d", seq_len(n_e))
  pobreza_e <- c(rep(0.15, 10), rep(0.40, 10), rep(0.70, 10))

  # Crear sf con puntos
  coords <- data.frame(x = runif(n_e, -80, -70), y = runif(n_e, -18, -2))
  shp_e <- sf::st_as_sf(data.frame(UBIGEO = ubigeo_e, coords), coords = c("x", "y"), crs = 4326)

  resultado <- endes_sfh_estratificado(
    estimacion_directa = prev_e,
    vardir = var_e,
    ubigeo = ubigeo_e,
    pobreza = pobreza_e,
    shp = shp_e,
    k_vecinos = 3L,
    method = "REML"
  )

  expect_s3_class(resultado, "data.frame")
  expect_true("estrato" %in% names(resultado))
  expect_equal(sort(unique(resultado$estrato)), c("alto", "bajo", "medio"))
  expect_equal(nrow(resultado), n_e)
  expect_true(all(!is.na(resultado$eblup_espacial)))
  expect_true(!is.null(attr(resultado, "sfh_fits")))
})

test_that("SFH estratificado valida inputs inconsistentes", {
  skip_if_not_installed("sae")
  skip_if_not_installed("sf")

  expect_error(
    endes_sfh_estratificado(
      estimacion_directa = c(0.3, 0.5),
      vardir = c(0.01),  # longitud incorrecta
      ubigeo = c("010101", "010102"),
      pobreza = c(0.2, 0.6),
      shp = sf::st_as_sf(data.frame(UBIGEO = c("010101", "010102"),
                                     x = c(-77, -76), y = c(-12, -11)),
                          coords = c("x", "y"), crs = 4326)
    ),
    "misma longitud"
  )
})

test_that("SFH estratificado rechaza cortes de pobreza invalidos", {
  skip_if_not_installed("sae")
  skip_if_not_installed("sf")

  n_t <- 10
  shp_t <- sf::st_as_sf(
    data.frame(UBIGEO = sprintf("%06d", 1:n_t),
               x = runif(n_t), y = runif(n_t)),
    coords = c("x", "y"), crs = 4326
  )

  expect_error(
    endes_sfh_estratificado(
      estimacion_directa = runif(n_t), vardir = rep(0.01, n_t),
      ubigeo = sprintf("%06d", 1:n_t), pobreza = runif(n_t),
      shp = shp_t, cortes_pobreza = c(0.7, 0.3)  # invertidos
    ),
    "cortes crecientes"
  )
})

test_that("SFH estratificado valida maxiter_sfh y precision_sfh", {
  skip_if_not_installed("sae")
  skip_if_not_installed("sf")

  n_t <- 10
  shp_t <- sf::st_as_sf(
    data.frame(UBIGEO = sprintf("%06d", 1:n_t),
               x = runif(n_t), y = runif(n_t)),
    coords = c("x", "y"), crs = 4326
  )

  expect_error(
    endes_sfh_estratificado(
      estimacion_directa = runif(n_t), vardir = rep(0.01, n_t),
      ubigeo = sprintf("%06d", 1:n_t), pobreza = runif(n_t),
      shp = shp_t, maxiter_sfh = 0
    ),
    "maxiter_sfh"
  )
  expect_error(
    endes_sfh_estratificado(
      estimacion_directa = runif(n_t), vardir = rep(0.01, n_t),
      ubigeo = sprintf("%06d", 1:n_t), pobreza = runif(n_t),
      shp = shp_t, precision_sfh = 0
    ),
    "precision_sfh"
  )
})

# ---- Tests: endes_sfh_loo_cv ----

test_that("LOO-CV retorna metricas y predicciones validas", {
  skip_if_not_installed("sae")

  set.seed(321)
  n_loo <- 15
  prev_loo <- runif(n_loo, 0.2, 0.8)
  var_loo <- runif(n_loo, 0.005, 0.02)

  W_loo <- matrix(0, n_loo, n_loo)
  for (i in 1:n_loo) {
    vecinos <- setdiff(c(max(1, i - 1), min(n_loo, i + 1)), i)
    W_loo[i, vecinos] <- 1 / length(vecinos)
  }

  res_cv <- endes_sfh_loo_cv(
    estimacion_directa = prev_loo,
    vardir = var_loo,
    matriz_adyacencia = W_loo,
    method = "REML"
  )

  expect_type(res_cv, "list")
  expect_true(all(c("predicciones", "metricas") %in% names(res_cv)))
  expect_s3_class(res_cv$predicciones, "data.frame")
  expect_true(res_cv$metricas$rmse >= 0)
  expect_true(res_cv$metricas$cobertura_95 >= 0 && res_cv$metricas$cobertura_95 <= 1)
  expect_true(res_cv$metricas$n_evaluados > 0)
})
