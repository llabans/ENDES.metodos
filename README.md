# ENDES.metodos

[![R-CMD-check](https://github.com/llabans/ENDES.metodos/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/llabans/ENDES.metodos/actions/workflows/R-CMD-check.yaml)

**Herramientas reproducibles en R para el análisis de encuestas poblacionales complejas y la estimación de áreas pequeñas en Perú**

> **Estado del proyecto:** versión experimental `0.1.0`, en desarrollo y validación activa.

`ENDES.metodos` es un paquete de R orientado al análisis estadístico de encuestas poblacionales peruanas, principalmente la **Encuesta Demográfica y de Salud Familiar (ENDES)** y la **Encuesta Nacional de Hogares (ENAHO)**.

El paquete proporciona herramientas para:

* definir diseños muestrales complejos;
* estimar indicadores ponderados de salud;
* realizar análisis por dominios y subpoblaciones;
* ajustar modelos de regresión considerando el diseño de la encuesta;
* producir estimaciones para áreas pequeñas;
* evaluar la incertidumbre y el desempeño predictivo de los modelos;
* construir flujos de trabajo transparentes y reproducibles.

`ENDES.metodos` complementa al paquete [`ENDES.PE`](https://github.com/avallecam/ENDES.PE), que facilita la consulta y descarga de bases públicas de la ENDES.

El proyecto adopta convenciones utilizadas en encuestas del programa **Demographic and Health Surveys (DHS)**, pero no es un producto oficial del INEI, del DHS Program ni de otra institución pública.

---

## Problema que aborda

Las encuestas nacionales como ENDES y ENAHO utilizan diseños muestrales que incorporan:

* estratificación;
* selección por conglomerados;
* probabilidades desiguales de selección;
* factores de expansión;
* estimación por dominios y subpoblaciones.

Ignorar estos elementos puede producir estimaciones incorrectas de las varianzas, los intervalos de confianza y las asociaciones estadísticas.

Además, aunque estas encuestas permiten estimaciones nacionales y regionales, los resultados directos para provincias, distritos u otros dominios pequeños pueden presentar:

* tamaños muestrales reducidos;
* coeficientes de variación elevados;
* varianzas inestables;
* áreas sin observaciones;
* cambios abruptos entre áreas geográficamente cercanas.

`ENDES.metodos` busca integrar métodos basados en el diseño muestral con técnicas de estimación de áreas pequeñas para producir análisis más robustos, transparentes y reproducibles.

---

## Funcionalidades principales

### 1. Diseño y análisis de encuestas complejas

| Función                   | Descripción                                                                                                                        | Estado       |
| ------------------------- | ---------------------------------------------------------------------------------------------------------------------------------- | ------------ |
| `endes_diseno_encuesta()` | Construye un objeto `survey.design` e identifica las variables de conglomerado, estrato y ponderación según la unidad de análisis. | Implementada |
| `endes_subpoblacion()`    | Define subpoblaciones conservando la estructura completa del diseño muestral para estimar correctamente las varianzas.             | Implementada |
| `endes_estimar()`         | Calcula medias, proporciones, totales y razones ponderadas, globales o por dominio.                                                | Implementada |
| `endes_glm_encuesta()`    | Ajusta modelos lineales generalizados mediante `survey::svyglm()` y devuelve resultados estandarizados.                            | Implementada |
| `endes_dominio_directo()` | Obtiene estimaciones directas, errores estándar y varianzas por dominio para su uso posterior en modelos de áreas pequeñas.        | Implementada |

### 2. Construcción de indicadores de salud

| Función                            | Descripción                                                                                       | Estado       |
| ---------------------------------- | ------------------------------------------------------------------------------------------------- | ------------ |
| `endes_vars_anemia()`              | Construye variables de anemia en niños o mujeres a partir de variables ENDES compatibles.         | Experimental |
| `endes_vars_morbilidad_infantil()` | Construye indicadores de diarrea, infección respiratoria aguda y fiebre en menores de cinco años. | Experimental |

Las definiciones de los indicadores deben verificarse frente al cuestionario, diccionario y metodología correspondientes al año de la encuesta analizada.

### 3. Estimación de áreas pequeñas

| Función                          | Descripción                                                                                 | Estado       |
| -------------------------------- | ------------------------------------------------------------------------------------------- | ------------ |
| `endes_sfh_prevalencia()`        | Ajusta un modelo Spatial Fay–Herriot para estimar prevalencias en áreas pequeñas.           | Experimental |
| `endes_sfh_estratificado()`      | Ajusta modelos Spatial Fay–Herriot separados por estratos de pobreza.                       | Experimental |
| `endes_sfh_loo_cv()`             | Ejecuta validación cruzada leave-one-out y calcula RMSE, MAE, sesgo y cobertura empírica.   | Experimental |
| `inei_covariables_distritales()` | Integra covariables distritales abiertas del INEI para su uso en modelos de áreas pequeñas. | Experimental |

### 4. Incertidumbre y métodos computacionales

| Función                | Descripción                                                                                                         | Estado       |
| ---------------------- | ------------------------------------------------------------------------------------------------------------------- | ------------ |
| `endes_bootstrap_ic()` | Construye pesos replicados mediante bootstrap para diseños muestrales complejos.                                    | Experimental |
| `endes_sae_mcmc()`     | Propaga la incertidumbre de prevalencias y denominadores poblacionales mediante simulación Monte Carlo paramétrica. | Experimental |

> La denominación de `endes_sae_mcmc()` se conserva actualmente por compatibilidad con el desarrollo inicial. La implementación corresponde a simulación Monte Carlo paramétrica y su nomenclatura será revisada en una versión futura.

---

## Instalación

La versión de desarrollo puede instalarse directamente desde GitHub:

```r
install.packages("remotes")
remotes::install_github("llabans/ENDES.metodos")
```

Luego puede cargarse con:

```r
library(ENDES.metodos)
```

Para descargar y organizar bases públicas de la ENDES puede utilizarse también:

### Instalación opcional de ENDES.PE
[`ENDES.PE`](https://github.com/avallecam/ENDES.PE) es un paquete complementario que facilita la descarga y organización de bases públicas de la ENDES. No es necesario para utilizar las funciones principales de `ENDES.metodos`.

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

if (!requireNamespace("ENDES.PE", quietly = TRUE)) {
  remotes::install_github("avallecam/ENDES.PE")
}
library(ENDES.PE)
```

---

## Ejemplo reproducible

El siguiente ejemplo utiliza datos simulados para mostrar el flujo básico del paquete sin descargar información externa.

```r
library(ENDES.metodos)

set.seed(2026)

n <- 800

datos <- data.frame(
  V021 = sample(1:80, n, replace = TRUE),          # conglomerado
  V022 = sample(1:20, n, replace = TRUE),          # estrato
  V005 = sample(500000:1500000, n, replace = TRUE),# ponderación DHS
  anemia = rbinom(n, size = 1, prob = 0.35),
  area = factor(sample(c("Urbana", "Rural"), n, replace = TRUE)),
  edad_meses = sample(6:59, n, replace = TRUE),
  distrito = factor(sample(sprintf("D%02d", 1:20), n, replace = TRUE))
)
```

### 1. Definir el diseño muestral

Para niños, el argumento aceptado es `"nino"`, sin tilde:

```r
diseno <- endes_diseno_encuesta(
  datos = datos,
  unidad = "nino"
)
```

### 2. Estimar una prevalencia nacional

```r
estimacion_nacional <- endes_estimar(
  diseno = diseno,
  formula = ~anemia,
  fun = "proporcion"
)

estimacion_nacional
```

La salida contiene:

* estimación ponderada;
* error estándar;
* intervalo de confianza;
* coeficiente de variación;
* tamaño muestral no ponderado.

### 3. Estimar la prevalencia por área de residencia

```r
estimacion_area <- endes_estimar(
  diseno = diseno,
  formula = ~anemia,
  por = ~area,
  fun = "proporcion"
)

estimacion_area
```

### 4. Ajustar un modelo de regresión ponderado

```r
modelo <- endes_glm_encuesta(
  diseno = diseno,
  formula = anemia ~ area + edad_meses,
  familia = quasibinomial(),
  exponenciar = TRUE
)

modelo
```

### 5. Obtener estimaciones directas por dominio

```r
estimaciones_distritales <- endes_dominio_directo(
  diseno = diseno,
  resultado = ~anemia,
  dominio = ~distrito
)

estimaciones_distritales
```

Estas estimaciones directas y sus varianzas pueden utilizarse como insumos para modelos de áreas pequeñas.

---

## Ejemplo conceptual de Spatial Fay–Herriot

Para ajustar un modelo espacial, la matriz de adyacencia y las covariables deben estar ordenadas exactamente como las estimaciones directas.

```r
m <- nrow(estimaciones_distritales)

# Matriz de adyacencia simulada con estructura circular.
# Se utiliza únicamente con fines ilustrativos.
W <- matrix(0, nrow = m, ncol = m)

for (i in seq_len(m)) {
  anterior <- if (i == 1) m else i - 1
  siguiente <- if (i == m) 1 else i + 1

  W[i, anterior] <- 1
  W[i, siguiente] <- 1
}

W <- W / rowSums(W)

covariables <- data.frame(
  pobreza = runif(m, 0.05, 0.80),
  ruralidad = runif(m, 0, 1)
)

modelo_sfh <- endes_sfh_prevalencia(
  estimacion_directa = estimaciones_distritales$estimacion,
  vardir = pmax(estimaciones_distritales$vardir, 1e-8),
  matriz_adyacencia = W,
  covariables = covariables
)

modelo_sfh
```

La matriz espacial empleada en un análisis real debe construirse a partir de información geográfica válida y someterse a evaluación de sensibilidad.

---

## Principios metodológicos

El desarrollo del paquete sigue los siguientes principios:

1. **Respeto del diseño muestral**

   Las ponderaciones, los conglomerados y los estratos deben incorporarse explícitamente en la estimación.

2. **Análisis correcto de subpoblaciones**

   Las subpoblaciones se definen sobre el objeto de diseño, evitando eliminar observaciones antes de estimar las varianzas.

3. **Incertidumbre explícita**

   Los resultados deben incluir errores estándar, intervalos de confianza, varianzas directas o medidas de error predictivo.

4. **Reproducibilidad**

   Los análisis deben poder reconstruirse mediante código, datos públicos o simulados, semillas de aleatorización y documentación metodológica.

5. **Validación frente a referencias**

   Las funciones deben compararse con implementaciones consolidadas, estimaciones oficiales reproducibles y estudios de simulación con valores verdaderos conocidos.

6. **Transparencia sobre las limitaciones**

   Los modelos de áreas pequeñas dependen de sus covariables, transformaciones, estructura espacial y supuestos estadísticos. Una estimación suavizada no es automáticamente una estimación válida, por mucho que el mapa resultante sea visualmente atractivo.

---

## Estado de validación

`ENDES.metodos` se encuentra en una etapa experimental.

Las funciones principales de diseño, estimación y regresión cuentan con pruebas automatizadas iniciales y se apoyan en los paquetes `survey`, `sae`, `sf` y `spdep`. Sin embargo, el paquete aún requiere una validación metodológica más amplia antes de recomendar su uso rutinario.

La estrategia de validación incluye:

* comparación con funciones de referencia del paquete `survey`;
* reproducción de indicadores publicados;
* simulaciones para evaluar sesgo y error cuadrático medio;
* evaluación de la cobertura de intervalos de confianza;
* análisis de sensibilidad a matrices de adyacencia y covariables;
* validación cruzada de modelos de áreas pequeñas;
* revisión de definiciones operativas de indicadores;
* evaluación de casos extremos, valores faltantes y dominios con muestras reducidas.

---

## Limitaciones y uso responsable

* El paquete no reemplaza los manuales metodológicos de ENDES, ENAHO, INEI o DHS.
* Las variables y códigos pueden cambiar entre años, módulos y poblaciones.
* Las funciones experimentales deben ser revisadas antes de emplearse en publicaciones, informes oficiales o decisiones de política pública.
* Los modelos de áreas pequeñas no corrigen automáticamente errores de medición, sesgos de cobertura o covariables mal especificadas.
* Los resultados deben someterse a revisión estadística y epidemiológica.
* Los autores no garantizan la validez de análisis realizados sin verificar los datos de entrada, los supuestos del modelo y la correspondencia entre dominios geográficos.

---

## Hoja de ruta

Las siguientes etapas del proyecto incluyen:

* ampliar las pruebas unitarias y de integración;
* implementar integración continua mediante R CMD check;
* desarrollar estudios formales de simulación;
* validar indicadores frente a estimaciones oficiales reproducibles;
* mejorar el manejo de dominios sin muestra;
* revisar las transformaciones utilizadas para prevalencias extremas;
* fortalecer los diagnósticos de modelos Spatial Fay–Herriot;
* crear viñetas reproducibles en español e inglés;
* preparar una versión estable del paquete;
* publicar un informe metodológico o preprint de validación.

---

## Contribuciones

Las contribuciones de epidemiólogos, estadísticos, científicos de datos y desarrolladores de R son bienvenidas.

Para proponer una modificación:

1. abra un *issue* describiendo el problema o la mejora;
2. cree una rama específica para el cambio;
3. incluya pruebas cuando modifique una función;
4. documente los supuestos metodológicos;
5. envíe un *pull request* con una descripción reproducible.

Los cambios metodológicos deberán incluir referencias, justificación estadística y evidencia de validación.

---

## Citación

Mientras se prepara un archivo formal `CITATION.cff`, la información de citación puede consultarse desde R:

```r
citation("ENDES.metodos")
```

Referencia provisional:

> Labán-Seminario LM. *ENDES.metodos: métodos estadísticos para encuestas poblacionales en Perú*. Paquete de R, versión experimental 0.1.0.

---

## Licencia

Este proyecto se distribuye bajo la licencia **GPL-3**.

---

## Referencias y recursos

* [Programa Demographic and Health Surveys](https://www.dhsprogram.com/)
* [Indicadores de salud: aspectos conceptuales y operativos — OPS/PAHO](https://www3.paho.org/hq/joomlatools-files/docman-files/Health_Indicators-June18-es.pdf)
* [`survey`: análisis de muestras complejas en R](https://cran.r-project.org/package=survey)
* [`sae`: métodos de estimación de áreas pequeñas en R](https://cran.r-project.org/package=sae)
* [`ENDES.PE`](https://github.com/avallecam/ENDES.PE)

---

## Autor y contacto

**Luis Maximiliano Labán Seminario**

* GitHub: [@llabans](https://github.com/llabans)
* Correo: [luis.labans@gmail.com](mailto:luis.labans@gmail.com)

### Colaboradores

* [Andree Valle](https://github.com/avallecam)
* [Percy Soto](https://github.com/psotob91)
