# ENDES.metodos (en construcción)

**Métodos estadísticos para encuestas poblacionales en Perú**

[![R-CMD-check](https://github.com/llabans/ENDES.metodos/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/llabans/ENDES.metodos/actions/workflows/R-CMD-check.yaml)

`ENDES.metodos` es un prototipo metodológico para el análisis de datos de encuestas poblacionales peruanas (ENDES y ENAHO) basados en métodos geoespaciales y muestrales complejos. El paquete se alinea a la metodología de **DHS (Demographic and Health Surveys)**.

`ENDES.metodos` es un complemento analítico al paquete [ENDES.PE](https://github.com/avallecam/ENDES.PE).

## Desarrollos principales (en revisión-construcción)

*  **1. Construcción de indicadores de salud pública**

¿Qué es un indicador de salud pública?

Son estimaciones que caracterizan la salud de una población. Estas medidas contribuyen a la toma de decisiones en políticas de salud con la finalidad de cumplir metas de salud en la población.

> Fuente: Organización Panamericana de la Salud (OPS/PAHO). [Indicadores de Salud: Aspectos conceptuales y operativos](https://www3.paho.org/hq/joomlatools-files/docman-files/Health_Indicators-June18-es.pdf).

*  **2. Construcción de funciones para el análisis de datos de encuestas poblacionales:**

*   **`endes_diseno_encuesta()`**: Prepara los datos definiendo el diseño muestral complejo (estratificación por conglomerados y cálculo de pesos), consistente con la metodología de DHS.
*   **`endes_glm_encuesta()`**: Modelos lineales generalizados (GLMs pseudo-log-likelihood) adaptados o ponderados para diseños de encuestas (por construir).
*   **`endes_mag_encuesta()`**: Modelos aditivos generalizados (GAMs) adaptados o ponderados para diseños de encuestas (por construir).
*   **`endes_sfh_prevalencia()`**: Estimación de Áreas Pequeñas (SAE) de segunda generación empleando modelos Spatial Fay-Herriot para sub-dominios (por construir).
*   **`endes_sae_mcmc()`**: Estimación de intervalos de confianza mediante métodos computacionales robustos (por construir).

## Instalación

Puedes instalar la versión de desarrollo de `ENDES.metodos` desde [GitHub](https://github.com/) usando el paquete `devtools`:

```r
# install.packages("devtools")
devtools::install_github("llabans/ENDES.metodos")
```

## Uso Básico

```r
library(ENDES.metodos)
library(ENDES.PE) # Para descargar los datos de prueba

# 1. Descargando datos (P.e: ENDES Salud - Módulo Mujeres)
datos <- consulta_endes(periodo = 2022, codigo_modulo = 414, base = "CSALUD01")

# 2. Definiendo el diseño muestral complejo
diseno <- endes_diseno_encuesta(datos, unidad = "niños")

# 3. Estimando prevalencia de anemia en niños nivel distrital 
# anemia = variable binaria de anemia

estimacion_distrital <- endes_estimar(
  diseno = diseno, 
  formula = ~anemia, 
  por = ~ubigeo_distrito, 
  fun = "proporcion"
)

# 4. Evalúar calidad. Coeficiente de Variación (CV) muy alto, estimar con métodos de área pequeña: Fay-Herriot espacial.

# 5. Estimando prevalencia de anemia en niños nivel distrital corregida. Para controlar varianza provincial se utiliza la función de endes_sfh_prevalencia basada en métodos de Fay-Herriot espacial.

anemia_distrito <- endes_sfh_prevalencia(
  estimaciones_directas = estimacion_distrital,
  matriz_vecindad_espacial = matriz_distritos, # matriz de distritos adyacentes
  covariables_auxiliares = datos_censo # variables predictoras
)

```

## Información adicional

`ENDES.metodos` es un paquete experimental que se encuentra en proceso de construcción mediante la colaboración de estadísticos y epidemiólogos. Este paquete es la versión peruana de Demographic Health Surveys (DHS) [DHS Program](https://www.dhsprogram.com/). Las estimaciones obtenidas con el diseño actual no deben ser utilizadas en publicaciones y el autor no se responsabiliza por el uso indebido de las estimaciones obtenidas con el diseño actual. 

Se implementarán y validarán métodos estadísticos computacionales que permitan un análisis robusto, transparente y reproducible de encuestas poblacionales peruanas.

## Contacto 

* [Luis Labans](https://github.com/llabans)
* [Andree Valle](https://github.com/avallecam)
* [Percy Soto](https://github.com/psotob91/)


