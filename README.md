# Laboratorio 3 – Minería de Datos

**Universidad del Valle de Guatemala**
**Curso:** CC3074 – Minería de Datos
**Semestre:** I – 2026

## Integrantes

* Anggelie Velásquez
* Anthony Lou
* Isabella Obando

---

# Descripción del proyecto

Este repositorio contiene el desarrollo del **Laboratorio 3 del curso de Minería de Datos**, cuyo objetivo es analizar un conjunto de datos de listados de Airbnb y construir **modelos de regresión lineal para predecir el precio de las propiedades**.

El laboratorio simula una consultoría para la empresa **SmartStay Advisors**, que busca utilizar técnicas de minería de datos para:

* estimar precios competitivos de propiedades
* identificar propiedades con bajo desempeño
* comprender qué factores influyen en el precio y la ocupación
* mejorar la rentabilidad de las propiedades mediante análisis de datos

El análisis se realiza utilizando el dataset **`listings.RData`**, el cual contiene información sobre características de alojamientos, disponibilidad, reseñas y precios.

---

# Dataset

**Archivo:** `listings.RData`

El dataset contiene aproximadamente:

* **171,748 observaciones**
* **80 variables**

Algunas variables importantes incluyen:

* `price` – precio del alojamiento
* `bedrooms` – número de habitaciones
* `accommodates` – capacidad de huéspedes
* `room_type` – tipo de alojamiento
* `review_scores_rating` – calificación promedio de reseñas

Estas variables permiten analizar factores que podrían influir en el precio de los alojamientos.

---

# Estructura del análisis

El laboratorio se divide en varias etapas:

### 1. Análisis exploratorio de datos

Se realiza un análisis inicial del dataset para entender su estructura y comportamiento.

Incluye:

* descripción de variables
* análisis de distribuciones
* análisis de relaciones entre variables
* visualización de datos

### 2. Preprocesamiento

Se realizan transformaciones necesarias para preparar los datos para el análisis, por ejemplo:

* conversión de variables
* manejo de valores faltantes
* selección de variables relevantes

### 3. Modelos de regresión lineal

Se construyen diferentes modelos para predecir el precio:

* regresión lineal simple
* regresión lineal múltiple
* modelos con regularización (Ridge, Lasso o ElasticNet)

### 4. Evaluación de modelos

Los modelos se comparan utilizando un conjunto de prueba para evaluar su capacidad de predicción.

---

# Tecnologías utilizadas

El análisis se realizó utilizando:

* **R**
* **RStudio**

Principales librerías:

```
ggplot2
dplyr
tidyverse
```

---

# Contenido del repositorio

```
.
├── listings.RData
├── relaciones_variables.R
├── analisis_exploratorio.R
├── modelos_regresion.R
├── informe.Rmd / informe.html
└── README.md
```

---

# Reproducibilidad

Para reproducir el análisis:

1. Clonar el repositorio
2. Abrir el proyecto en **RStudio**
3. Cargar el dataset `listings.RData`
4. Ejecutar los scripts en orden

Ejemplo:

```r
load("listings.RData")
source("relaciones_variables.R")
```

---

# Resultados

El análisis exploratorio permitió identificar variables que influyen en el precio de los alojamientos, como:

* capacidad de huéspedes (`accommodates`)
* número de habitaciones (`bedrooms`)
* tipo de alojamiento (`room_type`)

Estas variables se utilizan posteriormente para construir modelos predictivos de regresión.

