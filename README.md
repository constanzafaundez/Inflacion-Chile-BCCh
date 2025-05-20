# **Automatización del Cálculo de Inflación en Chile: Extracción de Datos del IPC mediante API del Banco Central en R**

**Autor**: Constanza Faúndez

---

## 📊 Reporte Interactivo 

[Ver análisis completo](https://tuusuario.github.io/BCCh-Inflation-Analysis/analisis_inflacion.html)  
*(Haz clic para explorar gráficos interactivos)*

[![Preview](https://img.shields.io/badge/Preview-Reporte_HTML-blue?style=for-the-badge&logo=github)](https://tuusuario.github.io/BCCh-Inflation-Analysis/analisis_inflacion.html)

## **Introducción**

Este proyecto automatiza la obtención y procesamiento de datos oficiales del Índice de Precios al Consumidor (IPC) directamente desde la **API** del Banco Central de Chile (BCCh), permitiendo el cálculo automatizado de los principales indicadores de inflación.

### ¿Por qué usar una API?

- **Datos actualizados en tiempo real**: Evita descargas manuales y garantiza información siempre al día.
- **Automatización**: Reduce errores humanos y permite replicabilidad en futuros análisis.
- **Consistencia**: Los datos provienen directamente de la fuente oficial, asegurando confiabilidad.
- **Eficiencia**: Integración fluida con R para procesamiento y visualización sin intermediarios.

Este enfoque proporciona una solución robusta para el monitoreo continuo de la inflación, combinando técnicas modernas de ciencia de datos con información económica oficial (extracción automatizada de datos, procesamiento avanzado, visualización profesional y análisis estructural).

---

## **1. Metodología**

El análisis implementa el marco conceptual oficial del Banco Central de Chile para:

1. **Inflación Mensual**
   - Definición: Variación porcentual del IPC respecto al mes anterior  
   - Fórmula: `(IPC_t / IPC_t-1 - 1) × 100`  
   - Uso: Identifica shocks temporales  

2. **Inflación Anual**
   - Definición: Variación a 12 meses (comparación interanual)  
   - Fórmula: `(IPC_t / IPC_t-12 - 1) × 100`  
   - Relevancia: Principal indicador para política monetaria  

3. **Inflación Acumulada**
   - Definición: Variación respecto a diciembre del año anterior  
   - Fórmula: `(IPC_t / IPC_dic-1 - 1) × 100`  
   - Aplicación: Seguimiento de metas anuales  

---

## **2. Configuración del entorno**

El stack de econometría implementado en R requiere las siguientes librerías:

```r
library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)
library(zoo)
``

Autenticación en la API del BCCh

Antes de ejecutar la siguiente parte del código, debes:

- Registrar una cuenta en el portal del Banco Central de Chile
- Obtener tus credenciales de acceso (usuario y contraseña)

```{r pass}
Sys.setenv(BCCH_USER = "usuario@email.com") #Reemplaza con tu email
Sys.setenv(BCCH_PASS = "contraseña") #Reemplaza con tu contraseña
```
## **3. Extracción de datos**

Función robusta para obtener datos directamente desde la API del Banco Central de Chile:
  get_bcch_data <- function(series_code, start_date = NULL, end_date = NULL) {
    
  #' Obtiene datos del Banco Central de Chile
  #'
  #' @param series_code Código de la serie (ej: "F074.IPC.IND.Z.EP23.C.M")
  #' @param start_date Fecha de inicio en formato "YYYY-MM-DD" (opcional)
  #' @param end_date Fecha de fin en formato "YYYY-MM-DD" (opcional)
  #' @return Dataframe con los datos de la serie
  #'
  
  # Construir URL de la API
  base_url <- "https://si3.bcentral.cl/SieteRestWS/SieteRestWS.ashx"
  
  # Verificar credenciales
  if (Sys.getenv("BCCH_USER") == "" || Sys.getenv("BCCH_PASS") == "") {
    stop("Credenciales BCCH no encontradas. Configure las variables de entorno BCCH_USER y BCCH_PASS")
  }
  
  # Función auxiliar para obtener el rango completo de fechas
  get_date_range <- function(series_code) {
    query_params <- list(
      user = Sys.getenv("BCCH_USER"),
      pass = Sys.getenv("BCCH_PASS"),
      timeseries = series_code,
      operation = "GetSeries"  # Esta operación devuelve el rango completo
    )
    
    response <- GET(url = base_url, query = query_params, timeout(30))
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    if (!"Series" %in% names(data)) {
      stop("No se pudo determinar el rango de fechas disponible")
    }
    
    dates <- data$Series$Obs$indexDateString
    date_objects <- as.Date(dates, format = "%d-%m-%Y")
    
    return(list(
      first_date = min(date_objects),
      last_date = max(date_objects)
    ))
  }
  
  # Determinar fechas si no se especifican
  if (is.null(start_date) || is.null(end_date)) {
    date_range <- get_date_range(series_code)
    if (is.null(start_date)) start_date <- format(date_range$first_date, "%Y-%m-%d")
    if (is.null(end_date)) end_date <- format(date_range$last_date, "%Y-%m-%d")
    
    message(paste("Usando rango completo de fechas disponibles:",
                  start_date, "a", end_date))
  }
  
  # Validar parámetros de entrada
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", start_date) || !grepl("^\\d{4}-\\d{2}-\\d{2}$", end_date)) {
    stop("Las fechas deben estar en formato 'YYYY-MM-DD'")
  }
  
  if (as.Date(start_date) > as.Date(end_date)) {
    stop("La fecha de inicio debe ser anterior a la fecha de fin")
  }
  
  # Crear consulta con fechas determinadas
  query_params <- list(
    user = Sys.getenv("BCCH_USER"),
    pass = Sys.getenv("BCCH_PASS"),
    firstdate = start_date,
    lastdate = end_date,
    timeseries = series_code,
    operation = "GetSeries"
  )
  
  # Hacer la petición con manejo de errores
  tryCatch({
    response <- GET(url = base_url, query = query_params, timeout(30))
    
    if (http_status(response)$category != "Success") {
      stop(paste("Error en la API:", http_status(response)$message))
    }
    
    if (length(content(response, "text", encoding = "UTF-8")) == 0) {
      stop("La API no devolvió datos")
    }
    
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    if (!"Series" %in% names(data) || !"Obs" %in% names(data$Series)) {
      stop("Estructura de datos inesperada de la API")
    }
    
    processed_data <- data$Series$Obs %>%
      mutate(
        date = as.Date(indexDateString, format = "%d-%m-%Y"),
        value = as.numeric(value),
        series_code = series_code,
        year = year(date),
        month = month(date, label = TRUE, abbr = FALSE),
        day = day(date)
      ) %>%
      select(series_code, date, year, month, day, value) %>%
      arrange(date)
    
    return(processed_data)
    
  }, error = function(e) {
    stop(paste("Error al obtener datos:", e$message))
  })
}
#### **4. Ejecución de la extracción**

Este proceso automatiza la descarga de datos oficiales del Índice de Precios al Consumidor (IPC) directamente desde la API del Banco Central de Chile, garantizando precisión y actualización en tiempo real.

- **Detalles Técnicos**

  - Serie Utilizada: F074.IPC.IND.Z.EP23.C.M
  - Descripción: IPC General (Base Diciembre 2023 = 100)
  - Periodicidad: Mensual

**Nota: Para otras series (ej. IMACEC, tasa de desempleo), modificar series_code según el catálogo del BCCh.**

