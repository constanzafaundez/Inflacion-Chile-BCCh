# **Automatización del Cálculo de Inflación en Chile: Extracción de Datos del IPC mediante API del Banco Central en R**

**Autor**: Constanza Faúndez

---

## 📊 Reporte Interactivo 

[Ver artículo completo](https://constanzafaundez.github.io/Inflacion-Chile-BCCh/inflation.html)  
*(Haz clic para explorar gráficos interactivos)*

[![Preview](https://img.shields.io/badge/Preview-Reporte_HTML-blue?style=for-the-badge&logo=github)](https://constanzafaundez.github.io/Inflacion-Chile-BCCh/inflation.html)

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
```

**Autenticación en la API del BCCh**

Antes de ejecutar la siguiente parte del código, debes:

- Registrar una cuenta en el portal del Banco Central de Chile
- Obtener tus credenciales de acceso (usuario y contraseña)

```r
Sys.setenv(BCCH_USER = "usuario@email.com") #Reemplaza con tu email
Sys.setenv(BCCH_PASS = "contraseña") #Reemplaza con tu contraseña
```
## **3. Extracción de datos**

Función robusta para obtener datos directamente desde la API del Banco Central de Chile:

```r
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
```
## **4. Ejecución de la extracción**

Este proceso automatiza la descarga de datos oficiales del Índice de Precios al Consumidor (IPC) directamente desde la API del Banco Central de Chile, garantizando precisión y actualización en tiempo real.

- **Detalles Técnicos**

  - Serie Utilizada: F074.IPC.IND.Z.EP23.C.M
  - Descripción: IPC General (Base Diciembre 2023 = 100)
  - Periodicidad: Mensual

**Nota: Para otras series (ej. IMACEC, tasa de desempleo), modificar series_code según el catálogo del BCCh.**

```r 
# Extracción de datos

ipc_data <- get_bcch_data("F074.IPC.IND.Z.EP23.C.M")
```

## **5. Procesamiento de datos**

Transformación de datos brutos del IPC en métricas de inflación estandarizadas:

```r
# Obtener los valores de diciembre como referencia

dec_baselines <- ipc_data %>% 
  filter(month(date) == 12) %>%
  mutate(ref_year = year(date) + 1) %>%
  select(ref_year, dec_value = value)

# Calcular todas las métricas de inflación
ipc_processed <- ipc_data %>%
  arrange(date) %>%  # Ordenamos por fecha
  mutate(
    year = year(date),
    month = month(date, label = TRUE, abbr = FALSE),
    month_year = format(date, "%Y-%m")
  ) %>%
  # Unir con los valores de referencia de diciembre
  left_join(dec_baselines, by = c("year" = "ref_year")) %>%
  # Calcular las tres métricas principales
  mutate(
    # Inflación mensual (variación respecto al mes anterior)
    monthly_inflation = (value / lag(value, 1) - 1) * 100,
    
    # Inflación anual (variación respecto al mismo mes del año anterior)
    annual_inflation = (value / lag(value, 12) - 1) * 100,
    
    # Inflación acumulada (variación respecto a diciembre del año anterior)
    bcch_accumulated = (value / dec_value - 1) * 100
  ) %>%
  # Limpiar y organizar el output
  select(
    date, 
    month_year, 
    year, 
    month,
    ipc_index = value,
    monthly_inflation,
    annual_inflation,
    accumulated_inflation = bcch_accumulated
  ) %>%
  # Redondear todos los porcentajes a 1 decimal
  mutate(across(c(monthly_inflation, annual_inflation, accumulated_inflation), 
                ~round(., 1)))


# Mostrar las primeras filas del resultado
head(ipc_processed)
```

## **6. Análsisis de resultados**

Representación gráfica para diagnóstico económico:

  **i. Inflación mensual**

```r

plot_monthly <- ggplot(ipc_processed, aes(x = date, y = monthly_inflation)) +
  geom_col(
    aes(fill = monthly_inflation < 0),  # Colores distintos para inflación positiva/negativa
    width = 20, 
    alpha = 0.8, 
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c("#005293", "#E2001A"), 
    guide = "none"
  ) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.7) +
  labs(
    title = "Inflación Mensual en Chile 2009-2025",
    subtitle = "Variación porcentual mes a mes",
    x = NULL, 
    y = "Variación % mensual",
    caption = "Fuente: Elaboración propia en base a datos del Banco Central de Chile"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 10),
    plot.caption = element_text(color = "gray50"),
    legend.position = "none"
  )

# Convertir a gráfico interactivo con plotly

ggplotly(plot_monthly) %>% 
  layout(
    hovermode = "x unified",
    annotations = list(
      x = 0.5, y = -0.3,
      text = "Nota: Barras azules = inflación +; Barras rojas = inflación -",
      showarrow = FALSE,
      xref = "paper", yref = "paper"
    )
  )

```

El gráfico muestra la volatilidad característica de la inflación mensual, con eventos destacados:

- Protestas sociales (2019)
- Pandemia COVID-19 (2020-2021)
- Presiones inflacionarias post-pandemia (2022)

**ii. Inflación anual**

```r 
plot_annual <- ggplot(ipc_processed %>% filter(!is.na(annual_inflation)), 
                      aes(x = date, y = annual_inflation)) +
  geom_line(color = "#005293", size = 0.5) +
  # Líneas para el rango meta del BCCh (2%-4%)
  geom_hline(yintercept = c(2, 4), linetype = "dashed", color = "#E2001A") +
  geom_hline(yintercept = 0, color = "black") +
  labs(
    title = "Inflación Anual en Chile 2009-2025",
    subtitle = "Variación porcentual en 12 meses",
    x = NULL, 
    y = "Variación % anual",
    caption = "Fuente: Elaboración propia en base a datos del Banco Central de Chile\nLíneas punteadas: Rango meta de inflación BCCh (2%-4%)"
  ) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(-2, 15, by = 1))

# Versión interactiva

ggplotly(plot_annual) %>% 
  layout(
    hovermode = "x unified",
    annotations = list(
      x = 0.5, y = -0.3,
      text = "El rango meta de inflación del BCCh es 2%-4% anual",
      showarrow = FALSE,
      xref = "paper", yref = "paper"
    )
  )
```

El gráfico permite evaluar la efectividad del esquema de metas de inflación:

- Períodos dentro del rango meta (2013-2019)
- Eventos de overshooting inflacionario (2022)
- Efectividad de las medidas contractivas recientes

## **7. Reporte de indicadores actuales**

```r

# Calcular métricas clave

ultimo_mes <- ipc_processed %>% filter(date == max(date))

inflacion_actual <- data.frame(
  "Indicador" = c("Mes", "IPC", "Inflación Mensual", "Inflación Anual", "Inflación Acumulada"),
  "Valor" = c(
    format(ultimo_mes$date, "%B %Y"),
    round(ultimo_mes$ipc_index, 1),
    paste0(ultimo_mes$monthly_inflation, "%"),
    paste0(ultimo_mes$annual_inflation, "%"),
    paste0(ultimo_mes$accumulated_inflation, "%")
  )
)
```
---

## **8. Actualizaciones**

**Última actualización:** 19 mayo 2025  

**Comentario:** Código en desarrollo para automatización de reportes inflacionarios.  

Próximas mejoras incluirán:

- Integración con API BCCh en tiempo real  
- Paneles interactivos con Shiny  
- Validación automática de metadatos  



