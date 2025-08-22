# ===================================================================
#         ANÁLISIS INTERACTIVO DE MORTALIDAD COVID-19
# ===================================================================

# ========= 1. CARGA DE LIBRERÍAS =========
library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(forcats)
library(sf)
library(DT)
library(plotly)
library(leaflet)
library(shinycssloaders)
library(htmltools)
library(lubridate)
library(tidyr) 

# ========= PREPARACIÓN DE DATOS GLOBALES =========
poblacion_ecuador <- tibble(
  provincia_residencia = c("AZUAY", "BOLIVAR", "CAÑAR", "CARCHI", "CHIMBORAZO", "COTOPAXI", "EL ORO", "ESMERALDAS", "GALAPAGOS", "GUAYAS", "IMBABURA", "LOJA", "LOS RIOS", "MANABI", "MORONA SANTIAGO", "NAPO", "ORELLANA", "PASTAZA", "PICHINCHA", "SANTA ELENA", "SANTO DOMINGO DE LOS TSACHILAS", "SUCUMBIOS", "TUNGURAHUA", "ZAMORA CHINCHIPE"),
  poblacion = c(881394, 209933, 281396, 192938, 517342, 471233, 715751, 643654, 33042, 4393661, 476259, 521915, 921763, 1562079, 192504, 133705, 179265, 114202, 3228233, 401178, 916053, 212075, 590600, 116514)
)

# Se asume que el archivo 'provincias.geojson' está en el directorio de trabajo
ecuador_mapa <- st_read("provincias.geojson") %>% 
  mutate(
    provincia_residencia = toupper(chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", DPA_DESPRO)),
    provincia_residencia = if_else(provincia_residencia == "CAÐAR", "CAÑAR", provincia_residencia)
  )

# ===================================================================
#                   INTERFAZ DE USUARIO (UI)
# ===================================================================
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "COVID-19 Ecuador"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visión General", tabName = "general", icon = icon("dashboard")),
      menuItem("Evolución Temporal", tabName = "temporal", icon = icon("chart-line")),
      menuItem("Análisis Provincial", tabName = "provincial", icon = icon("map-marked-alt")),
      menuItem("Análisis por Sexo", tabName = "sexo", icon = icon("venus-mars")),
      menuItem("Mapa Interactivo", tabName = "mapa", icon = icon("globe-americas")),
      menuItem("Comparativa Provincial", tabName = "comparativa", icon = icon("bar-chart-o")),
      hr(),
      fileInput("file1", "Cargar archivo CSV (delimitado por ;)",
                multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      p("El archivo debe tener las columnas requeridas (ej. 'fecha_defuncion').", style = "font-size: 11px; color: gray; padding: 10px;")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "general",
              fluidRow(
                valueBoxOutput("totalFallecidosBox", width = 4),
                valueBoxOutput("edadPromedioBox", width = 4),
                valueBoxOutput("provinciaMaxBox", width = 4)
              ),
              fluidRow(
                box(
                  title = "Fallecidos por Provincia", status = "primary", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("plotFallecidosProvincia"), type = 6)
                ),
                box(
                  title = "Distribución de Edades", status = "primary", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("plotHistogramaEdad"), type = 6)
                )
              )
      ),
      tabItem(tabName = "temporal",
              fluidRow(
                box(
                  title = "Total de Fallecidos por Mes (Nivel Nacional)", status = "primary", solidHeader = TRUE, width = 12,
                  withSpinner(plotlyOutput("plotResumenMensual", height = "400px"), type = 6),
                  p("Este gráfico muestra el total de fallecidos confirmados agregados por mes durante todo el período.", style = "font-size: 12px; text-align: center; color: gray;")
                )
              ),
              # ======== INICIO DEL BLOQUE AÑADIDO ========
              fluidRow(
                box(
                  title = "Evolución de Fallecidos Totales Acumulados por Provincia", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(4,
                           radioButtons("agrupacion_temporal_total_prov", "Agrupar por:",
                                        choices = c("Día" = "day", "Semana" = "week", "Mes" = "month"),
                                        selected = "week", inline = TRUE)
                    ),
                    column(8,
                           uiOutput("provincia_selector_temporal_total_ui")
                    )
                  ),
                  hr(),
                  withSpinner(plotlyOutput("plotEvolucionTotalProvincial", height = "500px"), type = 6),
                  p("Este gráfico muestra cómo el número total de fallecidos acumulados ha crecido en el tiempo para las provincias seleccionadas.", style = "font-size: 12px; text-align: center; color: gray;")
                )
              ),
              # ======== FIN DEL BLOQUE AÑADIDO ========
              fluidRow(
                box(
                  title = "Evolución de la Tasa de Mortalidad Acumulada por Provincia", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(4,
                           radioButtons("agrupacion_temporal_prov", "Agrupar por:",
                                        choices = c("Día" = "day", "Semana" = "week", "Mes" = "month"),
                                        selected = "week", inline = TRUE)
                    ),
                    column(8,
                           uiOutput("provincia_selector_temporal_ui")
                    )
                  ),
                  hr(),
                  withSpinner(plotlyOutput("plotEvolucionProvincial", height = "500px"), type = 6),
                  p("Este gráfico muestra cómo la tasa de mortalidad total (fallecidos acumulados por 100,000 hab.) ha crecido en el tiempo para las provincias seleccionadas.", style = "font-size: 12px; text-align: center; color: gray;")
                )
              )
      ),
      tabItem(tabName = "provincial",
              fluidRow(
                box(
                  title = "Filtro Provincial (solo para Estadísticas Descriptivas)", status = "info", solidHeader = TRUE, width = 4,
                  uiOutput("provincia_selector_ui")
                ),
                box(
                  title = "Estadísticas Descriptivas por Provincia", status = "info", solidHeader = TRUE, width = 8,
                  withSpinner(DT::dataTableOutput("tablaStatsProvincia"), type = 6)
                )
              ),
              fluidRow(
                box(
                  title = "Tabla de Frecuencias de Edad (Nivel Nacional)", status = "info", solidHeader = TRUE, width = 12,
                  p("Distribución de edades de todos los fallecidos a nivel nacional, agrupada en intervalos según la regla de Sturges."),
                  withSpinner(DT::dataTableOutput("tablaFrecProvincia"), type = 6)
                )
              )
      ),
      tabItem(tabName = "sexo",
              fluidRow(
                box(
                  title = "Distribución de Edad por Sexo", status = "warning", solidHeader = TRUE, width = 7,
                  withSpinner(plotlyOutput("plotDensidadSexo"), type = 6)
                ),
                box(
                  title = "Pruebas de Hipótesis", status = "warning", solidHeader = TRUE, width = 5,
                  h4("1. Prueba F de Igualdad de Varianzas"),
                  p("Esta prueba verifica si la dispersión (varianza) de la edad de fallecimiento es igual entre hombres y mujeres.", style = "font-size: 13px;"),
                  withSpinner(verbatimTextOutput("resultadoFTest")),
                  h5(strong(textOutput("conclusionFTest"))),
                  hr(),
                  h4("2. Prueba T para la Diferencia de Medias"),
                  p("Dados los resultados anteriores, esta prueba determina si existe una diferencia significativa en la edad *media* de fallecimiento.", style = "font-size: 13px;"),
                  withSpinner(verbatimTextOutput("resultadoTTest")),
                  h5(strong(textOutput("conclusionTTest")))
                )
              )
      ),
      tabItem(tabName = "mapa",
              fluidRow(
                box(
                  title = "Mapa Interactivo de Tasa de Mortalidad", status = "danger", solidHeader = TRUE, width = 12,
                  withSpinner(leafletOutput("mapaLeaflet", height = "650px"), type = 6)
                )
              )
      ),
      tabItem(tabName = "comparativa",
              fluidRow(
                box(
                  title = "Tasa de Mortalidad por Provincia",
                  status = "danger", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("plotTasaMortalidad", height = "600px"), type = 6),
                  p("Fallecidos por cada 100,000 habitantes. Ordenado de mayor a menor tasa.", style = "font-size: 12px; color: gray;")
                ),
                box(
                  title = "Fallecidos Totales por Provincia",
                  status = "primary", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("plotFallecidosAbsolutosProvincia", height = "600px"), type = 6),
                  p("Número total de fallecidos. Ordenado de mayor a menor cantidad.", style = "font-size: 12px; color: gray;")
                )
              )
      )
    )
  )
)

# ===================================================================
#                    LÓGICA DEL SERVIDOR (SERVER)
# ===================================================================
server <- function(input, output, session) {
  
  # --- 1. PROCESAMIENTO REACTIVO DE DATOS ---
  datos_mortalidad <- reactive({
    if (is.null(input$file1)) {
      # Usar un archivo de ejemplo si no se carga ninguno
      # Asegúrate de que este archivo exista en tu directorio de trabajo
      df_casos <- read_delim(
        "MSP_cvd19_casos_20220403.csv", 
        delim = ";", 
        col_types = cols(.default = col_character()),
        locale = locale(encoding = "ISO-8859-1")
      )
    } else {
      df_casos <- read_delim(
        input$file1$datapath,
        delim = ";",
        col_types = cols(.default = col_character())
      )
    }
    
    df_casos %>%
      mutate(
        clasificacion_final_limpia = toupper(trimws(clasificacion_final)),
        condicion_final_limpia = toupper(trimws(condicion_final)),
        fecha_evento = ymd(fecha_defuncion),
        edad_paciente = as.numeric(edad_paciente)
      ) %>%
      filter(
        clasificacion_final_limpia == "CONFIRMADO" & 
          condicion_final_limpia %in% c("MUERTO", "FALLECIDO") &
          !is.na(edad_paciente) & edad_paciente >= 0 &
          !is.na(fecha_evento)
      ) %>%
      select(sexo_paciente, provincia_residencia, edad_paciente, fecha_evento)
  })
  
  # --- 2. SALIDAS PARA LA PESTAÑA "VISIÓN GENERAL" ---
  output$totalFallecidosBox <- renderValueBox({
    req(nrow(datos_mortalidad()) > 0)
    total <- nrow(datos_mortalidad())
    valueBox(prettyNum(total, big.mark = ","), "Total Fallecidos", icon = icon("skull-crossbones"), color = "red")
  })
  
  output$edadPromedioBox <- renderValueBox({
    req(nrow(datos_mortalidad()) > 0)
    edad_prom <- round(mean(datos_mortalidad()$edad_paciente, na.rm = TRUE), 1)
    valueBox(edad_prom, "Edad Promedio", icon = icon("user-clock"), color = "blue")
  })
  
  output$provinciaMaxBox <- renderValueBox({
    req(nrow(datos_mortalidad()) > 0)
    prov <- datos_mortalidad() %>%
      count(provincia_residencia) %>%
      top_n(1, n) %>%
      pull(provincia_residencia)
    valueBox(prov, "Provincia con más casos", icon = icon("city"), color = "green")
  })
  
  output$plotFallecidosProvincia <- renderPlotly({
    req(nrow(datos_mortalidad()) > 0)
    df <- datos_mortalidad() %>%
      count(provincia_residencia, name = "total_fallecidos") %>%
      mutate(provincia_residencia = fct_reorder(provincia_residencia, total_fallecidos))
    
    p <- ggplot(df, aes(x = total_fallecidos, y = provincia_residencia, 
                        text = paste("Provincia:", provincia_residencia, "<br>Fallecidos:", comma(total_fallecidos)))) +
      geom_col(fill = "#19c6b9") +
      scale_x_continuous(labels = comma_format()) +
      labs(x = "Número de fallecidos", y = NULL) +
      theme_minimal(base_size = 12)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plotHistogramaEdad <- renderPlotly({
    req(nrow(datos_mortalidad()) > 0)
    p <- ggplot(datos_mortalidad(), aes(x = edad_paciente)) +
      geom_histogram(binwidth = 5, fill = "#1d3557", color = "white", alpha = 0.9) +
      labs(x = "Edad (años)", y = "Número de fallecidos") +
      theme_minimal()
    ggplotly(p)
  })
  
  # --- SALIDAS PARA LA PESTAÑA "ANÁLISIS PROVINCIAL" ---
  output$provincia_selector_ui <- renderUI({
    choices <- sort(unique(datos_mortalidad()$provincia_residencia))
    selectInput("provincia_select", "Seleccione una Provincia:", choices = choices)
  })
  
  output$tablaStatsProvincia <- DT::renderDataTable({
    req(input$provincia_select)
    df <- datos_mortalidad() %>% filter(provincia_residencia == input$provincia_select)
    
    stats <- tibble(
      Estadística = c("Número de Fallecidos", "Edad Promedio", "Desv. Estándar (Edad)", "Edad Mínima", "Edad Máxima"),
      Valor = c(
        nrow(df), 
        round(mean(df$edad_paciente, na.rm=T), 2), 
        round(sd(df$edad_paciente, na.rm=T), 2), 
        min(df$edad_paciente, na.rm=T), 
        max(df$edad_paciente, na.rm=T)
      )
    )
    
    DT::datatable(stats, 
                  options = list(dom = 't', paging = FALSE, searching = FALSE), 
                  rownames = FALSE,
                  caption = tags$caption(style = 'caption-side: top; text-align: left; font-size: 1.2em;', 
                                         paste("Estadísticas para:", input$provincia_select)))
  })
  
  output$tablaFrecProvincia <- DT::renderDataTable({
    df_total <- datos_mortalidad()
    validate(need(nrow(df_total) > 1, "No hay suficientes datos para generar la tabla."))
    
    k <- nclass.Sturges(df_total$edad_paciente)
    cortes <- seq(min(df_total$edad_paciente), max(df_total$edad_paciente), length.out = k + 1)
    
    tabla_frec <- tibble(edad_paciente = df_total$edad_paciente) %>%
      mutate(clase_edad = cut(edad_paciente, breaks = cortes, right = FALSE, include.lowest = TRUE, dig.lab = 4)) %>%
      count(clase_edad, name = "Fa") %>%
      filter(!is.na(clase_edad)) %>%
      arrange(clase_edad) %>%
      mutate(
        Fr = Fa / sum(Fa),
        Fa_Acum = cumsum(Fa),
        Fr_Acum = cumsum(Fr)
      ) %>%
      rename(
        `Clase de Edad` = clase_edad,
        `Frec. Absoluta` = Fa,
        `Frec. Relativa` = Fr,
        `Frec. Abs. Acum.` = Fa_Acum,
        `Frec. Rel. Acum.` = Fr_Acum
      )
    
    DT::datatable(
      tabla_frec,
      rownames = FALSE,
      options = list(pageLength = 10, lengthMenu = c(5, 10, 15), searching = FALSE, dom = 'ltip'),
      caption = tags$caption(style = 'caption-side: top; text-align: left;', 
                             "Tabla de Frecuencias de Edad (Nivel Nacional)")
    ) %>%
      DT::formatPercentage(c('Frec. Relativa', 'Frec. Rel. Acum.'), digits = 2) %>%
      DT::formatRound(c('Frec. Absoluta', 'Frec. Abs. Acum.'), digits = 0)
  })
  
  # --- SALIDAS PARA LAS OTRAS PESTAÑAS ---
  
  # Pestaña Temporal
  output$plotResumenMensual <- renderPlotly({
    req(nrow(datos_mortalidad()) > 0)
    df_mensual <- datos_mortalidad() %>%
      mutate(mes_evento = floor_date(fecha_evento, "month")) %>%
      count(mes_evento, name = "total_fallecidos") %>%
      arrange(mes_evento)
    
    req(nrow(df_mensual) > 0)
    p <- ggplot(df_mensual, aes(x = mes_evento, y = total_fallecidos, 
                                text = paste("Mes:", format(mes_evento, "%b %Y"), "<br>Fallecidos:", comma(total_fallecidos)))) +
      geom_col(fill = "#e31a1c", alpha = 0.8) +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      scale_y_continuous(labels = comma) +
      labs(x = "Mes", y = "Número de Fallecidos") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text") %>% config(displaylogo = FALSE)
  })
  
  # ======== GRÁFICO DE LINEAS ========
  output$provincia_selector_temporal_total_ui <- renderUI({
    choices_prov <- sort(unique(datos_mortalidad()$provincia_residencia))
    selectInput("provincias_select_temporal_total", 
                label = "Seleccione Provincias:",
                choices = choices_prov, selected = c("PICHINCHA", "GUAYAS"), multiple = TRUE, width = '100%')
  })
  
  output$plotEvolucionTotalProvincial <- renderPlotly({
    req(input$provincias_select_temporal_total, nrow(datos_mortalidad()) > 0)
    
    df_periodico <- datos_mortalidad() %>%
      filter(provincia_residencia %in% input$provincias_select_temporal_total) %>%
      mutate(fecha_agrupada = floor_date(fecha_evento, unit = input$agrupacion_temporal_total_prov)) %>%
      count(provincia_residencia, fecha_agrupada, name = "fallecidos_periodo")
    
    df_completo <- df_periodico %>%
      tidyr::complete(provincia_residencia, fecha_agrupada, fill = list(fallecidos_periodo = 0)) %>%
      arrange(provincia_residencia, fecha_agrupada)
    
    df_acumulado <- df_completo %>%
      group_by(provincia_residencia) %>%
      mutate(fallecidos_acumulados = cumsum(fallecidos_periodo)) %>%
      ungroup()
    
    req(nrow(df_acumulado) > 0)
    
    p <- ggplot(df_acumulado, aes(
      x = fecha_agrupada, 
      y = fallecidos_acumulados, 
      color = provincia_residencia, 
      group = provincia_residencia,
      text = paste("<b>Provincia:</b>", provincia_residencia,
                   "<br><b>Fecha:</b>", format(fecha_agrupada, "%Y-%m-%d"),
                   "<br><b>Fallecidos Acumulados:</b>", comma(fallecidos_acumulados))
    )) +
      geom_line(size = 1.1) +
      scale_y_continuous(labels = comma) +
      labs(
        x = "Fecha", 
        y = "Total de Fallecidos Acumulados", 
        color = "Provincia"
      ) +
      theme_minimal() + 
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>% config(displaylogo = FALSE)
  })
  
  
  output$provincia_selector_temporal_ui <- renderUI({
    choices_prov <- sort(unique(datos_mortalidad()$provincia_residencia))
    selectInput("provincias_select_temporal", 
                label = "Seleccione Provincias:",
                choices = choices_prov, selected = c("PICHINCHA", "GUAYAS"), multiple = TRUE, width = '100%')
  })
  
  output$plotEvolucionProvincial <- renderPlotly({
    req(input$provincias_select_temporal, nrow(datos_mortalidad()) > 0, nrow(poblacion_ecuador) > 0)
    
    df_periodico <- datos_mortalidad() %>%
      filter(provincia_residencia %in% input$provincias_select_temporal) %>%
      mutate(fecha_agrupada = floor_date(fecha_evento, unit = input$agrupacion_temporal_prov)) %>%
      count(provincia_residencia, fecha_agrupada, name = "fallecidos_periodo")
    
    df_completo <- df_periodico %>%
      tidyr::complete(provincia_residencia, fecha_agrupada, fill = list(fallecidos_periodo = 0)) %>%
      arrange(provincia_residencia, fecha_agrupada)
    
    df_acumulado <- df_completo %>%
      group_by(provincia_residencia) %>%
      mutate(fallecidos_acumulados = cumsum(fallecidos_periodo)) %>%
      ungroup() %>%
      left_join(poblacion_ecuador, by = "provincia_residencia") %>%
      filter(!is.na(poblacion) & poblacion > 0) %>%
      mutate(tasa_acumulada = (fallecidos_acumulados / poblacion) * 100000)
    
    req(nrow(df_acumulado) > 0)
    
    p <- ggplot(df_acumulado, aes(
      x = fecha_agrupada, 
      y = tasa_acumulada, 
      color = provincia_residencia, 
      group = provincia_residencia,
      text = paste("<b>Provincia:</b>", provincia_residencia,
                   "<br><b>Fecha:</b>", format(fecha_agrupada, "%Y-%m-%d"),
                   "<br><b>Tasa Acumulada:</b>", round(tasa_acumulada, 2), "por 100k",
                   "<br><b>Fallecidos Acumulados:</b>", comma(fallecidos_acumulados))
    )) +
      geom_line(size = 1.1) +
      labs(
        x = "Fecha", 
        y = "Tasa Acumulada (por 100,000 hab.)", 
        color = "Provincia"
      ) +
      theme_minimal() + 
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>% config(displaylogo = FALSE)
  })
  
  # Pestaña Sexo
  output$plotDensidadSexo <- renderPlotly({
    req(nrow(datos_mortalidad()) > 0)
    p <- ggplot(datos_mortalidad(), aes(x = edad_paciente, fill = sexo_paciente)) +
      geom_density(alpha = 0.7) +
      scale_fill_manual(values = c("MUJER" = "#FF6B6B", "HOMBRE" = "#4ECDC4"), name = "Sexo") +
      labs(x = "Edad del Paciente (años)", y = "Densidad") +
      theme_minimal()
    ggplotly(p)
  })
  
  f_test_result <- reactive({
    req(length(unique(datos_mortalidad()$sexo_paciente)) > 1)
    var.test(edad_paciente ~ sexo_paciente, data = datos_mortalidad())
  })
  
  output$resultadoFTest <- renderPrint({ f_test_result() })
  
  output$conclusionFTest <- renderText({
    p_value_f <- f_test_result()$p.value
    if (p_value_f < 0.05) {
      "Conclusión (F-Test): Las varianzas son estadísticamente diferentes (p < 0.05)."
    } else {
      "Conclusión (F-Test): No hay evidencia de que las varianzas sean diferentes (p >= 0.05)."
    }
  })
  
  t_test_result <- reactive({
    req(length(unique(datos_mortalidad()$sexo_paciente)) > 1)
    t.test(edad_paciente ~ sexo_paciente, data = datos_mortalidad(), var.equal = FALSE)
  })
  
  output$resultadoTTest <- renderPrint({ t_test_result() })
  
  output$conclusionTTest <- renderText({
    p_value_t <- t_test_result()$p.value
    if (p_value_t < 0.05) {
      "Conclusión (T-Test): Existe una diferencia estadísticamente significativa."
    } else {
      "Conclusión (T-Test): No hay evidencia de una diferencia significativa."
    }
  })
  
  # === DATOS PRE-CALCULADOS PARA PESTAÑAS GEOGRÁFICAS ===
  datos_tasa_provincia <- reactive({
    req(nrow(datos_mortalidad()) > 0)
    
    datos_mortalidad() %>%
      count(provincia_residencia, name = "total_fallecidos") %>%
      mutate(provincia_residencia = toupper(provincia_residencia)) %>%
      left_join(poblacion_ecuador, by = "provincia_residencia") %>%
      filter(!is.na(poblacion) & poblacion > 0) %>%
      mutate(tasa_mortalidad = (total_fallecidos / poblacion) * 100000)
  })
  
  # --- Pestaña Mapa Interactivo ---
  mapa_datos <- reactive({
    req(nrow(datos_tasa_provincia()) > 0)
    left_join(ecuador_mapa, datos_tasa_provincia(), by = "provincia_residencia")
  })
  
  output$mapaLeaflet <- renderLeaflet({
    req(nrow(mapa_datos()) > 0)
    df_mapa <- mapa_datos()
    pal <- colorNumeric(palette = "viridis", domain = df_mapa$tasa_mortalidad, reverse = TRUE)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Fallecidos: %s<br/>Población: %s<br/>Tasa de mortalidad: %s / 100k",
      df_mapa$DPA_DESPRO, 
      comma(df_mapa$total_fallecidos, accuracy = 1), 
      comma(df_mapa$poblacion, accuracy = 1), 
      round(df_mapa$tasa_mortalidad, 1)
    ) %>% lapply(HTML)
    
    leaflet(df_mapa) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(tasa_mortalidad), 
        weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
        label = labels, 
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")
      ) %>%
      addLegend(pal = pal, values = ~tasa_mortalidad, opacity = 0.7, title = "Tasa de Mortalidad", position = "bottomright")
  })
  
  # --- Pestaña Comparativa Provincial ---
  
  # Gráfico 1: Tasa de Mortalidad
  output$plotTasaMortalidad <- renderPlotly({
    req(nrow(datos_tasa_provincia()) > 0)
    df_grafico <- datos_tasa_provincia() %>%
      mutate(provincia_residencia = fct_reorder(provincia_residencia, tasa_mortalidad))
    
    p <- ggplot(df_grafico, aes(
      x = tasa_mortalidad, 
      y = provincia_residencia, 
      fill = tasa_mortalidad,
      text = paste("<b>Provincia:</b>", provincia_residencia, 
                   "<br><b>Tasa por 100k:</b>", round(tasa_mortalidad, 2),
                   "<br><b>Fallecidos:</b>", comma(total_fallecidos))
    )) +
      geom_col() +
      scale_fill_viridis_c(option = "viridis", direction = -1) +
      labs(x = "Tasa (por 100,000 hab.)", y = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Gráfico 2: Fallecidos Absolutos
  output$plotFallecidosAbsolutosProvincia <- renderPlotly({
    req(nrow(datos_tasa_provincia()) > 0)
    df_grafico <- datos_tasa_provincia() %>%
      mutate(provincia_residencia = fct_reorder(provincia_residencia, total_fallecidos))
    
    p <- ggplot(df_grafico, aes(
      x = total_fallecidos, 
      y = provincia_residencia, 
      fill = total_fallecidos,
      text = paste("<b>Provincia:</b>", provincia_residencia, 
                   "<br><b>Fallecidos:</b>", comma(total_fallecidos),
                   "<br><b>Tasa por 100k:</b>", round(tasa_mortalidad, 2))
    )) +
      geom_col() +
      scale_fill_viridis_c(option = "cividis", direction = -1) +
      scale_x_continuous(labels = comma) +
      labs(x = "Número Total de Fallecidos", y = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
}

# ========= EJECUCIÓN DE LA APLICACIÓN =========
shinyApp(ui = ui, server = server)