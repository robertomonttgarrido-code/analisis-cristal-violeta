options(encoding = "UTF-8")

library(shiny)
library(bslib)
library(dplyr)
library(sf)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(ggplot2)
library(DT)
library(htmltools)
library(htmlwidgets)


Departamentos_Honduras <- readRDS("data/departamentos_honduras_simplificado.rds")

honduras_recortado <- readRDS("data/hidrografia_honduras_simplificada.rds")

df_trimestres <- readRDS("data/df_trimestres.rds")

Casos_GBG_internos_honduras <- readRDS("data/casos_gbg_internos_honduras.rds")

fincas_camaroneras_general <- readRDS("data/fincas_camaroneras_general.rds")

Establecimientos_bovinos_choluteca_y_valle <- readRDS(
  "data/establecimientos_bovinos_choluteca_y_valle.rds"
)

Deptos_Honduras <- readRDS("data/deptos_honduras.rds")

colores_honduras_ley<- c("Presencia CV"="red",
                         "Ausencia CV"="green",
                         "Casos GBG"="#B23AEE",
                         "Granjas bovinas"="#79CDCD",
                         "Fincas camaroneras"="orange")

pal_deptos <- colorNumeric(
  palette = c("#ffffcc", "#ffeda0", "#feb24c", "#fd8d3c", "#f03b20", "#bd0026"),
  domain = Departamentos_Honduras$`N casos GBG`
)

df_trimestres <- df_trimestres %>%
  mutate(
    `FECHA DE RECOLECCION DE LA MUESTRA` = as.Date(`FECHA DE RECOLECCION DE LA MUESTRA`)
  )

Casos_GBG_internos_honduras <- Casos_GBG_internos_honduras %>%
  mutate(
    FECHA = as.Date(FECHA)
  )

ui <- page_navbar(
  title = "Analisis Cristal Violeta",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    title = "Controles",
    sliderInput("rango_fechas", 
                "Periodo de muestreo:",
                min = as.Date("2024-10-01"), 
                max = as.Date("2026-03-31"),
                value = c(as.Date("2024-10-01"), as.Date("2026-03-31")), 
                timeFormat = "%b %Y", 
                step = 15,             
                animate = TRUE),      
    
    checkboxGroupInput("estatus_sel", "Estatus CV:",
                       choices = c("Presencia CV", "Ausencia CV"),
                       selected = c("Presencia CV", "Ausencia CV")),
    hr(),
    card(
      card_header("Resumen Dinamico"),
      textOutput("conteo_total"),
      span(textOutput("conteo_pos"), style = "color: red; font-weight: bold;"),
      span(textOutput("conteo_neg"), style = "color: green; font-weight: bold;")
    )
  ),
  
  selectInput(
    inputId = "tipo_finca_sel",
    label = "Tipo de finca:",
    choices = c(
      "Todos" = "Todos",
      sort(unique(na.omit(df_trimestres$`Tipo finca`)))
    ),
    selected = "Todos",
    multiple = TRUE
  ),
  
  nav_panel("Mapa de Vigilancia", 
            leafletOutput("mapa_cv", height = "750px")),
  
  nav_panel("Analisis Temporal", 
            card(
              card_header("Muestras por Mes y Estatus"),
              plotlyOutput("grafico_barras")
            )),
  
  nav_panel("Tabla de Registros", 
            card(
              DT::dataTableOutput("tabla_datos")
            ))
)

server <- function(input, output, session) {
  
  # Usando el nombre de tu dataframe cargado
  datos_filtrados <- reactive({
    
    req(input$tipo_finca_sel)
    
    df <- df_trimestres %>%
      filter(
        `FECHA DE RECOLECCION DE LA MUESTRA` >= input$rango_fechas[1],
        `FECHA DE RECOLECCION DE LA MUESTRA` <= input$rango_fechas[2],
        `Estatus CV` %in% input$estatus_sel
      )
    
    if (!("Todos" %in% input$tipo_finca_sel)) {
      df <- df %>%
        filter(`Tipo finca` %in% input$tipo_finca_sel)
    }
    
    df
  })
  
  datos_GBG <- reactive({
    Casos_GBG_internos_honduras %>%
      filter(FECHA >= input$rango_fechas[1],
             FECHA <= input$rango_fechas[2])
  })
  
  output$mapa_cv <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$OpenStreetMap.HOT) %>%
      
      setView(lng = -87.10318,lat = 13.31479, zoom = 7.5)%>%
      addPolygons(data = Departamentos_Honduras, 
                  color = "firebrick", 
                  weight = 1.5,
                  fillColor = ~pal_deptos(`N casos GBG`), 
                  fillOpacity = 0.2,
                  label = ~paste("Depto:",`DEPTO`,
                                 "<br>Num casos GBG:", `N casos GBG`)%>%
                    lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-size" = "13px")
                  ),
                  group = "Departamentos") %>%
      addPolylines(data = honduras_recortado,
                   color = "blue",
                   weight = 0.6,
                   group = "Hidrografia")%>%
      addCircles(data = Establecimientos_bovinos_choluteca_y_valle,
                 lng = ~`Long final`,
                 lat = ~`Lat final`,
                 radius = 120,
                 label = ~paste("Nombre granja:",`Nombre`)%>%
                   lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-size" = "13px") 
                 ),
                 color = "#79CDCD",
                 fillColor = "#79CDCD",
                 opacity = 0.9,
                 fillOpacity = 0.6,
                 group = "Granjas bovinas")%>%
      addCircles(data = fincas_camaroneras_general,
                 lng = ~`Longitud`,
                 lat = ~`Latitud`,
                 radius = 120,
                 label = ~paste("CUE:",`CUE`,
                                "<br>Nombre finca:", `ESTABLECIMIENTO`,
                                "<br>Hectareas:", `Ha.`)%>%
                   lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-size" = "13px")
                 ),
                 color = "orange",
                 fillColor = "orange",
                 opacity = 0.9,
                 fillOpacity = 0.6,
                 group = "Fincas camaroneras general")%>%
      addLegend(position = "bottomleft", 
                colors = colores_honduras_ley,
                labels = names(colores_honduras_ley),
                title = ('<div style="font-size: 13px; color: black;">Colores puntos</div>'),
                opacity = 1)%>%
      addLegend(
        data = Departamentos_Honduras,
        pal = pal_deptos,
        values = ~`N casos GBG`,
        opacity = 0.7,
        title = ('<div style="font-size: 13px; color: black;">Casos GBG por Depto</div>'),
        position = "bottomleft")%>%
      addControl(
        html = '<div style="background: white; padding: 5px; border: 1px solid #999;
                      font-size: 12px; max-width: 600px;">
            <strong>Capas disponibles:</strong><br></div>',
        position = "topright"
      )%>%
      addLayersControl(
        overlayGroups = c("Departamentos","Casos gusano barrenador",
                          "Hidrografia","Granjas bovinas","Fincas muestreadas",
                          "Fincas camaroneras general"),  # Listado de capas controlables
        options = layersControlOptions(collapsed = FALSE),  # Expandido por defecto
        position = "topright"
      )%>%
      hideGroup("Casos gusano barrenador")%>%
      hideGroup("Hidrografia")%>%
      hideGroup("Granjas bovinas")%>%
      hideGroup("Fincas camaroneras general")%>%
      addScaleBar(position = "bottomright", options = scaleBarOptions())%>%
      addMeasure(
        position = "topright",
        primaryLengthUnit = "meters",
        secondaryLengthUnit = "kilometers",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7BE0AD",
        localization = "es"
      )%>%
      htmlwidgets::onRender("
      function(el, x) {
        L.control.zoom({
          position: 'bottomright'
        }).addTo(this);
      }
    ")
  })
  
  observe({
    df <- datos_filtrados() %>%
      arrange(`Estatus CV` == "Presencia CV")
    
    if(nrow(df) == 0) {
      leafletProxy("mapa_cv") %>% clearGroup("Fincas muestreadas")
      return()
    }
    
    colores <- ifelse(df$`Estatus CV` == "Presencia CV", "red", "green")
    
    leafletProxy("mapa_cv", data = df) %>%
      clearGroup("Fincas muestreadas") %>%
      addCircles(lng = ~`Longit final`, lat = ~`Latitud final`,
                 radius = 120, 
                 color = colores,
                 opacity = 0.9,
                 fillOpacity = 0.7,
                 group = "Fincas muestreadas",
                 label = ~paste0("<strong>Establecimiento:</strong> ", Establecimiento, 
                                 "<br><strong>Tipo:</strong> ", `Tipo de establecimiento`,
                                 "<br><strong>Estatus:</strong> ", `Estatus CV`,
                                 "<br><strong>CUE:</strong> ", `CUE`,
                                 "<br><strong>Tipo finca:</strong> ", `Tipo finca`) %>% lapply(HTML))
  })
  
  observe({
    df2 <- datos_GBG()
    
    leafletProxy("mapa_cv", data = df2) %>%
      clearGroup("Casos gusano barrenador") %>%
      addCircles(
        lng = ~LONGITUD,
        lat = ~LATITUD,
        radius = ~sqrt(`SUSCEPTIBLES`) * 100, 
        color = "#B23AEE",
        opacity = 0.9,
        fillOpacity = 0.4,
        group = "Casos gusano barrenador",
        label = ~paste("Especie:",`ESPECIE`,
                       "<br>Fecha:", `FECHA`,
                       "<br>Num. susceptibles:", `SUSCEPTIBLES`,
                       "<br>Obs:", `COMENTARIO`)%>%
          lapply(HTML)
      )
  })
  
  output$grafico_barras <- renderPlotly({
    df_grafico <- datos_filtrados() %>%
      mutate(Mes = format(`FECHA DE RECOLECCION DE LA MUESTRA`, "%Y-%m")) %>%
      group_by(Mes, `Estatus CV`) %>%
      summarise(Conteo = n(), .groups = 'drop')
    
    p <- ggplot(df_grafico, aes(x = Mes, y = Conteo, fill = `Estatus CV`)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Presencia CV" = "red", "Ausencia CV" = "green")) +
      theme_minimal() +
      labs(x = "Mes", y = "Cantidad de Muestras") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$tabla_datos <- DT::renderDataTable({
    datos_filtrados()
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  output$conteo_total <- renderText({ paste("Total de muestras:", nrow(datos_filtrados())) })
  output$conteo_pos <- renderText({ paste("Positivos (Presencia):", nrow(filter(datos_filtrados(), `Estatus CV` == "Presencia CV"))) })
  output$conteo_neg <- renderText({ paste("Negativos (Ausencia):", nrow(filter(datos_filtrados(), `Estatus CV` == "Ausencia CV"))) })
}

shinyApp(ui, server)