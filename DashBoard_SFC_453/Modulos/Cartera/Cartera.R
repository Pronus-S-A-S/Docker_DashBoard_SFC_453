
source('Modulos/Cartera/funciones_cartera.R', encoding = 'UTF-8')
library(dplyr)

CarteraUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel(h1("Distribución de Cartera", align = "center")),
    hr(),
    fluidRow(h2("Seleccione los inputs", align = "center")),
    fluidRow(
      column(4, pickerInput(ns("tipo_entidad"),h3("Tipo Entidad", align = "center"),
                            choices= as.character(as.numeric(fn_entidades(df_datos_sfc))),
                            selected = as.character(as.numeric(fn_entidades(df_datos_sfc)))[1],
                            options = list(`actions-box` = TRUE),
                            multiple = T), align = "center"),
      column(4, pickerInput(ns("name_entidad"),h3("Nombre Entidad", align = "center"),
                            choices="Banco Caja Social",
                            selected = "Banco Caja Social",
                            options = list(`actions-box` = TRUE),
                            multiple = T), align = "center"),
      column(4,fluidRow(fluidRow(br(),br()),
                        fluidRow(actionButton(ns("buscar"), label = "Buscar",
                                              style="color: #fff; background-color: #FE4902; border-color: #FE4902"),
                                 align = "center")))
    ),
    hr(),
    fluidRow(h2("Gráficas", align = "center")),
    fluidRow(
      column(6, withLoader(plotlyOutput(ns("gph_saldo")), type="image", loader="loader.GIF")),
      tabBox(tabPanel('ICV Saldo',withLoader(plotlyOutput(ns("gph_areas")),type="image", loader="loader.GIF")),
                       tabPanel('ICV Porcentaje',withLoader(plotlyOutput(ns("gph_areas_porcentaje")),type="image", loader="loader.GIF")))
    ),
    fluidRow(
      column(6,withLoader(plotlyOutput(ns("gph_dist")),type="image", loader="loader.GIF")),
      column(6,withLoader(plotlyOutput(ns("icv")),type="image", loader="loader.GIF")),
    ),
    fluidRow(h2("Gráficas", align = "center")),
    fluidRow(withLoader(plotlyOutput(ns('ecosistema')),type="image", loader="loader.GIF"))
  )
}

CarteraServer = function(input, output, session) {
 
  observeEvent(input$tipo_entidad, ignoreInit = T,{
    updatePickerInput(session,"name_entidad",choices = fn.nombre_entidad(df_datos_sfc,
                                                                         tipo_entidad1 = input$tipo_entidad))
  })

  output$gph_saldo <- renderPlotly({
    
    input$buscar
    
    dist <- isolate(fn.gph_saldo(df_datos_sfc,
                                 tipo_entidad1 = input$tipo_entidad,
                                 nombre_entidad = input$name_entidad))
    dist
  })
  
  output$gph_dist <- renderPlotly({
    
    input$buscar
    dist <- isolate(fn.gph_dist(df_datos_sfc,
                                tipo_entidad1 = input$tipo_entidad,
                                nombre_entidad = input$name_entidad))
    dist
  })
  output$gph_areas <- renderPlotly({
    
    input$buscar
    dist <- isolate(fn.gph_areas(df_datos_sfc,
                                tipo_entidad1 = input$tipo_entidad,
                                nombre_entidad = input$name_entidad))
    dist
  })
  
  output$gph_areas_porcentaje <- renderPlotly({
    
    input$buscar
    dist <- isolate(fn.gph_areas_per(df_datos_sfc,
                                 tipo_entidad1 = input$tipo_entidad,
                                 nombre_entidad = input$name_entidad))
    dist
  })
  
  output$icv <- renderPlotly({
    
    input$buscar
    
    dist <- isolate(fn.icv.per(df_datos_sfc, input$tipo_entidad, input$name_entidad))
    
    dist
    
  })
  
  output$ecosistema <- renderPlotly({
    
    fn.ecosistema(df_datos_sfc)
    
  })
   
}

