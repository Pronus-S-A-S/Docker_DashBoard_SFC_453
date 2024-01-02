# Librerías ---------------------------------------------------------------

rm(list=ls())
library(shinyWidgets)
library(tidyr)
library(plotly)
library(ggplot2)
library(shiny)
library(shinymanager)
library(timetk)
library(lubridate)
library(rsconnect)
library(DT)
library(shinythemes)
# library(maptools)
library(sp)
library(tmap)
library(treemap)
library(shinydashboard)
library(tidyverse)
library(AzureStor)
library(readxl)
library(bslib)
library(shiny)
library(htmltools)
library(plotly)
library(leaflet)
library(shinycustomloader)
library(htmlwidgets)
library(shinyWidgets)
library(dplyr)
library(nycflights13)
library(histoslider)
library(rlang)
library(rmarkdown)
# library(d3treeR)
library(plotly)
library(data.table)
library(networkD3)
library(scales)
library(timetk)


options(scipen = 999)


# Funciones de la sesión --------------------------------------------------

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();" ## Como gestionar el cierre de sesión del aplicativo por inactividad

credentials = data.frame(
  user = c('1',"Finamco"),
  password = c('1',"Finamco2023*"), 
  stringsAsFactors = FALSE
)

shinymanager::set_labels(
  language = "en",
  "Please authenticate" = "",
  "Username:" = "Nombre de Usuario:",
  "Password:" = "Contraseña:",
  "Login" = "Iniciar sesión",
  "Username or password are incorrect" = "El usuario o contraseña son incorrectos") ## Manejo del inicio de sesión



# Cargue Datos ------------------------------------------------------------


bl_endp_key <- storage_endpoint("https://pronuscontrolprdstgeacct.blob.core.windows.net",
                                sas="?sv=2022-11-02&ss=bfqt&srt=sco&sp=rwdlacupiytfx&se=2024-12-31T21:33:43Z&st=2024-01-02T13:33:43Z&spr=https&sig=IKG%2F8%2BABd5LJTiUQtcCbdcwNwcl79YgUodPyQ5tOYiE%3D")
cont <- storage_container(bl_endp_key, "rcp")

fn.load_data = function(dir, file_name){
  full_dir = paste(dir,file_name,sep = "/")
  fname <- tempfile()
  storage_download(cont,full_dir,fname,overwrite=T)
  list2env(readRDS(fname),envir = .GlobalEnv)

}

fn.load_data(dir = "Datos_SFC/F_453",
             file_name = "list_Data_RCP.rds")


# Cargue de Datos ---------------------------------------------------------


color_pronus = colorRampPalette(c("#A21824","#E7B040","#FE4902",
                                  "#E7E6E6","#9A9A9A"))

pronus_theme = theme(
  panel.background = element_rect(fill = "transparent",
                                  colour = NA_character_), # necessary to avoid drawing panel outline
  panel.grid.major = element_blank(), # get rid of major grid
  panel.grid.minor = element_blank(), # get rid of minor grid
  plot.background = element_rect(fill = "transparent",
                                 colour = NA_character_), # necessary to avoid drawing plot outline
  legend.background = element_rect(fill = "transparent"),
  legend.key = element_rect(fill = "transparent"),
  plot.title = element_text(hjust = 0.5, colour = "#A21824", size = 18),
  plot.subtitle = element_text(hjust = 0.5),
  legend.title = element_text(hjust = 0.5, colour = "#A21824", size = 14),
  legend.text = element_text(colour = "#404040"),
  axis.text = element_text(colour = "#707070"),
  text = element_text(colour = "#707070"),
  axis.title.x = element_text(size=14,family = ),
  axis.title.y = element_text(size=14),
  legend.position = "right",
)


# Parametros --------------------------------------------------------------


# Funciones ---------------------------------------------------------------

source('Modulos/Home.R', encoding = 'UTF-8')
source('Modulos/Cartera/Cartera.R', encoding = 'UTF-8')

# Aplicación --------------------------------------------------------------

ui <- secure_app(
  head_auth = tags$script(inactivity),
  fluidPage(
    titlePanel("",windowTitle = "Dash SFC"),
    tags$head(
      includeCSS("www/CSS.css"),
      tags$style(
        HTML("#dashboard{margin-bottom:50px;}")
      )),
    
    headerPanel(title=tags$a(
      fluidRow(
        column(4),
        column(4, tags$a(href='https://pronus.co/', tags$img(src='logo_pronus_portada.png', height = 85), target="_blank"),align = "center"),
        column(4))),
      br()),
    fluidRow(br()),
    navbarPage(
      title = span("Dashboard - SFC", style = "color:white"),
      id = "Actual",
      tabPanel(icon("home"),value = 'home', HomeUI('home')),
      tabPanel("Cartera", CarteraUI('Cartera_SFC'))
      )
  ),
  
  tags_top = tags$div(
    tags$head(includeCSS("www/CSS.css")),
    tags$img(
      src = 'logo_horizontal-01.png',
      height = 131,
      width = 250
    )),
  
  tags_bottom = tags$div(
    tags$p(
      "Para cualquier pregunta, por favor contactar",
      tags$a(
        href = "mailto:consultascontrol@pronus.co",
        target="_top", "Equipo de Data y Analítica"
      ))) ## Acá cae la imagen visual de la página de carga
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  
  observeEvent(input$link_to_453, {
    updateTabsetPanel(session, "Actual", "Cartera")
  })

  callModule(CarteraServer, 'Cartera_SFC')
}

shinyApp(ui = ui, server = server)

