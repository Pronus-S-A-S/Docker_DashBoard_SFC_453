fecha_corte = Sys.Date()

HomeUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel(h1("Dashboard Superfinanciera", align = "center")),
    br(),
    fluidRow(
      column(4,
             wellPanel(fluidRow(h2(
               actionLink(
                 "link_to_453",
                 h2("Distribución Cartera", align = "center")
               )
             ),
             br(),
             p(paste0('Corte:',fecha_corte), align = "center")
             ),
             style = "padding: 45px;")
      )),
    fluidRow(br()),
    fluidRow(
      column(3),
      column(6,tags$div(
        tags$p(
          "Para cualquier pregunta, por favor contactar",
          tags$a(
            href = "mailto:consultascontrol@pronus.co",
            target="_top", "Equipo de Data y Analítica"
          )))),
      column(3)),
    fluidRow(br()),
  )
}

## Prueba push