# Modules for app

leafletDisplay <- function(id, label = "ssp") {
  ns <- NS(id)
  tagList(
    leaflet::leafletProxy("shp_map", data = dataList[[ns()]]),
    verbatimTextOutput(ns("out"))
  )
}

counterServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      count <- reactiveVal(0)
      observeEvent(input$button, {
        count(count() + 1)
      })
      output$out <- renderText({
        count()
      })
      count
    }
  )
}

ui <- fluidPage(
  counterButton("counter1", "Counter #1"),
  counterButton("counter2", "Counter #2")
)

server <- function(input, output, session) {
  counterServer("counter1")
  counterServer("counter2")
}

shinyApp(ui, server)