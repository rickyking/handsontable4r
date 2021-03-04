library(handsontable4r)
library(shiny)

ui <- fluidPage(
  titlePanel("Handsontable"),
  sidebarLayout(
    sidebarPanel(
      helpText("Handsontable demo output.")
    ),
    mainPanel(
      handsontable4rOutput("hot", width = 350)
    )
  )
)


server <- function(input, output, session) {
  # this caching step is no longer necessary
  # it was left as an example
  values = reactiveValues()

  data = reactive({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]]))
        DF = data.frame(val = 1:10, bool = TRUE, nm = LETTERS[1:10],
                        dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                        stringsAsFactors = F)
      else
        DF = values[["DF"]]
    }

    values[["DF"]] = DF
    DF
  })

  output$hot <- renderHandsontable4r({
    DF = data()
    if (!is.null(DF))
      handsontable4r(DF)
  })
}



shinyApp(ui, server)