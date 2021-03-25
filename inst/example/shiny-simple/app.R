library(handsontable4r)
library(shiny)
library(shinydashboard)

# handsontable4r(data.frame(x = seq(1, 10), y = seq(11, 20)))
min_color <- list(pch = ".", cex = 4, col = "red")
max_color <- list(pch = ".", cex = 4, col = "green")
spline_height <- 50
spline_width <- 300
df <- readRDS("../dftbl.rds") %>%
  rowwise() %>%
  mutate(
    `Trend 1M` = kableExtra::spec_plot(
      `Trend 1M`,
      min = min_color,
      max = max_color,
      height = spline_height,
      res = 300,
      width = spline_width
    )[["svg_text"]],
    `Trend Rel 1M` = kableExtra::spec_plot(
      `Trend Rel 1M`,
      min = min_color,
      max = max_color,
      height = spline_height,
      res = 300,
      width = spline_width
    )[["svg_text"]],
    `Volume` = kableExtra::spec_plot(
      `Volume`,
      min = min_color,
      max = max_color,
      polymin = 0,
      height = spline_height,
      res = 300,
      width = spline_width
    )[["svg_text"]],
    `ETF Weight` = `ETF Weight`/100
  )


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(handsontable4rOutput("hot", width = "100%", height = "1000px")),

      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
  )
)

server <- function(input, output) {
  output$hot <- renderHandsontable4r({
      handsontable4r(df %>% select(-ticker),
                     rownames = df$ticker,
                     rowHeaderWidth = 100,
                     fixedColumnsLeft = 1,
                     height = 1000,
                     width = "100%"
                     ) %>%
      hot_heatmap(cols = 2:7,
                  color_scale = c("#f87274", "#FFFFFF", "#63be7b")) %>%
      hot_col(col = c("Last", "TrdVal. Avg 1M", "TrdVal. Min 1M"), format = "$0,000.00") %>%
      hot_col(
        col = c("Trend 1M", "Trend Rel 1M", "Volume"),
        renderer = htmlwidgets::JS("Handsontable.renderers.HtmlRenderer")
      ) %>%
      hot_col("ETF Weight", format = "0.00%")
  })
}

shinyApp(ui, server)
