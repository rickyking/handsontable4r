#' @import htmlwidgets
#' @export
handsontable4r <- function(data,
                           width = NULL,
                           height = NULL,
                           readonly = TRUE,
                           digits = 3) {
  col_typs <- get_col_types(data)

  cols = lapply(seq_along(col_typs), function(i) {
    type = col_typs[i]
    if (type == "factor") {
      res = list(
        type = "dropdown",
        source = levels(data[, i]),
        allowInvalid = FALSE
      )
    } else if (type == "numeric") {
      res = list(type = "numeric",
                 numericFormat = list(pattern = "0.00"))
    } else if (type == "integer") {
      res = list(type = "numeric",
                 numericFormat = list(pattern = "0"))
    } else if (type == "date") {
      res = list(type = "date",
                 correctFormat = TRUE,
                 dateFormat = "YYYY/MM/DD")
    } else {
      res = list(type = type)
    }
    res$readOnly = readonly
    # res$renderer = htmlwidgets::JS("customRenderer")
    # res$default = NA
    res
  })



  x = list(
    data = jsonlite::toJSON(
      data,
      na = "null",
      rownames = FALSE,
      digits = digits
    ),
    colHeaders =  colnames(data),
    columns = cols

  )
  # create the widget
  htmlwidgets::createWidget("handsontable4r", x, width = width, height = height)
}


#' @export
handsontable4rOutput <-
  function(outputId,
           width = "100%",
           height = "400px") {
    shinyWidgetOutput(outputId, "handsontable4r", width, height, package = "sigma")
  }
#' @export
renderHandsontable4r <-
  function(expr,
           env = parent.frame(),
           quoted = FALSE) {
    if (!quoted) {
      expr <- substitute(expr)
    } # force quoted
    shinyRenderWidget(expr, sigmaOutput, env, quoted = TRUE)
  }
