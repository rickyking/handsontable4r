#' Handsontable widget
#'
#' Create table
#' @param data data frame
#' @param width the width of widget
#' @param height the height of widget
#' @param readonly if the entire table is readonly
#' @param rownames the rownames
#' @param rowHeaderWidth the width of the header
#' @param digits the digits
#'
#' @import htmlwidgets
#' @export
handsontable4r <- function(data,
                           width = NULL,
                           height = NULL,
                           readonly = TRUE,
                           rownames = NULL,
                           rowHeaderWidth = NULL,
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
    res
  })

  x = list(
    data = jsonlite::toJSON(
      data,
      na = "null",
      rownames = FALSE,
      digits = digits
    ),
    height=height,
    width=width,
    colHeaders =  colnames(data),
    columns = cols,
    dropdownMenu = c(
      'filter_by_condition',
      'filter_operators',
      'filter_by_value',
      'filter_action_bar'
    ),
    filters = TRUE,
    licenseKey = 'non-commercial-and-evaluation',
    rowHeaderWidth = rowHeaderWidth,
    multiColumnSorting = TRUE,
    autoRowSize = TRUE,
    autoColumnSize = TRUE,
    className = "htCenter htMiddle",
    afterGetRowHeader = htmlwidgets::JS("function(col, TH) {
      TH.className = 'htMiddle'
    }")
  )

  if (!is.null(rownames)) {
    x$rowHeaders = rownames
  }

  if (!is.null(rowHeaderWidth)) {
    x$rowHeaderWidth = rowHeaderWidth
  }

  # create the widget
  htmlwidgets::createWidget("handsontable4r", x, width = width, height = height)
}








#' Handsontable widget
#'
#' Configure multiple columns.
#'
#' @param hot rhandsontable object
#' @param colWidths a scalar or numeric vector of column widths
#' @param columnSorting logical enabling row sorting. Sorting only alters the
#'  table presentation and the original dataset row order is maintained.
#'  The sorting will be done when a user click on column name
#' @param manualColumnMove logical enabling column drag-and-drop reordering
#' @param manualColumnResize logical enabline column width resizing
#' @param fixedColumnsLeft a scalar indicating the number of columns to
#'  freeze on the left
#' @param ... passed to hot_col
#' @seealso \code{\link{hot_col}}, \code{\link{hot_rows}}, \code{\link{hot_cell}}
#' @export
hot_cols = function(hot,
                    colWidths = NULL,
                    columnSorting = NULL,
                    manualColumnMove = NULL,
                    manualColumnResize = NULL,
                    fixedColumnsLeft = NULL,
                    ...) {
  if (!is.null(colWidths))
    hot$x$colWidths = colWidths

  if (!is.null(columnSorting))
    hot$x$columnSorting = columnSorting
  if (!is.null(manualColumnMove))
    hot$x$manualColumnMove = manualColumnMove
  if (!is.null(manualColumnResize))
    hot$x$manualColumnResize = manualColumnResize

  if (!is.null(fixedColumnsLeft))
    hot$x$fixedColumnsLeft = fixedColumnsLeft

  for (i in seq_len(length(hot$x$columns)))
    hot = hot %>% hot_col(i, ...)

  hot
}

#' Handsontable widget
#'
#' Configure single column.
#'
#' @param hot rhandsontable object
#' @param col vector of column names or indices
#' @param type character specify the data type. Options include:
#'  numeric, date, checkbox, select, dropdown, autocomplete, password,
#'  and handsontable (not implemented yet)
#' @param format characer specifying column format. See Cell Types at
#'  \href{http://handsontable.com}{Handsontable.js} for the formatting
#'  options for each data type. Numeric columns are formatted using
#'  \href{http://numbrojs.com}{Numbro.js}.
#' @param source a vector of choices for select, dropdown and autocomplete
#'  column types
#' @param strict logical specifying whether values not in the \code{source}
#'  vector will be accepted
#' @param readOnly logical making the column read-only
#' @param validator character defining a Javascript function to be used
#'  to validate user input. See \code{hot_validate_numeric} and
#'  \code{hot_validate_character} for pre-build validators.
#' @param allowInvalid logical specifying whether invalid data will be
#'  accepted. Invalid data cells will be color red.
#' @param halign character defining the horizontal alignment. Possible
#'  values are htLeft, htCenter, htRight and htJustify
#' @param valign character defining the vertical alignment. Possible
#'  values are htTop, htMiddle, htBottom
#' @param renderer character defining a Javascript function to be used
#'  to format column cells. Can be used to implement conditional formatting.
#' @param copyable logical defining whether data in a cell can be copied using
#'  Ctrl + C
#' @param dateFormat character defining the date format. See
#'  \href{https://github.com/moment/moment}{Moment.js} for details.
#' @param default default column value for new rows (NA if not specified; shiny only)
#' @param language locale passed to \href{http://numbrojs.com}{Numbro.js};
#'  default is 'en-US'.
#' @param ... passed to handsontable
#' @seealso \code{\link{hot_cols}}, \code{\link{hot_rows}}, \code{\link{hot_cell}}
#' @export
hot_col = function(hot,
                   col,
                   type = NULL,
                   format = NULL,
                   source = NULL,
                   strict = NULL,
                   readOnly = NULL,
                   validator = NULL,
                   allowInvalid = NULL,
                   halign = NULL,
                   valign = NULL,
                   renderer = NULL,
                   copyable = NULL,
                   dateFormat = NULL,
                   default = NULL,
                   language = NULL,
                   ...) {
  cols = hot$x$columns
  if (is.null(cols)) {
    # create a columns list
    warning(
      "rhandsontable column types were previously not defined but are ",
      "now being set to 'text' to support column properties"
    )
    cols = lapply(hot$x$colHeaders, function(x) {
      list(type = "text")
    })
  }

  for (i in col) {
    if (is.character(i))
      i = which(hot$x$colHeaders == i)

    if (!is.null(type))
      cols[[i]]$type = type
    if (!is.null(dateFormat))
      cols[[i]]$dateFormat = dateFormat
    if (!is.null(source))
      cols[[i]]$source = source
    if (!is.null(strict))
      cols[[i]]$strict = strict
    if (!is.null(readOnly))
      cols[[i]]$readOnly = readOnly
    if (!is.null(copyable))
      cols[[i]]$copyable = copyable
    if (!is.null(default))
      cols[[i]]$default = default

    if (!is.null(format) ||
        !is.null(language))
      cols[[i]]$numericFormat = list()
    if (!is.null(format))
      cols[[i]]$numericFormat$pattern = format
    if (!is.null(language))
      cols[[i]]$numericFormat$culture = language

    if (!is.null(validator))
      cols[[i]]$validator = JS(validator)
    if (!is.null(allowInvalid))
      cols[[i]]$allowInvalid = allowInvalid
    if (!is.null(renderer))
      cols[[i]]$renderer = JS(renderer)

    if (!is.null(list(...)))
      cols[[i]] = c(cols[[i]], list(...))

    className = c(halign, valign)
    if (!is.null(className)) {
      cols[[i]]$className = paste0(className, collapse = " ")
    }
  }

  hot$x$columns = cols
  hot
}



#' Handsontable widget
#'
#' Shiny bindings for handsontable4r
#'
#' @param outputId output variable to read from
#' @param width,height must be a valid CSS unit in pixels
#'  or a number, which will be coerced to a string and have \code{"px"} appended.
#' @seealso \code{\link{renderHandsontable4r}}
#' @export
handsontable4rOutput <-
  function(outputId,
           width = "100%",
           height = "100%") {
    htmlwidgets::shinyWidgetOutput(outputId, 'handsontable4r', width, height,
                                   package = 'handsontable4r')
  }

#' Handsontable widget
#'
#' Shiny bindings for handsontable4r
#'
#' @param expr an expression that generates an handsontable4r
#' @param env the environment in which to evaluate \code{expr}.
#' @param quoted is \code{expr} a quoted expression (with \code{quote()})? This
#'  is useful if you want to save an expression in a variable.
#' @seealso \code{\link{Handsontable4rOutput}}
#' @export
renderHandsontable4r <-
  function(expr,
           env = parent.frame(),
           quoted = FALSE) {
    if (!quoted) {
      expr <- substitute(expr)
    } # force quoted
    htmlwidgets::shinyRenderWidget(expr, handsontable4rOutput, env, quoted = TRUE)
  }


#' Handsontable widget
#'
#' Configure row settings that pertain to the entire table.
#' Note that hot_rows is not to be confused with \code{\link{hot_row}}. See
#' \href{http://handsontable.com}{Handsontable.js} for details.
#'
#' @param hot handsontable4r object
#' @param rowHeights a scalar or numeric vector of row heights
#' @param fixedRowsTop a scaler indicating the number of rows to
#'  freeze on the top
#' @seealso \code{\link{hot_cols}}, \code{\link{hot_cell}}
#' @export
hot_rows = function(hot, rowHeights = NULL, fixedRowsTop = NULL) {
  if (!is.null(rowHeights)) hot$x$rowHeights = rowHeights
  if (!is.null(fixedRowsTop)) hot$x$fixedRowsTop = fixedRowsTop
  hot
}

#' Handsontable widget
#'
#' Configure properties of all cells in a given row(s).
#' Note that hot_row is not to be confused with \code{\link{hot_rows}}.  See
#' \href{http://handsontable.com}{Handsontable.js} for details.
#'
#' @param hot handsontable4r object
#' @param row numeric vector of row indexes
#' @param readOnly logical making the row(s) read-only
#' @seealso \code{\link{hot_cols}}, \code{\link{hot_cell}}, \code{\link{hot_rows}}
#' @export
hot_row = function(hot, row, readOnly = NULL) {
  if ( !is.null(readOnly) ) {
    colDim = hot$x$rDataDim[2]
    for ( i in row ) {
      for ( j in seq_len(colDim) ) {
        hot = hot %>% hot_cell(i, j, readOnly = readOnly)
      }
    }
  }
  hot
}

#' Handsontable widget
#'
#' Configure single cell.  See
#' \href{http://handsontable.com}{Handsontable.js} for details.
#'
#' @param hot handsontable4r object
#' @param row numeric row index
#' @param col column name or index
#' @param comment character comment to add to cell
#' @param readOnly logical making the cell read-only
#' @seealso \code{\link{hot_cols}}, \code{\link{hot_rows}}
#' @export
hot_cell = function(hot, row, col, comment = NULL, readOnly = NULL) {
  if (is.character(col)) col = which(hot$x$colHeaders == col)

  cell = list(row = row - 1, col = col - 1)

  if (!is.null(comment)) cell$comment = list(value = comment)
  if (!is.null(readOnly)) cell$readOnly = readOnly

  hot$x$cell = c(hot$x$cell, list(cell))

  if (!is.null(comment)) hot = hot %>% hot_table(enableComments = TRUE)

  hot
}
