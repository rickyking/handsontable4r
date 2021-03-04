#' Handsontable widget
#'
#' Add heatmap to table.
#'
#' @param hot rhandsontable object
#' @param cols numeric vector of columns to include in the heatmap. If missing
#'  all columns are used.
#' @param color_scale character vector that includes the lower and upper
#'  colors
#' @param renderer character defining a Javascript function to be used
#'  to determine the cell colors. If missing,
#'  \code{rhandsontable:::renderer_heatmap} is used.
#' @export
hot_heatmap = function(hot, cols, color_scale = c("#ED6D47", "#FFFFFF", "#17F556"),
                       renderer = NULL) {
  hot$x$isHeatmap = TRUE

  if (is.null(renderer)) {
    renderer = renderer_heatmap(color_scale)
  }

  if (missing(cols))
    cols = seq_along(hot$x$colHeaders)
  for (x in hot$x$colHeaders[cols])
    hot = hot %>% hot_col(x, renderer = renderer)

  hot
}

# Used by hot_heatmap
renderer_heatmap = function(color_scale) {
  renderer = gsub("\n", "", "
      function (instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.NumericRenderer.apply(this, arguments);
        heatmapScale  = chroma.scale(['%s1', '%s2', '%s3']);
        if (instance.heatmap[col]) {
          mn = instance.heatmap[col].min;
          mx = instance.heatmap[col].max;
          mn = -10,
          mx = 10,
          pt = (parseInt(value, 10) - mn) / (mx - mn);
          td.style.backgroundColor = heatmapScale(pt).hex();
        }
      }
      ")
  renderer = gsub("%s1", color_scale[1], renderer)
  renderer = gsub("%s2", color_scale[2], renderer)
  renderer = gsub("%s3", color_scale[3], renderer)
  renderer
}
