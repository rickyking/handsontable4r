# Map R classes to handsontable.js types
get_col_types = function(data) {
  if (is.matrix(data))  {
    types = rep(typeof(data), ncol(data))
  } else if (is.data.frame(data)){
    types = as.character(lapply(data, class))
  } else{
    stop("Unsupported object type: ", class(data), " Can't extract column types.")
  }

  types <- sapply(types, function(type) {
    if (grepl("factor", type)) return("factor")

    switch(type,
           integer="integer",
           double="numeric",
           numeric="numeric",
           character="text",
           logical="checkbox",
           Date="date",
           "text")
  })

  as.character(types)
}
