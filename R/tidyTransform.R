#' tidyTransform
#'
#' Transform a table using a spec, specifying how the
#' data is to be transformed, and what assumptions should
#' be fulfilled.
#'
#' @param data data-table or tibble
#' @param spec a list with names header and fun 
#' @return A transformed tibble 
#' @examples
#' mtcars_fixed <- safeTransform(mtcars, 'specs/mtcars.R')
#' @export

tidyTransform <- function(data,spec){
   # ====================================================
   delimiter <- strrep('*',51)
   writeLines(delimiter)

   rawSchema <- spec$header$rawSchema
   validateData(data,rawSchema, type = 'input')

   # ====================================================

   # Transform
   writeLines('Transforming data...')
   data <- spec$fun(data)
   writeLines('Success!')

   # ====================================================
   writeLines(delimiter)

   tidySchemata <- spec$header$tidySchemata
   sapply(tidySchemata, validateData, data = data, type = 'output')
   writeLines('Success!')
   data
}
