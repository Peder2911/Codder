#' safeTransform
#'
#' Transform a table using a cladefile, specifying how the
#' data is to be transformed, and what assumptions should
#' be fulfilled.
#'
#' @param data data-table or tibble
#' @param alterlist a list with names header and fun 
#' @return A transformed tibble 
#' @examples
#' mtcars_fixed <- safeTransform(mtcars, 'clades/mtcars.R')
#' @export

safeTransform <- function(data,alterlist){
   delimiter <- strrep('*',51)

   writeLines(delimiter)

   rawSchema <- alterlist$header$rawSchema

   testSchema(data,rawSchema)

   # ====================================================

   # Transform
   writeLines('Transforming data...')
   data <- alterlist$fun(data)
   writeLines('Success!')

   # ====================================================
   writeLines(delimiter)

   tidySchemata <- alterlist$header$tidySchemata
   writeLines(paste0('Tidy schema(ta):\n',yaml::as.yaml(tidySchemata)))

   sapply(tidySchemata, testSchema, data = data)

   writeLines('Success!')

   data
}

