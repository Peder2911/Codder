
#' readSpec 
#' 
#' Get a spec from a file, separating and reading the yaml header, and sourcing
#' the r code with dget. 
#' @param file The file to read
#' @return A list with names header and fun
#' @importFrom magrittr "%>%"
#' @examples
#' readSpec("mtcars.R") 
#' @export

readSpec <- function(file){
   splitScript <- function(lines){
      headerspan <- do.call(seq,as.list(which(lines == '---')))

      list(
         header = lines[headerspan] %>%
            yaml::yaml.load(),
         fun = lines[-headerspan] %>%
            parse(text = .) %>%
            eval()
      )
   }

   lines <- readLines(file)
   splitScript(lines)
}
