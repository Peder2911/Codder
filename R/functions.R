
#' readDispatch
#' 
#' Read a file according to its extension 
#' @param file A file with an extension
#' @return a data-table or R object 
#' @examples
#' data <- readDispatch('somedata.csv')

readDispatch <- function(file){
   disp <- list(
      rds = readRDS,
      csv = read.csv,
      xlxs = readxl::read_xlsx)
   ext <- tools::file_ext(file)
   disp[[ext]](file)
}

#' getAlterFile
#' 
#' Reads a clade object from a procedure file in a directory
#' The file is chosen from filename.
#' @param dir The directory to look for the file
#' @return a list with names header and fun 
#' @examples
#' mtcarsAlter <- getCladeFile("clades","mtcars")
#' @export

getAlterFile <- function(dir,clade){
   cladefiles <- list.files(dir,full.names = TRUE) 
   cladefilenames <- gsub('.*/','',cladefiles)
   if(any(grepl(clade,cladefilenames))){
      cladefile <- cladefiles[grepl(clade,cladefilenames)][1]
   } else {
      stop(paste0('Unknown clade: ', clade))
   }

   getAlterObject(cladefile)
}

#' getAlterObject
#' 
#' Get an alter object from a file, separating and reading the yaml header,
#' and sourcing the r code with dget. 
#' @param file The file to read
#' @return A list with names header and fun
#' @importFrom magrittr "%>%"
#' @examples
#' getAlter("mtcars.R") 
#' @export

getAlterObject <- function(file){
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
