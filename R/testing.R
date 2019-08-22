
#' testEqual
#' 
#' A verbose function that checks if x and y are equal 
#' @param x A vector
#' @param y A vector
#' @param msg What is being tested
#' @return TRUE if passed, FALSE if not
#' @examples
#' testEqual(c(1,2,3),c(1,2,3),msg = 'simple test')

testEqual <- function(x,y,msg = 'something'){
   writeLines(paste0('*** Testing ',msg))
   res <- x == y
   if(!all(res)){
      failed  <- paste0(x[!res], ' - ', y[!res])
      stop(paste0('Failed:', failed, sep = '\n'))
   } else {
      writeLines('Passed!')
   }
}

#' testColumns
#' 
#' Perform tests on the columns of a data frame, to see if
#' it corresponds with a given schema
#' @param data A data frame
#' @param data A schema 
#' @return TRUE if passed, FALSE if not 

testColumns <- function(data,schema){
   # Test classes
   tests <- list(testEqual(sapply(data,class),schema$columns,'column types'),
                 testEqual(names(data),names(schema$columns),'column names'))
   all(unlist(tests))
}

#' testSchema
#' 
#' Apply lots of tests to a data frame 
#' @param data A data frame
#' @param data A schema 
#' @return TRUE if passed, FALSE if not
#' @importFrom magrittr "%>%"

testSchema <- function(data,schema){
   sapply(list(testColumns),function(test){
      do.call(test,list(data,schema))    
   }) 
}
