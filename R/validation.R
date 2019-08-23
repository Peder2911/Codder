# ================================================
# Functions for validating data, both shape,======
# form, types, etc. ==============================
# ================================================
# ================================================

#' datatype 
#' 
#' "Bags" R vector data types (from class) into three bins:
#' num = {integer, numeric}
#' char = {character, factor}
#' bool = {logical}
#' Returns binned data type 
#' 
#' @param x A vector 
#' @return A data type string
#' @examples
#' datatype(c(1,2,3)) == datatype(c(1L,2L,3L))
#' [1] TRUE
#' datatype(c('a','b')) == datatype(factor('a','b'))
#' [1] TRUE

datatype <- function(x){
   bins <- list(num = c('integer','numeric'),
                char = c('character','factor'),
                bool = c('logical'))
   rtype <- class(x)
   names(bins)[sapply(bins, function(b) any(rtype %in% b))]
}

#' validateColumns 
#'  
#' Asserts that: 
#' Columns have the column names specified in the schema
#' Columns have the data types specified in the schema
#' 
#' @param data A data frame
#' @param spec A spec 
validateColumns <- function(data,spec){
   setEqual <- function(x,y){
      print(x)
      print(y)
      all(x == y)
   }
   assertthat::on_failure(setEqual) <- function(call,env){
      msg <- 'Character vectors are not equal!'
      xdiff <- setdiff(eval(call$x), eval(call$y))
      ydiff <- setdiff(eval(call$y), eval(call$x))
      if(length(xdiff) > 0){
         msg <- paste0(msg,' Not in Y: ', paste(xdiff,sep = ', ')) 
      }
      if(length(ydiff) > 0){
         msg <- paste0(msg,' Not in X: ', paste(ydiff,sep = ', ')) 
      }
      msg
   }
   # Test classes
   results <- c(assertthat::assert_that(setEqual(sapply(data,datatype),
                                                 unlist(spec$columns))),
                assertthat::assert_that(setEqual(names(data),names(spec$columns))))
   all(results)
}

#' hasIdColumn 
#'  
#' Asserts that: 
#' One (and only one) of the columns in the data have the same number of unique
#' values as there are rows in the data.
#' 
#' This is to test whether the data has a value that can be used as a primary key.
#' 
#' @param data A data frame
hasIdColumn <- function(data,...){
   hasid <- function(data){
      any(sapply(data, function(var) length(unique(var))) == nrow(data))
   }
   assertthat::on_failure(hasid) <- function(call,env){
      "Data has no ID column!"
   }
   assertthat::assert_that(hasid(data))
}

#' validateData 
#' 
#' Apply lots of tests to a data frame 
#' @param data A data frame
#' @param spec A spec 
#' @param type A Either 'input' or 'output', to reserve some validation for either 
#' @return TRUE if passed, FALSE if not
#' @importFrom magrittr "%>%"
validateData <- function(data,spec,type = 'input'){
   procedures <- list(validateColumns)
   if(type == 'input'){
      procedures <- procedures
   } else if (type == 'output'){
      procedures <- c(procedures, list(hasIdColumn))
   } else {
      stop('You done goofed')
   }

   sapply(procedures,function(procedure){
      do.call(procedure,list(data,spec))    
   }) 
}
