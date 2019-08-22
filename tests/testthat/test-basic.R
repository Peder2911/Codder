
f <- tempfile()
alterfile <- '
---

title: testscript
rawSchema:
   columns:
      a: factor 
      b: numeric 
      c: numeric 
tidySchemata:
   a: 
      columns:
         a: factor 
         bc: numeric 
---
suppressPackageStartupMessages(library(dplyr))

function(data){
   mutate(data,
          bc = b + c) %>%
   select(a,bc)
}
'
writeLines(alterfile,f)

test_that("reading alter stuff", {
   ao <- codder::getAlterObject(f)
   expect_equal(length(ao),2)
   expect_equal(names(ao),c('header','fun'))
   expect_true(all(c('title','rawSchema','tidySchemata') %in% names(ao$header)))
})

test_that("succeeds with good data",{
   ao <- codder::getAlterObject(f)
   gooddat <- data.frame(a = c('a','b','c'), b = c(1,2,3), c = c(4,5,6))
   expect_failure(expect_error({safeTransform(gooddat,ao)}))
})

test_that("fails with bad data",{
   ao <- codder::getAlterObject(f)
   wrongtypes <- data.frame(a = c('a','b','c'), b = c(1,2,3), c = c(4,5,6))
   wrongnames <- data.frame(a = factor(c('a','b','c')), e = c(1,2,3), c = c(4,5,6))
   missescolumn <- data.frame(a = factor(c('a','b','c')), b = c(1,2,3))
   expect_error({safeTransform(wrongtypes,ao)})
   expect_error({safeTransform(wrongnames,ao)})
   expect_error({safeTransform(missescolumn,ao)})
})
