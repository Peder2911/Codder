
f <- tempfile()
alterfile <- '
---

title: testscript
rawSchema:
   columns:
      a: char
      b: num
      c: num
tidySchemata:
   a: 
      columns:
         a: char
         bc: num
---
suppressPackageStartupMessages(library(dplyr))

function(data){
   mutate(data,
          bc = b + c) %>%
   select(a,bc)
}
'
writeLines(alterfile,f)
test_that("can read alter-object", {
   expect_failure(expect_error({ao <- codder::readSpec(f)}))
})

ao <- codder::readSpec(f)

test_that("reading alter stuff", {
   expect_equal(length(ao),2)
   expect_equal(names(ao),c('header','fun'))
   expect_true(all(c('title','rawSchema','tidySchemata') %in% names(ao$header)))
})

test_that("succeeds with good data",{
   gooddat <- tibble::tibble(a = c('a','b','c'), b = c(1,2,3), c = c(4,5,6))
   expect_error(tidyTransform(gooddat,ao), NA)
})
