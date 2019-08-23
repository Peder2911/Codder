
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

ao <- codder::readSpec(f)

test_that("fails with wrong raw types",{
   wrongtypes <- tibble::tibble(a = 1:3, b = 1:3, c = 4:6)
   expect_error({tidyTransform(wrongtypes,ao)},"Character vectors are not equal")
})
test_that("fails with wrong raw names",{
   wrongnames <- tibble::tibble(a = c('a','b','c'), e = c(1,2,3), c = c(4,5,6))
   expect_error({tidyTransform(wrongnames,ao)},"Character vectors are not equal")
})
test_that("fails with missing columns",{
   missescolumn <- tibble::tibble(a = c('a','b','c'), b = c(1,2,3))
   expect_error({tidyTransform(missescolumn,ao)},"Character vectors are not equal")
})
