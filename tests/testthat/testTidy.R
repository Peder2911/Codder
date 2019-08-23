# ================================================
# Tidy spec, meant to succeed ====================
# ================================================
tidyspecscript <- '
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
# ================================================
# Messy spec, meant to fail ======================
# ================================================
# Won't work, because no column has unique values
# for each row, functioning as an ID column.

messyspecscript <- '
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
         a: num 
         b: num 
         c: num 
---
suppressPackageStartupMessages(library(dplyr))

function(data){
   data.frame(a = rep(1,6),
              b = rep(c(1,2),3),
              c = rep(c(1,2,3),2))
}
'

# ================================================
# ================================================
# ================================================
messyfile <- tempfile()
tidyfile <- tempfile()
setup({
       writeLines(messyspecscript,messyfile)
       writeLines(tidyspecscript,tidyfile)})
teardown({unlink(messyfile)
          unlink(tidyfile)})

test_that("Succeeds with ID column",{
   somedata <- tibble::tibble(a = c('a','b','c'),
                                 b = 1:3,
                                 c = 4:6)
   tidyspec <- codder::readSpec(tidyfile)
   expect_error(tidyTransform(somedata,tidyspec),NA)
})

test_that("Fails without ID column",{
   somedata <- tibble::tibble(a = c('a','b','c'),
                                 b = 1:3,
                                 c = 4:6)
   messyspec <- codder::readSpec(messyfile)
   expect_error(tidyTransform(somedata,messyspec),"Data has no ID column")
})
