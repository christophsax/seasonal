test_that("parse_spc accounts for uppercase arguments and spec", {
  
  text <- '
    SERIES{
    TITLE = "xyz"
    START = 2014.01
    PERIOD = 12
    SAVE = (A1 B1)
    PRINT = BRIEF
    NAME = "abc"
    FILE = "path/to/file.dat"
  }'
  
  expect_snapshot(parse_spc(text))
  
  pp.cap <- parse_spc(text)
  expect_false(is.null(pp.cap[['series']][['file']]))
  
})
