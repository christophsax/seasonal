test_that("ext_ser_call()", {
  
  pp <- list(
    title = "abc",
    start = 2014.01,
    period = 12,
    save = c("a1", "b1"),
    print = "brief",
    format = "datevalue",
    name = "xyz",
    file = "file.dat"
  )
  
  expect_snapshot(ext_ser_call(pp, "x"))  
  
})

test_that("ext_ser_call() defaults to format \"free\"", {
  
  pp <- list(
    title = "abc",
    start = 2014.01,
    period = 12,
    save = c("a1", "b1"),
    print = "brief",
    name = "xyz",
    file = "file.dat"
  )
  
  expect_snapshot(ext_ser_call(pp, "x"))
  
})
