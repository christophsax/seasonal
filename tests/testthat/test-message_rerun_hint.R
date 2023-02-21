test_that("message_rerun_hint works", {
  call <- quote(seas(x))

  expect_message(
    message_rerun_hint(call, list(foo = "bar")),
    "seas\\(x, foo = \"bar\"\\)"
  )
})

test_that("message_rerun_hint with multiple dots", {
  call <- quote(seas(x))

  expect_message(
    message_rerun_hint(call, list(foo = "bar", baz = 3)),
    "seas\\(x, baz = 3, foo = \"bar\"\\)"
  )
})

test_that("message_rerun_hint consolidates dots with same name", {
  call <- quote(seas(x))

  expect_message(
    message_rerun_hint(
      call,
      list(foo = "bar", foo = "baz")
    ),
    "seas\\(x, foo = c\\(\"bar\", \"baz\"\\)\\)"
  )
})

test_that("message_rerun_hint preserves existing args", {
  call <- quote(seas(x, foo = "yes"))

  expect_message(
    message_rerun_hint(
      call,
      list(foo = "no")
    ),
    "seas\\(x, foo = c\\(\"no\", \"yes\"\\)\\)"
  )
})
