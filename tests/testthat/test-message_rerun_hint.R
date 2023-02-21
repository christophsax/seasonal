test_that("message_rerun_hint works", {
  call <- quote(seas(x))

  expect_message(
    message_rerun_hint(call, list(foo = "bar")),
    "seas\\(x, foo = \"bar\"\\)"
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
