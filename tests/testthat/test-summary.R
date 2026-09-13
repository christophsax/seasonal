# summary() and its print and as.data.frame methods

test_that("summary() builds the coefficient matrix", {
  skip_if_no_x13()

  s <- summary(test_model("regressors"))

  expect_s3_class(s, "summary.seas")
  expect_true(is.matrix(s$coefficients))
  expect_equal(
    colnames(s$coefficients),
    c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
  )

  # the z value is the estimate over its standard error
  expect_equal(
    unname(s$coefficients[, "z value"]),
    unname(s$coefficients[, "Estimate"] / s$coefficients[, "Std. Error"]),
    tolerance = 1e-8
  )
})

test_that("summary() carries the model diagnostics", {
  skip_if_no_x13()

  m <- test_model("seats")
  s <- summary(m)

  expect_equal(s$transform.function, transformfunction(m))
  expect_equal(s$nobs, nobs(m))
  expect_equal(s$bic, BIC(m))
  expect_length(s$qsv, length(qs(m)[1, ]))
})

test_that("print() of a summary shows the coefficients and the diagnostics", {
  skip_if_no_x13()

  out <- capture.output(print(summary(test_model("regressors"))))

  expect_match(out, "Coefficients:", all = FALSE)
  expect_match(out, "SEATS adj.|X-11 adj.|adj\\.", all = FALSE)
  expect_match(out, "Box-Ljung", all = FALSE)
})

test_that("print() of a summary takes the stats option", {
  skip_if_no_x13()

  m <- test_model("seats")

  out <- capture.output(print(summary(m, stats = c("aicc", "bic"))))
  expect_true(length(out) > 0)

  expect_no_error(capture.output(print(summary(m, stats = NULL))))
})

test_that("as.data.frame() of a summary is one row per coefficient", {
  skip_if_no_x13()

  m <- test_model("regressors")
  df <- as.data.frame(summary(m))

  expect_s3_class(df, "data.frame")
  expect_equal(
    colnames(df),
    c("term", "estimate", "std.error", "statistic", "p.value")
  )
  expect_equal(nrow(df), length(coef(m)))
  expect_equal(df$estimate, unname(coef(m)), tolerance = 1e-8)
})

test_that("as.data.frame() of a summary without coefficients is NULL", {
  skip_if_no_x13()

  s <- summary(test_model("seats"))
  s$coefficients <- NULL

  expect_null(as.data.frame(s))
})
