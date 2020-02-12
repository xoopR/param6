context("paramset")

test_that("constructor",{
  expect_error(paramset$new(), "Need")
  expect_error(paramset$new(a))
  expect_error(paramset$new(a = "1"))
  expect_silent(paramset$new(a = Set$new()))
  expect_silent(paramset$new(a = Set$new(1) ~ 1))
  expect_error(paramset$new(a = Set$new(1) ~ 2), "Assertion")
  expect_silent(paramset$new(a = Set$new(1), b = Set$new(2)))
  expect_silent(paramset$new(a = Set$new(1) ~ 1, b = Set$new(2)))
  expect_silent(paramset$new(a = Set$new(1) ~ 1, b = Set$new(2) ~ 2))
})

p = paramset$new(ntrees = PosIntegers$new(),
                 splitrule = Set$new("logrank","extratrees","C","maxstat") ~ "logrank",
                 sample.fraction = Interval$new(0, 1) ~ 0.5)

test_that("values",{
  expect_equal(p$values$ntrees, NA)
  expect_equal(p$values$sample.fraction, 0.5)
  expect_silent({p$values$ntrees = 1})
  expect_equal(p$values$ntrees, 1)
  expect_silent({p$values = list(splitrule = "C", sample.fraction = 0.1, ntrees = 2)})
  expect_error({p$values$ntrees = -5}, "doesn't lie in")
})

test_that("params",{
  expect_class(p$params, "data.table")
  expect_error({p$params = "a"})
})
