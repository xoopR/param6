context("ParamSet")

test_that("constructor",{
  expect_error(ParamSet$new(), "Need")
  expect_error(ParamSet$new(a))
  expect_error(ParamSet$new("a"), "not an")
  expect_error(ParamSet$new(a = "1"), "not an R6 Set")
  expect_silent(ParamSet$new(a = Set$new()))
  expect_silent(ParamSet$new(a = Set$new(1) ~ 1))
  expect_error(ParamSet$new(a = Set$new(1) ~ 2), "does not lie in")
  expect_silent(ParamSet$new(a = Set$new(1), b = Set$new(2)))
  expect_silent(ParamSet$new(a = Set$new(1) ~ 1, b = Set$new(2)))
  expect_silent(ParamSet$new(a = Set$new(1) ~ 1, b = Set$new(2) ~ 2))
})

p = ParamSet$new(ntrees = PosIntegers$new(),
                 splitrule = Set$new("logrank","extratrees","C","maxstat") ~ "logrank",
                 sample.fraction = Interval$new(0, 1) ~ 0.5)

test_that("values",{
  expect_silent({p$values$asas = 1})
  expect_silent({p$values = list(ntrees = 1, basdsd = 2)})
  expect_silent({p$values$ntrees = NA})
  expect_equal(p$values$ntrees, NA)
  expect_equal(p$values$sample.fraction, 0.5)
  expect_silent({p$values$ntrees = 1})
  expect_equal(p$values$ntrees, 1)
  expect_silent({p$values = list(splitrule = "C", sample.fraction = 0.1, ntrees = 2)})
  expect_error({p$values$ntrees = -5}, "does not lie in")
})

test_that("params",{
  expect_equal(class(p$params)[1], "data.table")
  expect_error({p$params = "a"})
})

test_that("rbind",{
  p1 = ParamSet$new(a = LogicalSet$new() ~ TRUE)
  p2 = ParamSet$new(b = LogicalSet$new() ~ FALSE)
  expect_equal(rbind(p1,p2), ParamSet$new(a = LogicalSet$new() ~ TRUE,
                                          b = LogicalSet$new() ~ FALSE))
})
