context("ParamSetCollection")

p1 <- ParamSet$new(
  splitrule = Set$new("logrank", "extratrees", "C", "maxstat") ~ tags("train", "predict") + "logrank",
  sample.fraction = Interval$new(0, 1) ~ 0.5
)
p2 <- ParamSet$new(ntrees = PosIntegers$new() ~ 2)
p3 <- ParamSet$new(lgl = LogicalSet$new())

test_that("constructor", {
  expect_error(ParamSetCollection$new(p1, p2), "unused")
  expect_error(ParamSetCollection$new(list(p1, p2)), "names")
  expect_error(ParamSetCollection$new(list(a = p1, a = p2)), "unique")
  expect_silent(ParamSetCollection$new(list(a = p1, b = p2)))
})

c <- ParamSetCollection$new(list(a = p1, b = p2))

test_that("print", {
  expect_output(print(c))
})

test_that("add", {
  expect_error(c$add(p3), "list")
  expect_error(c$add(list(p3)), "names")
  expect_error(c$add(list(a = p3)), "unique")
  expect_silent(c$add(list(c = p3)))
})

test_that("remove", {
  expect_error(c$remove(list(c)), "character")
  expect_equal(c$remove("d"), c)
  expect_silent(c$remove("c"))
})

test_that("get_values", {
  expect_equal(c$get_values(), list(
    a_splitrule = "logrank", a_sample.fraction = 0.5,
    b_ntrees = 2
  ))
  expect_equal(c$get_values("predict"), list(a_splitrule = "logrank"))
})

test_that("params", {
  checkmate::expect_data_table(c$params, max.rows = 3, max.cols = 4)
})

test_that("supports", {
  expect_equal(c$supports, list(
    a_splitrule = Set$new("logrank", "extratrees", "C", "maxstat"),
    a_sample.fraction = Interval$new(0, 1),
    b_ntrees = PosIntegers$new()
  ))
})

test_that("tags", {
  expect_equal(c$tags, list(a_splitrule = c("train", "predict"), a_sample.fraction = NULL, b_ntrees = NULL))
})

test_that("length", {
  expect_equal(c$length, 3)
})

test_that("ids", {
  expect_equal(c$ids, c("a_splitrule", "a_sample.fraction", "b_ntrees"))
})

test_that("Values", {
  expect_equal(c$values, list(a_splitrule = "logrank", a_sample.fraction = 0.5, b_ntrees = 2))
  expect_silent({
    c$values$a_splitrule <- "C"
  })
  expect_equal(c$values$a_splitrule, "C")
  expect_error(
    {
      c$values$a_splitrule <- 2
    },
    "does not lie"
  )
  expect_equal(c$.__enclos_env__$private$.sets$a$values$splitrule, "C")
  expect_silent({
    c$values <- list(a_sample.fraction = 0.1)
  })
  expect_equal(c$params$Value, list(NULL, 0.1, 2))
})
