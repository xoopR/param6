expect_R6_class <- function(obj, what) { # nolint
  expect_true(inherits(obj, c(what, "R6")))
}
