expect_equal_ps <- function(obj, expected) {
  pobj <- get_private(obj)
  pexp <- get_private(expected)
  expect_equal(sort(names(pobj)), sort(names(pexp)))
  lapply(names(pexp), function(x) {
    if (is.function(pexp[[x]])) {
      expect_equal(deparse(pobj[[x]]), deparse(pexp[[x]]), info = x)
    } else {
      expect_equal(pobj[[x]], pexp[[x]], info = x)
    }
  })
}


expect_R6_class <- function(obj, what) { # nolint
  expect_true(inherits(obj, c(what, "R6")))
}
