expect_equal_ps <- function(obj, expected) {
  pobj <- get_private(obj)
  pexp <- get_private(expected)
  expect_equal(sort(names(pobj)), sort(names(pexp)))
  lapply(names(pexp), function(x) {
    if (x == "deep_clone") {
      expect_equal(deparse(pobj[[x]]), deparse(pexp[[x]]), info = x)
    } else {
      expect_equal(pobj[[x]], pexp[[x]], info = x)
    }
  })
}
