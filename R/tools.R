#' Check if two parameters are equal
#' @description Primarily for internal use
#' @param obj,expected [ParameterSet]
#' @export
expect_equal_ps <- function(obj, expected) {
  pobj <- get_private(obj)
  pexp <- get_private(expected)
  expect_equal(sort(names(pobj)), sort(names(pexp)))
  lapply(names(pexp), function(x) {
    expect_equal(deparse(pobj[[x]]), deparse(pexp[[x]]), info = x)
  })
}