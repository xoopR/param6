#' Check if two parameters are equal
#' @description Primarily for internal use
#' @param obj,expected [ParameterSet]
#' @export
expect_equal_ps <- function(obj, expected) {
  pobj <- get_private(obj)
  pexp <- get_private(expected)
  testthat::expect_equal(sort(names(pobj)), sort(names(pexp)))
  lapply(names(pexp), function(x) {
    if (is.list(pexp[[x]]) && length(names(pexp[[x]])) > 0) {
      testthat::expect_equal(deparse(sort_named_list(pobj[[x]])),
                   deparse(sort_named_list(pexp[[x]])), info = x)
    } else {
      testthat::expect_equal(deparse(pobj[[x]]), deparse(pexp[[x]]), info = x)
    }
  })
}
