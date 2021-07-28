#' Check if two parameters are equal
#' @description Primarily for internal use
#' @param obj,expected [ParameterSet]
#' @export
expect_equal_ps <- function(obj, expected) {
  pobj <- get_private(obj)
  pexp <- get_private(expected)

  testthat::expect_setequal(pobj$.id, pexp$.id)

  testthat::expect_equal(sort_named_list(pobj$.value),
                         sort_named_list(pexp$.value))

  testthat::expect_equal(sort_named_list(pobj$.tags),
                         sort_named_list(pexp$.tags))

  testthat::expect_equal(sort_named_list(pobj$.supports),
                         sort_named_list(pexp$.supports))

  testthat::expect_equal(sort_named_list(pobj$.immutable),
                         sort_named_list(pexp$.immutable))

  if (is.null(pexp$.deps)) {
    testthat::expect_null(pobj$.deps)
  } else {
    testthat::expect_equal(pobj$.deps[order(pobj$.deps$id), ],
                           pexp$.deps[order(pexp$.deps$id), ])
  }

  if (is.null(pexp$.trafo)) {
    testthat::expect_null(pobj$.trafo)
  } else if (is.function(pexp$.trafo)) {
    testthat::expect_equal(deparse(pobj$.trafo), deparse(pexp$.trafo))
  } else {
    testthat::expect_equal(deparse(sort_named_list(pobj$.trafo)),
                           deparse(sort_named_list(pexp$.trafo)))
  }

  if (!is.null(pexp$.tag_properties)) {
    testthat::expect_setequal(names(pobj$.tag_properties),
                              names(pexp$.tag_properties))
    Map(testthat::expect_setequal, pobj$.tag_properties, pexp$.tag_properties)
  }


  testthat::expect_setequal(names(pobj$.isupports), names(pexp$.isupports))
  Map(testthat::expect_setequal, pobj$.isupports, pexp$.isupports)

  invisible(NULL)
}
