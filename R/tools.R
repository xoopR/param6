#' Check if two parameters are equal
#' @description Primarily for internal use
#' @param obj,expected [ParameterSet]
#' @export
expect_equal_ps <- function(obj, expected) {
  pobj <- get_private(obj)
  pexp <- get_private(expected)

  expect_setequal(pobj$.id, pexp$.id)

  expect_equal(sort_named_list(pobj$.value),
               sort_named_list(pexp$.value))

  expect_equal(sort_named_list(pobj$.tags),
               sort_named_list(pexp$.tags))

  expect_equal(sort_named_list(pobj$.supports),
               sort_named_list(pexp$.supports))

  expect_equal(sort_named_list(pobj$.immutable),
               sort_named_list(pexp$.immutable))

  if (is.null(pexp$.deps)) {
    expect_null(pobj$.deps)
  } else {
    expect_equal(pobj$.deps[order(pobj$.deps$id), ],
                 pexp$.deps[order(pexp$.deps$id), ])
  }

  if (is.null(pexp$.trafo)) {
    expect_null(pobj$.trafo)
  } else if (is.function(pexp$.trafo)) {
    expect_equal(deparse(pobj$.trafo), deparse(pexp$.trafo))
  } else if (is.null(names(pexp$.trafo))) {
    expect_equal(deparse(pobj$.trafo), deparse(pexp$.trafo))
  } else {
    expect_equal(deparse(sort_named_list(pobj$.trafo)),
                 deparse(sort_named_list(pexp$.trafo)))
  }

  if (!is.null(pexp$.tag_properties)) {
    expect_setequal(names(pobj$.tag_properties), names(pexp$.tag_properties))
    Map(expect_setequal, pobj$.tag_properties, pexp$.tag_properties)
  }


  expect_setequal(names(pobj$.isupports), names(pexp$.isupports))
  Map(expect_setequal, pobj$.isupports, pexp$.isupports)
}
