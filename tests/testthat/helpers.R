expect_equal_ParameterSet <- function(object, expected) { # nolint
  expect_ParameterSet(object)
  expect_ParameterSet(expected)

  expect_equal(
    sapply(object$supports, function(x) x$strprint),
    sapply(expected$supports, function(x) x$strprint)
  )
  if (length(expected$tags)) {
    expect_equal(object$tags, expected$tags)
  } else {
    expect_equal(unname(object$tags), unname(expected$tags))
  }

  expect_equal(object$values[order(names(object$values))],
               expected$values[order(names(expected$values))])
}

expect_ParameterSet <- function(object) { # nolint
  expect(class(object)[1] == "ParameterSet", "Object is not a ParameterSet.")
}
