test_that("untyped construction", {
  # empty
  expect_silent(Dictionary$new())
  # elements same types
  expect_silent(Dictionary$new(list(a = 1, b = 1)))
  # elements different types
  expect_silent(Dictionary$new(list(a = 1, b = 2L, c = "c")))
  # not unique
  expect_error(Dictionary$new(list(a = 1, a = 2)), "unique")
})

test_that("typed construction", {
  # empty
  expect_silent(Dictionary$new(types = "numeric"))
  # elements same types
  expect_silent(Dictionary$new(list(a = 1, b = 1), types = "numeric"))
  expect_error(Dictionary$new(list(a = 1, b = 1), types = "integer"), "integer")
  # elements different types
  expect_error(Dictionary$new(list(a = 1, b = "b"), types = "numeric"),
               "character")
  expect_silent(Dictionary$new(list(a = 1, b = "b"), types = c("numeric",
                                                               "character")))
  # custom types
  expect_silent(Dictionary$new(list(r = Reals$new(), b = Naturals$new()),
                               types = "Set"))
})

test_that("add untyped", {
  d_untyped <- Dictionary$new(list(a = 1, b = 2))

  expect_silent(d_untyped$add(list(c = 3)))
  expect_error(d_untyped$add(list(c = 4)), "duplicated")
  expect_error(d_untyped$add(list(a = 3)), "duplicated")
  expect_silent(d_untyped$add(list(d = "a")))

  expect_equal(
    Dictionary$new(list(a = 1, b = 2))$add(list(c = 3, d = 4)),
    Dictionary$new(list(a = 1, b = 2, c = 3, d = 4))
  )

  expect_error(Dictionary$new()$add(), "Either")
})

test_that("add typed", {
  d_typed <- Dictionary$new(list(a = 1, b = 2), types = "numeric")

  expect_silent(d_typed$add(list(c = 3)))
  expect_error(d_typed$add(list(c = 4)), "duplicated")
  expect_error(d_typed$add(list(a = 3)), "duplicated")
  expect_error(d_typed$add(list(d = "a")), "numeric")

  expect_equal(
    Dictionary$new(list(a = 1, b = 2), types = "numeric")$add(list(c = 3,
                                                                   d = 4)),
    Dictionary$new(list(a = 1, b = 2, c = 3, d = 4), types = "numeric")
  )
})

test_that("remove", {
  d_typed <- Dictionary$new(list(a = 1, b = 2), types = "numeric")
  d_untyped <- Dictionary$new(list(a = 1, b = 2))

  expect_error(d_typed$remove("c"), "subset of")
  expect_error(d_untyped$remove("c"), "subset of")

  expect_equal(d_typed$remove(letters[1:2]), Dictionary$new(types = "numeric"))
  expect_equal(d_untyped$remove(letters[1:2]), Dictionary$new())

  expect_silent(d_untyped$add(list(a = 1, b = 2)))
  expect_equal(d_untyped$remove("a"), Dictionary$new(list(b = 2)))
})

test_that("get", {
  d_untyped <- Dictionary$new(list(a = 1, b = "a"))
  expect_equal(d_untyped$get("a"), 1)
  expect_equal(d_untyped$get("b"), "a")
  expect_error(d_untyped$get("c"), "subset of")
  expect_error(d_untyped$get(letters[1:2]), "length")

  d_typed <- Dictionary$new(list(a = 1, b = 2), types = "numeric")
  expect_equal(d_typed$get("a"), 1)
  expect_equal(d_typed$get("b"), 2)
  expect_equal(d_typed$get(letters[1:2]), c(1, 2))
  expect_error(d_typed$get("c"), "subset of")

  d <- Dictionary$new(list(a = Set$new(1), b = 2))
  expect_equal(d$get("a"), Set$new(1))
  expect_equal(d$get("b"), 2)

  d <- Dictionary$new(list(a = Set$new(1), b = Set$new(2)), types = "Set")
  expect_equal(d$get(c("a", "b")), list(a = Set$new(1), b = Set$new(2)))
})

test_that("get_list", {
  d_untyped <- Dictionary$new(list(a = 1, b = 2))

  expect_error(d_untyped$get_list("c"), "subset of")
  expect_equal(d_untyped$get_list("a"), list(a = 1))
  expect_equal(d_untyped$get_list(c("a", "b")), list(a = 1, b = 2))
  expect_equal(d_untyped[c("a", "b")], list(a = 1, b = 2))

  d <- Dictionary$new(list(a = Set$new(1), b = 2))
  expect_equal(d$get_list(c("a", "b")), list(a = Set$new(1), b = 2))
})

test_that("has", {
  d_untyped <- Dictionary$new(list(a = 1, b = 2))
  expect_true(d_untyped$has("a"))
  expect_false(d_untyped$has("c"))
  expect_equal(d_untyped$has(c("a", "c", "b")), c(TRUE, FALSE, TRUE))
})

test_that("has_value", {
  d_untyped <- Dictionary$new(list(a = 1, b = 2))
  expect_true(d_untyped$has_value(1))
  expect_false(d_untyped$has_value(3))
  expect_equal(d_untyped$has_value(1:3), c(TRUE, TRUE, FALSE))
})

test_that("rekey", {
  d_untyped <- Dictionary$new(list(a = 1, b = 2))
  expect_silent(d_untyped$rekey("a", "c"))
  expect_equal(d_untyped$items, list(c = 1, b = 2))
  expect_error(d_untyped$rekey("a", "d"), "element of set")
  expect_error(d_untyped$rekey("c", "b"), "duplicated")
})

test_that("active", {
  d_typed <- Dictionary$new(list(a = 1, b = 2), types = "numeric")
  expect_equal(d_typed$keys, c("a", "b"))
  expect_equal(d_typed$length, 2)
  expect_equal(length(d_typed), 2)

  expect_true(d_typed$typed)
  expect_false(Dictionary$new(list(a = 1, b = 2))$typed)

  expect_equal(d_typed$types, c("numeric"))
  expect_equal(Dictionary$new(list(a = 1, b = 2))$types, NULL)

  expect_equal(d_typed$values, c(1, 2))
  expect_equal(Dictionary$new(list(a = 1, b = 2))$values, list(1, 2))

  expect_equal(d_typed$items, list(a = 1, b = 2))
  d_typed$items$a <- 2
  expect_equal(d_typed$items, list(a = 2, b = 2))
  expect_error({ d_typed$items$a <- "c" }) # nolint
  d_typed$items$a <- NULL
  expect_equal(d_typed$items, list(b = 2))
  d_typed$items <- list(a = 3, b = 4)
  expect_equal(d_typed$items, list(a = 3, b = 4))
  d_typed$items[letters[1:2]] <- NULL
  expect_equal(d_typed$values, NULL)
})

test_that("print and summary", {
  d_typed <- Dictionary$new(list(a = 1, b = 2, c = 3), types = "numeric")
  d_untyped <- Dictionary$new(list(a = 1, b = 2, c = 3))

  expect_equal(as.character(d_typed), "{a: 1, b: 2, c: 3}")
  expect_equal(as.character(d_typed, 1), "{a: 1, ..., c: 3}")
  expect_equal(as.character(d_untyped, 1), "{a: 1, ..., c: 3}")

  expect_output(print(d_typed))
  expect_output(summary(d_typed))
  expect_output(summary(d_typed), "Typed dictionary of")
  expect_output(print(d_untyped))
  expect_output(summary(d_untyped))
  expect_output(summary(d_untyped), "Untyped dictionary of 3 items.")
})

test_that("concatenate", {
  a_typed <- Dictionary$new(list(a = 1), types = "numeric")
  b_typed <- Dictionary$new(list(b = 2), types = c("numeric", "integer"))
  c_typed <- Dictionary$new(list(c = 3), types = c("integer", "numeric"))
  d_typed <- Dictionary$new(list(a = 3), types = "numeric")
  e_typed <- Dictionary$new(list(a = 3L), types = "integer")

  a_untyped <- Dictionary$new(list(a = 1))
  b_untyped <- Dictionary$new(list(b = 2))
  c_untyped <- Dictionary$new(list(c = 2))
  d_untyped <- Dictionary$new(list(a = 2))

  expect_error(c(a_typed, b_typed), "same type")
  expect_error(c(a_typed, c_typed), "same type")
  expect_error(c(d_typed, e_typed), "same type")
  expect_error(c(a_typed, a_untyped), "typed or all")
  expect_error(c(a_typed, d_typed), "duplicated")
  expect_equal(c(b_typed, c_typed),
               Dictionary$new(list(b = 2, c = 3),
                              types = c("numeric", "integer")))

  expect_error(c(a_untyped, a_typed), "typed or all")
  expect_error(c(a_typed, d_typed), "duplicated")
  expect_equal(c(a_untyped, b_untyped, c_untyped),
               Dictionary$new(list(a = 1, b = 2, c = 2)))
})

test_that("merge", {
  a_typed <- Dictionary$new(list(a = 1), types = "numeric")
  c_typed <- Dictionary$new(list(c = 3), types = c("integer", "numeric"))
  d_typed <- Dictionary$new(list(a = 3), types = "numeric")

  a_untyped <- Dictionary$new(list(a = "b"))
  b_untyped <- Dictionary$new(list(b = 2))
  c_untyped <- Dictionary$new(list(c = 2))
  d_untyped <- Dictionary$new(list(a = 2))

  expect_equal(a_typed$merge(list(b_untyped, c_untyped))$items,
               list(a = 1, b = 2, c = 2))
  expect_error(a_typed$merge(a_untyped)$items, "character")
  expect_error(a_typed$merge("a"), "Dictionary or")
})

test_that("deep clone", {
  d1 <- Dictionary$new(list(a = Set$new(1), d = 1))
  d2 <- d1$clone(deep = TRUE)
  d3 <- d1
  d2$add(list(b = 2))
  expect_equal(length(d1), length(d3))
  expect_false(length(d1) == length(d2))
})
