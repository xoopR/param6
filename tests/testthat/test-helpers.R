test_that("assert_contains", {
  expect_silent(assert_contains(Set$new(1), 1, "ID"))
  expect_error(assert_contains(Set$new(1), 2, "ID"),
               "does not lie in support of")
  expect_error(assert_contains(Set$new(1), 2), "does not lie in")
})

test_that("assert_no_cycles", {
  expect_silent(assert_no_cycles(data.table(id = "a", on = "b")))
  expect_silent(assert_no_cycles(data.table(id = 1:2, on = c(3, 3))))

  expect_error(assert_no_cycles(data.table(id = 1:2, on = 2:1)),
               "Cycles detected")
  expect_error(assert_no_cycles(data.table(id = 1:3, on = c(2, 3, 1))),
               "Cycles detected")
  expect_error(assert_no_cycles(data.table(id = 1:3, on = c(2, 3, 2))),
               "Cycles detected")
})

test_that("cnd", {
  expect_warning(cnd("eq", 1, "a"))
})

test_that("assert_condition", {
  expect_silent(assert_condition("a", Set$new(1), cnd("eq", 1)))
  expect_silent(assert_condition("a", Set$new(1, 2, 3), cnd("any", c(1, 2))))

  expect_error(assert_condition("a", Set$new(1), cnd("eq", 2)),
               "Condition is not possible")
  expect_error(assert_condition("a", Set$new(1, 2, 3), cnd("any", c(1, 4))),
               "Condition is not possible")
  expect_error(assert_condition("a", Set$new(1), cnd("neq", 2)),
               "Condition is redundant")
  expect_error(assert_condition("a", Set$new(1, 2, 3), cnd("nany", c(1, 4))),
               "Condition is redundant")
})

test_that("string_as_set", {
  expect_null(string_as_set(NULL))
  expect_equal(string_as_set("a"), "{a}")
  expect_equal(string_as_set(c("a", "b")), "{a, b}")
})

test_that("env_append", {
  a <- R6Class("a",
               public = list(b = list(), c = function() private$.c),
               private = list(.c = list(y = 1)))
  obj <- a$new()
  expect_silent(env_append(obj, "b", list(x = 1)))
  expect_silent(env_append(get_private(obj), ".c", list(z = 2)))
  expect_equal(obj$b, list(x = 1))
  expect_equal(obj$c(), list(y = 1, z = 2))
})

test_that("invert names", {
  expect_equal(
    invert_names(list(x = "a", y = "a", z = "b")),
    list(a = c("x", "y"), b = "z")
  )
})

test_that("expand_list", {
  expect_equal(
    expand_list(letters[1:3], list(a = 1, c = 2)),
    list(a = 1, b = NULL, c = 2)
  )
})

test_that("named_list", {
  nl <- list(a = 1)
  expect_equal(named_list(1, "a"), nl)

  nl[1:2] <- NULL
  expect_equal(named_list(), nl)
})

test_that("as_named_list", {
  nl <- list(a = 1, b = 2)
  expect_equal(as_named_list(c(1, 2), letters[1:2]), nl)

  nl[1:2] <- NULL
  expect_equal(as_named_list(), nl)
})

test_that("cnd", {
  expect_error(cnd("a", 1), "must be one")
  expect_s3_class(cnd("eq", 1), "cnd")
})


test_that("load_support", {
  expect_is(load_support(), "Dictionary")
})

test_that("sort_named_list", {
  expect_equal(sort_named_list(list(b = 1, a = 2)),
               list(a = 2, b = 1))
})

test_that("%nin%", {
  expect_false("a" %nin% letters)
  expect_true(1 %nin% letters)
})
