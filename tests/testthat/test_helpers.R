context("helpers")

test_that("assert_contains", {
  expect_silent(assert_contains(Set$new(1), 1, "ID"))
  expect_error(assert_contains(Set$new(1), 2, "ID"), "does not lie in support of")
  expect_error(assert_contains(Set$new(1), 2), "does not lie in")
})

test_that("assert_no_cycles", {
  expect_silent(assert_no_cycles(data.table(id = "a", on = "b")))
  expect_silent(assert_no_cycles(data.table(id = 1:2, on = c(3, 3))))

  expect_error(assert_no_cycles(data.table(id = 1:2, on = 2:1)), "Cycles detected")
  expect_error(assert_no_cycles(data.table(id = 1:3, on = c(2, 3, 1))), "Cycles detected")
  expect_error(assert_no_cycles(data.table(id = 1:3, on = c(2, 3, 2))), "Cycles detected")
})

test_that("assert_condition", {
  expect_silent(assert_condition("a", Set$new(1), "Equal", 1))
  expect_silent(assert_condition("a", Set$new(1, 2, 3), "AnyOf", c(1, 2)))

  expect_error(assert_condition("a", Set$new(1), "Equal", 2), "Condition is not possible")
  expect_error(assert_condition("a", Set$new(1, 2, 3), "AnyOf", c(1, 4)), "Condition is not possible")
  expect_error(assert_condition("a", Set$new(1), "NotEqual", 2), "Condition is redundant")
  expect_error(assert_condition("a", Set$new(1, 2, 3), "NotAnyOf", c(1, 4)), "Condition is redundant")
})

test_that("string_as_set", {
  expect_null(string_as_set(NULL))
  expect_equal(string_as_set("a"), "{a}")
  expect_equal(string_as_set(c("a", "b")), "{a, b}")
})
