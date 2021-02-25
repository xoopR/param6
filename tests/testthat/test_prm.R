test_that("prm", {
  expect_equal(
    unclass(prm("a", Set$new(1), 1, "a")),
    list(id = "a", support = "{1}", value = 1, tags = "a")
  )
  expect_true(support_dictionary$has("{1}"))
  expect_equal(
    unclass(prm("a", Set$new(1), 1, "a")),
    list(id = "a", support = "{1}", value = 1, tags = "a")
  )

  expect_equal(
    unclass(prm("a", "reals", 1, "a")),
    list(id = "a", support = "reals", value = 1, tags = "a")
  )

  expect_equal(
    unclass(prm("a", "reals")),
    list(id = "a", support = "reals", value = NULL, tags = NULL)
  )

  expect_equal(
    unclass(prm("a", "reals", tags = letters[1:2])),
    list(id = "a", support = "reals", value = NULL, tags = letters[1:2])
  )

  expect_equal(class(prm("a", "reals")), "prm")
})

test_that("prm - error", {
  expect_error(prm("a", Set$new(1), tags =  "c"), "'c' is a")
  expect_error(prm("c", "reals", 2), "'c' is a")

  expect_error(prm("a", "Reals", 1, "a"), "does not exist")
  expect_error(prm("a", 1, 1, "a"), "character scalar")
})

test_that("required prm", {
  expect_silent(prm("a", Set$new(1), 1, "required"))
  expect_error(prm("a", Set$new(1), NULL, "required"), "required")
})

test_that("as.prm.data.table", {
  prms <- list(
    prm("a", Set$new(1), 1, letters[1:2]),
    prm("b", Reals$new(), NULL),
    prm("d", Reals$new(), 2)
  )
  dt <- data.table::data.table(Id = letters[c(1, 2, 4)],
                               Support = list(Set$new(1), Reals$new(),
                                              Reals$new()),
                               Value = list(1, NULL, 2),
                               Tags = list(letters[1:2], NULL, NULL))
  expect_equal(as.prm(dt), prms)

  prms <- list(
    prm("a", "naturals", 1, letters[1:2]),
    prm("b", "reals", NULL),
    prm("d", "reals", 2)
  )
  dt <- data.table::data.table(Id = letters[c(1, 2, 4)],
                               Support = list("naturals", "reals", "reals"),
                               Value = list(1, NULL, 2),
                               Tags = list(letters[1:2], NULL, NULL))
  expect_equal(as.prm(dt), prms)
})

test_that("as.prm.ParameterSet", {
  prms <- list(
    prm("a", Set$new(1), 1),
    prm("b", "reals"),
    prm("d", "reals")
  )
  expect_equal(as.prm(as.ParameterSet(prms)), prms)
})
