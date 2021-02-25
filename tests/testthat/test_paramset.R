test_that("ParameterSet constructor - silent", {
  prms <- list(
    prm("a", Set$new(1), 1, "t1"),
    prm("b", "reals", NULL),
    prm("d", "reals", 2)
  )
  expect_silent(ParameterSet$new(prms))

  prms <- list(
    prm("a", Set$new(1), 1),
    prm("b", "reals"),
    prm("d", "reals")
  )
  expect_silent(ParameterSet$new(prms))

  expect_silent(ParameterSet$new())
})

test_that("ParameterSet constructor - error", {
  prms <- list(
    prm("a", Set$new(1), 1, "a"),
    prm("a", "reals", NULL),
    prm("d", "reals", 2)
  )
  expect_error(ParameterSet$new(prms), "ids are not unique")
})

test_that("ParamSet actives - not values or tag propeties", {
  prms <- list(
    prm("a", Set$new(1, 2), 1, c("t1", "t2")),
    prm("b", "reals", 2, "t2"),
    prm("d", "reals", 2)
  )
  p <- ParameterSet$new(prms, list(linked = "t1", required = "t2"))

  expect_equal(p$tags, list(a = c("t1", "t2"), b = "t2"))
  expect_equal(p$ids, c("a", "b", "d"))
  expect_equal(length(p), 3)
  expect_equal(p$supports, list(a = Set$new(1, 2), b = Reals$new(),
                                d = Reals$new()))
})

test_that("ParamSet actives - values", {
  prms <- list(
    prm("a", Set$new(1, 2), 1),
    prm("b", "reals", NULL),
    prm("d", "reals", 2)
  )
  p <- ParameterSet$new(prms)

  expect_equal(p$values, list(a = 1, d = 2))
  expect_silent(p$values$a <- 2)
  expect_equal(p$values$a, 2)
  expect_error(p$values$a <- 3, "3 does not")
  expect_equal(p$values$a, 2)
  expect_error(p$values <- list(a = 3, b = 1, c = 1), "3 does not")
  expect_equal(p$values, list(a = 2, d = 2))
  expect_silent(p$values <- list(a = 1))
  expect_equal(p$values, list(a = 1))
  expect_silent(p$values$a <- NULL)
  p$values <- list(a = 1, b = 1, d = 1)
  p$values$a <- NULL
  expect_equal(p$values, list(b = 1, d = 1))
})

test_that("ParamSet actives - tag properties", {
  prms <- list(
    prm("a", Set$new(1, 2), 1, c("t1", "t2")),
    prm("b", "reals", 2, "t2"),
    prm("d", "reals", 2, "t3")
  )
  p <- ParameterSet$new(prms, list(linked = "t1", required = "t2"))

  expect_equal(p$tag_properties, list(linked = "t1", required = "t2"))
  expect_silent({p$tag_properties <- NULL}) # nolint
  expect_silent({p$tag_properties$required <- "t2"}) # nolint
  expect_error({p$tag_properties <- list(required = "t1", linked = "t1")}) # nolint
  expect_error({p$tag_properties <- list(required = "t3")}) # nolint
  expect_error({p$tag_properties <- list(linked = "t2")}) # nolint
})

test_that("as.data.table.ParameterSet and print", {
  prms <- list(
    prm("a", Set$new(1), 1, c("t1", "t2")),
    prm("b", "reals", NULL),
    prm("d", "reals", 2)
  )
  p <- ParameterSet$new(prms)
  expect_equal(as.data.table(p),
               data.table::data.table(Id = letters[c(1, 2, 4)],
                                      Support = list(Set$new(1), Reals$new(),
                                                     Reals$new()),
                                      Value = list(1, NULL, 2),
                                      Tags = list(c("t1", "t2"), NULL, NULL)))

  expect_output(print(p))
})

test_that("as.ParameterSet.data.table", {
  prms <- list(
    prm("a", Set$new(1), 1, c("t1", "t2")),
    prm("b", Reals$new(), NULL),
    prm("d", Reals$new(), 2)
  )
  dt <- data.table::data.table(Id = letters[c(1, 2, 4)],
                               Support = list(Set$new(1), Reals$new(),
                                              Reals$new()),
                               Value = list(1, NULL, 2),
                               Tags = list(c("t1", "t2"), NULL, NULL))
  expect_equal(as.ParameterSet(dt), ParameterSet$new(prms))

  prms <- list(
    prm("a", "naturals", 1, c("t1", "t2")),
    prm("b", "reals", NULL),
    prm("d", "reals", 2)
  )
  dt <- data.table::data.table(Id = letters[c(1, 2, 4)],
                               Support = list("naturals", "reals", "reals"),
                               Value = list(1, NULL, 2),
                               Tags = list(c("t1", "t2"), NULL, NULL))
  expect_equal(as.ParameterSet(dt), ParameterSet$new(prms))
})

test_that("get_values", {
  prms <- list(
    prm("a", Set$new(1), 1, tags = "t1"),
    prm("b", "reals", tags = "t1"),
    prm("d", "reals", tags = "t2")
  )
  p <- ParameterSet$new(prms)
  expect_equal(p$get_values(inc_null = TRUE), list(a = 1, b = NULL, d = NULL))
  expect_equal(p$get_values(inc_null = FALSE, simplify = FALSE), list(a = 1))

  expect_equal(p$get_values(inc_null = TRUE, tags = "t1"),
               list(a = 1, b = NULL))
  expect_equal(p$get_values(inc_null = FALSE, tags = "t1"), 1)
  expect_equal(p$get_values(inc_null = FALSE, tags = "t1", simplify = FALSE),
               list(a = 1))

  expect_equal(p$get_values(inc_null = TRUE, tags = "t2"), NULL)
  expect_equal(p$get_values(inc_null = FALSE, tags = "t2", simplify = FALSE),
               list())

  prms <- list(
    prm("Pre1__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre1__par2", "reals", 2, tags = "t2"),
    prm("Pre2__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre2__par2", "reals", tags = "t2")
  )
  p <- ParameterSet$new(prms)
  expect_equal(p$get_values("Pre1"), list(Pre1__par1 = 1, Pre1__par2 = 2))
  expect_equal(p$get_values(c("Pre1", "Pre2")),
               list(Pre1__par1 = 1, Pre1__par2 = 2, Pre2__par1 = 1,
                    Pre2__par2 = NULL))
  expect_equal(p$get_values("par1"), list(Pre1__par1 = 1, Pre2__par1 = 1))
})

test_that("trafo", {
  prms <- list(
    prm("a", Set$new(1, 2), 1, tags = "t1"),
    prm("b", "reals", tags = "t1"),
    prm("d", "reals", tags = "t2")
  )
  p <- ParameterSet$new(prms)
  expect_equal(p$trafo, NULL)
  expect_error({p$trafo <- "a"}, "function") # nolint
  expect_error({p$trafo <- function(x, self) "a"}, "list") # nolint
  expect_silent({
    p$trafo <- function(x, self) {
      x$a <- x$a + 1
      x$b <- 3
      x
    }
  })
  expect_error({
    p$trafo <- function(x, self) {
      x$a <- x$a + 2
      x$b <- 3
      x
    }
  }, "outside")
  expect_equal(p$get_values(inc_null = FALSE), list(a = 2, b = 3))

  prms <- list(
    prm("a", Set$new(1, exp(1)), 1, tags = "t1"),
    prm("b", "reals", 2, tags = "t1"),
    prm("d", "reals", tags = "t2")
  )
  p <- ParameterSet$new(prms)
  expect_silent({
    p$trafo <- function(x, self) {
      x <- lapply(self$get_values(tags = "t1", transform = FALSE), exp)
      x
    }
  })
  expect_equal(p$get_values(inc_null = FALSE), list(a = exp(1), b = exp(2)))

  prms <- list(
    prm("a", Set$new(1, exp(1)), exp(1), tags = "t1"),
    prm("b", "reals", exp(2), tags = "t1"),
    prm("d", "reals", tags = "t2")
  )
  p2 <- ParameterSet$new(prms)

  expect_equal(as.data.table(p$transform()), as.data.table(p2))

  p <- ParameterSet$new(
  list(prm(id = "a", 2, support = Reals$new(), tags = "t1"),
       prm(id = "b", 3, support = Reals$new(), tags = "t1"),
       prm(id = "d", 4, support = Reals$new()))
)
  p$trafo <- function(x, self) {
    out <- lapply(self$get_values(tags = "t1"), function(.x) 2^.x)
    out <- c(out, list(d = x$d))
    out
  }
  p$get_values()
})

test_that("rep", {
  prms <- list(
    prm("par1", Set$new(1), 1, tags = "t1"),
    prm("par2", "reals", 3, tags = "t2")
  )
  p1 <- ParameterSet$new(prms, tag_properties = list(required = "t1",
                                                     linked = "t2"))

  prms <- list(
    prm("Pre1__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre1__par2", "reals", 3, tags = "Pre1__t2"),
    prm("Pre2__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre2__par2", "reals", 3, tags = "Pre2__t2")
  )
  p2 <- ParameterSet$new(
    prms,
    tag_properties = list(required = "t1", linked = c("Pre1__t2", "Pre2__t2"))
  )

  expect_equal(p1$rep(2, "Pre"), p2)

  prms <- list(
    prm("par1", Set$new(1), 1, tags = "t1"),
    prm("par2", "reals", 3, tags = "t2")
  )
  p1 <- ParameterSet$new(prms, tag_properties = list(required = "t1",
                                                     linked = "t2"))
  expect_equal(rep(p1, 2, "Pre"), p2)
  expect_equal(length(p1), 2)
})

test_that("add_dep", {
  prms <- list(
    prm("a", Set$new(1), 1, tags = "t1"),
    prm("b", "reals", tags = "t1"),
    prm("d", "reals", tags = "t2")
  )
  p <- ParameterSet$new(prms)
  expect_error(p$add_dep("a", "b", cnd(1, "eq")), "failed")
  expect_silent(p$add_dep("b", "a", cnd(1, "eq")))
  p$values$b <- 3
  expect_error({ p$values$a <- NULL }, "failed") # nolint


  prms <- list(
    prm("Pre1__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre1__par2", "reals", 3, tags = "t2"),
    prm("Pre2__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre2__par2", "reals", 3, tags = "t2")
  )
  p2 <- ParameterSet$new(prms)
  expect_error(p2$add_dep("par1", "par2", cnd(1:2, "any")), "Dependency of")
  expect_silent(p2$add_dep("par1", "par2", cnd(3, "eq")))
  expect_error(p2$add_dep("Pre1", "Pre2", cnd(3, "eq")), "subset of")
})

test_that("c", {
  prms <- list(
    prm("a", Set$new(1, 2), 1, c("t1", "t2")),
    prm("b", "reals", NULL, "t3"),
    prm("d", "reals", 2),
    prm("e", "reals", 2)
  )
  p <- ParameterSet$new(prms, list(required = "t1", linked = "t2"))

  p1 <- ParameterSet$new(list(prm("a", Set$new(1, 2), 1, c("t1", "t2"))),
                         list(required = "t1", linked = "t2"))
  p2 <- ParameterSet$new(list(prm("b", "reals", NULL, "t3")))
  p3 <- ParameterSet$new(list(prm("d", "reals", 2), prm("e", "reals", 2)))
  p4 <- ParameterSet$new(list(prm("e", "reals", 1, "t1")),
                         list(linked = "t1"))

  expect_error(c(p1, p4), "inconsistent")
  expect_equal(as.data.table(c(p1, p2, p3)), as.data.table(p))
  expect_equal(p$tag_properties, c(p1, p2)$tag_properties)
  expect_equal(c(p2, p3)$tag_properties, list())
})

test_that("checks", {
  prms <- list(
    prm("a", Set$new(0.5, 1), 1, tags = "t1"),
    prm("b", "reals", 2, tags = "t1"),
    prm("d", "reals", 5, tags = "t2")
  )
  p <- ParameterSet$new(prms)
  expect_true(p$check())
  expect_equal(p$checks, NULL)
  expect_error(p$add_check(function(x, self) x$a == 1, "e"), "subset")
  expect_silent(p$add_check(function(x, self) x$a == 0.5, "a"))
  expect_equal(
    p$checks,
    data.table(ids = list("a"), tags = list(),
               fun = list(body(function(x) x$a == 0.5))))
  expect_false(expect_warning(p$check(error_on_fail = FALSE), "Check on"))
  expect_error(p$check(error_on_fail = TRUE), "Check on")
  p$values$a <- 0.5
  expect_true(p$check())
  expect_silent(p$add_check(function(x, self) x$a + x$b == 2.5, c("a", "b")))
  expect_true(p$check())
  expect_silent(p$add_check(function(x, self) x$d == 6, "d"))
  expect_error(p$check())


  p$add_check(function(x, self) all(self$get_values(tags = "t1") > 0),
              tags = "t1")
  expect_error(p$check())
  expect_equal(nrow(p$checks), 4)

  prms <- list(
    prm("a", Set$new(0.5, 1), 1, tags = "t1"),
    prm("b", "reals", 2, tags = "t1"),
    prm("d", "reals", 5, tags = "t2")
  )
  p <- ParameterSet$new(prms, list(required = "t1"))
  expect_true(p$check())
  expect_error(p$values$a <- NULL, "Not all")
  expect_error(p$values$b <- NULL, "Not all")

  prms <- list(
    prm("a", Set$new(0.5, 1), 1, tags = "t1"),
    prm("b", "reals", 1, tags = "t1"),
    prm("d", "reals", 5, tags = "t2")
  )
  expect_error(ParameterSet$new(prms, list(required = "t2", linked = "t1")),
               "Multiple")

  prms <- list(
    prm("a", Set$new(0.5, 1), tags = "t1"),
    prm("b", "reals", 1, tags = "t1"),
    prm("d", "reals", tags = "t2")
  )
  expect_error(ParameterSet$new(prms, list(required = "t2", linked = "t1")),
               "required")

  prms <- list(
    prm("a", Set$new(0.5, 1), tags = "t1"),
    prm("b", "reals", 1, tags = "t1"),
    prm("d", "reals", 1, tags = "t2")
  )
  p <- ParameterSet$new(prms, list(required = "t2", linked = "t1"))
  expect_true(p$check())
  expect_error(p$values$a <- 1, "Multiple")
  expect_error(p$values$d <- NULL, "required")
})

test_that("extract - no deps or checks", {
  prms <- list(
    prm("Pre1__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre1__par2", "reals", 3, tags = "t2"),
    prm("Pre2__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre2__par2", "reals", 3, tags = "t2")
  )
  p <- ParameterSet$new(prms)

  prms <- list(
    prm("par1", Set$new(1), 1, tags = "t1"),
    prm("par2", "reals", 3, tags = "t2")
  )
  p2 <- ParameterSet$new(prms)
  expect_equal(p$extract(prefix = "Pre1"), p2)

  prms <- list(
    prm("Pre1__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre1__par2", "reals", 3, tags = "t2")
  )
  p2 <- ParameterSet$new(prms)
  expect_equal(p$extract("Pre1"), p2)

  prms <- list(
    prm("Pre1__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre2__par1", Set$new(1), 1, tags = "t1")
  )
  p2 <- ParameterSet$new(prms)
  expect_equal(p$extract("par1"), p2)

  prms <- list(
    prm("Pre1__par1", Set$new(1), 1, tags = "t1")
  )
  p2 <- ParameterSet$new(prms)
  expect_equal(p$extract("Pre1__par1"), p2)

  prms <- list(
    prm("Pre1__par1", Set$new(1), 1, tags = "Pre1__t1"),
    prm("Pre1__par2", "reals", 3, tags = "t2"),
    prm("Pre2__par1", Set$new(1), 1, tags = "Pre2__t1"),
    prm("Pre2__par2", "reals", 3, tags = "t2")
  )
  p <- ParameterSet$new(prms, list(linked = c("Pre1__t1", "Pre2__t1"),
                                   required = c("t2")))

  prms <- list(
    prm("par1", Set$new(1), 1, tags = "t1"),
    prm("par2", "reals", 3, tags = "t2")
  )
  p2 <- ParameterSet$new(prms, list(linked = "t1", required = "t2"))
  expect_equal(p$extract(prefix = "Pre1"), p2)
})

test_that("extract - deps", {
  prms <- list(
    prm("a", Set$new(1), 1, tags = "t1"),
    prm("b", "reals", tags = "t1"),
    prm("d", "reals", 2, tags = "t2")
  )
  p <- ParameterSet$new(prms)
  p$add_dep("b", "a", cnd(1, "eq"))
  p$add_dep("a", "d", cnd(0, "gt"))

  expect_equal(p$extract("a")$deps, NULL)
  expect_equal(p$extract(letters[1:2])$deps,
               data.table::data.table(id = "b", on = "a",
                                      cond = list(cnd(1, "eq"))))

  prms <- list(
    prm("Pre1__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre1__par2", "reals", 3, tags = "t2"),
    prm("Pre2__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre2__par2", "reals", 3, tags = "t2")
  )
  p <- ParameterSet$new(prms)
  p$add_dep("Pre1__par1", "Pre1__par2", cnd(3, "eq"))
  expect_equal(p$extract(prefix = "Pre1")$deps,
               data.table::data.table(id = "par1", on = "par2",
                                      cond = list(cnd(3, "eq"))))

})


test_that("extract - checks", {
  prms <- list(
    prm("a", Set$new(1), 1, tags = "t1"),
    prm("b", "reals", 1.5, tags = "t1"),
    prm("d", "reals", 2, tags = "t2")
  )
  p <- pset(prms)
  p$add_check(function(x, self) x$a + x$b == 2.5, c("a", "b"))
  p$add_check(function(x, self) x$a + x$b == 2.5, tags = "t1")


  expect_equal(p$extract("a")$checks, NULL)
  expect_equal(p$extract("b")$checks, NULL)
  expect_equal(p$extract(c("a", "b"))$checks, p$checks[1, ])
  expect_equal(p$extract(tags = "t1")$checks, p$checks[2, ])
  expect_equal(p$extract(id = c("a", "b"), tags = "t1")$checks, p$checks)
})
