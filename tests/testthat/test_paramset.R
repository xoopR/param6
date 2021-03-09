test_that("ParameterSet constructor - silent", {
  prms <- list(
    prm("a", Set$new(1), 1, "t1"),
    prm("b", "reals", NULL),
    prm("d", "reals", 2)
  )
  expect_is(ParameterSet$new(prms), "ParameterSet")

  prms <- list(
    prm("a", Set$new(1), 1),
    prm("b", "reals"),
    prm("d", "reals")
  )
  expect_is(ParameterSet$new(prms), "ParameterSet")

  expect_is(ParameterSet$new(), "ParameterSet")

  expect_is(as.ParameterSet(prm("a", "reals")), "ParameterSet")
})

test_that("ParameterSet constructor - error", {
  prms <- list(
    prm("a", Set$new(1), 1, "a"),
    prm("a", "reals", NULL),
    prm("d", "reals", 2)
  )
  expect_error(ParameterSet$new(prms), "ids are not unique")

  prms <- list(
    prm("a", Set$new(1), 1, "d"),
    prm("b", "reals", NULL),
    prm("d", "reals", 2)
  )
  expect_error(ParameterSet$new(prms), "ids and tags")
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
    prm("b", "reals", NULL, "t1"),
    prm("d", "reals", 2, "t1")
  )
  p <- ParameterSet$new(prms, list(linked = "t1"))

  expect_equal(p$values, list(a = 1, d = 2))
  expect_silent(p$values$a <- 2)
  expect_equal(p$values$a, 2)
  expect_error(p$values$a <- 3, "does not")
  expect_equal(p$values$a, 2)
  expect_error(p$values <- list(a = 3, d = 1), "does not")
  expect_equal(p$values, list(a = 2, d = 2))
  expect_silent(p$values <- list(a = 1))
  expect_equal(p$values, list(a = 1))
  expect_silent(p$values$a <- NULL)
  p$values <- list(a = 1, b = 1, d = NULL)
  p$values$a <- NULL
  expect_equal(p$values, list(b = 1))

  p$values$a <- 1

  expect_false(expect_warning(
    .check(p, supports = TRUE, deps = FALSE, tags = FALSE,
           error_on_fail = FALSE, value_check = list(a = 3),
           support_check = get_private(p)$.isupports)))

  p$add_dep("b", "a", cnd("eq", 1))
  expect_false(expect_warning(
    .check(p, supports = FALSE, deps = TRUE, tags = FALSE,
           error_on_fail = FALSE, value_check = list(b = 1, a = 3),
           dep_check = p$deps)))

  expect_false(expect_warning(
    .check(p, supports = FALSE, deps = FALSE, tags = TRUE,
           id = c("b", "d"),
           error_on_fail = FALSE, value_check = list(b = 1, d = 1),
           tag_check = p$tag_properties)))
  expect_error({p$values <- list(a = 1, b = 1, d = 1)}, "Multiple linked") # nolint


  prms <- list(
    prm("b", "naturals", 1),
    prm("d", "naturals", 2)
  )
  p <- ParameterSet$new(prms)
  expect_error({p$values <- list(b = 0.5, d = 0.5)}, "One or") # nolint

  prms <- list(
    prm("a", "nnaturals", 1)
  )
  p <- ParameterSet$new(prms)
  expect_silent({p$values$a <- 2}) # nolint
  expect_silent({p$values <- list(a = c(1, 2))}) # nolint
  expect_error({p$values <- list(a = c(1, 0.5))}, "does not lie") # nolint
})

test_that("ParamSet actives - tag properties", {
  prms <- list(
    prm("a", Set$new(1, 2), 1, c("t1", "t2")),
    prm("b", "reals", 2, "t2"),
    prm("d", "reals", NULL, "t3")
  )
  p <- ParameterSet$new(prms, list(linked = "t1", required = "t2"))

  expect_equal(p$tag_properties, list(linked = "t1", required = "t2"))
  expect_silent({p$tag_properties <- NULL}) # nolint
  expect_silent({p$tag_properties$required <- "t2"}) # nolint
  expect_error({p$tag_properties <- list(required = "t1", linked = "t1")}) # nolint
  expect_error({p$tag_properties <- list(required = "t3")}) # nolint
  expect_error({p$tag_properties <- list(linked = "t2")}) # nolint

  prms <- list(
    prm("a", "nreals", 1, tags = "t1"),
    prm("b", "nreals", 3, tags = "t2")
  )
  p <- ParameterSet$new(prms, tag_properties = list(unique = "t1"))
  expect_silent({p$values$a <- 2}) # nolint
  expect_silent({p$values <- list(a = c(1, 2))}) # nolint
  expect_error({p$values <- list(a = c(2, 2))}, "duplicated") # nolint
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

  p$trafo <- function(x, self) x
  expect_warning(as.data.table(p), "Dependencies")
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

  p <- ParameterSet$new(
  list(prm(id = "a", 2, support = Reals$new(), tags = "t1"),
       prm(id = "b", 3, support = Reals$new(), tags = "t1"),
       prm(id = "d", 4, support = Reals$new()))
)
  p$trafo <- function(x, self) {
    out <- lapply(self$get_values(tags = "t1", transform = FALSE),
                  function(.x) 2^.x)
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
    prm("Pre1__par2", "reals", 3, tags = "t2"),
    prm("Pre2__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre2__par2", "reals", 3, tags = "t2")
  )
  p2 <- ParameterSet$new(
    prms,
    tag_properties = list(required = "t1", linked = "t2")
  )

  expect_equal(p1$rep(2, "Pre"), p2)
  expect_error(p1$rep(3, letters[1:2]), "either be")

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
  expect_error(p$add_dep("a", "b", cnd("eq", 1)), "failed")
  expect_error(p$add_dep("a", "a", cnd("eq", 1)), "themselves")
  expect_silent(p$add_dep("b", "a", cnd("eq", 1)))
  expect_error(p$add_dep("b", "a", cnd("eq", 1)), "already depends")
  p$values$b <- 3
  expect_error({ p$values$a <- NULL }, "failed") # nolint


  prms <- list(
    prm("Pre1__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre1__par2", "reals", 3, tags = "t2"),
    prm("Pre2__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre2__par2", "reals", 3, tags = "t2")
  )
  p2 <- ParameterSet$new(prms)
  expect_error(p2$add_dep("par1", "par2", cnd("any", 1:2)), "Dependency of")
  expect_silent(p2$add_dep("par1", "par2", cnd("eq", 3)))
  expect_error(p2$add_dep("Pre1", "Pre2", cnd("eq", 3)), "subset of")

  prms <- list(
    prm("a", "nreals", 1, tags = "t1"),
    prm("b", "nreals", 2, tags = "t1"),
    prm("d", "nreals", 3, tags = "t2")
  )
  p <- ParameterSet$new(prms)
  p$add_dep("a", "b", cnd("lt", id = "b"))
  expect_error({p$values$a <- 2}, "a < b") # nolint
  p$add_dep("a", "d", cnd("len", id = "d"))
  expect_error({p$values$d <- c(1, 2)}, "a len d") # nolint
  expect_error(p$add_dep("a", "d", cnd("len", id = "b")), "element of set")

  prms <- list(
    prm("a", "nreals", 1:2, tags = "t1"),
    prm("b", "nreals", 2, tags = "t1"),
    prm("d", "nreals", 3, tags = "t2")
  )
  p <- ParameterSet$new(prms)
  p$add_dep("b", "a", cnd("len", 2))
  expect_error({p$values$a <- 1}, "b on 'a") # nolint
t})

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
  expect_equal(c(p2, p3)$tag_properties, NULL)

  prms <- list(
    prm("a", "reals", 2),
    prm("b", "reals", 2),
    prm("d", "reals"),
    prm("e", "reals")
  )
  p <- ParameterSet$new(prms)
  p$add_dep("a", "b", cnd("neq", 1))

  p1 <- ParameterSet$new(list(prm("a", "reals", 2), prm("b", "reals", 2)))
  p1$add_dep("a", "b", cnd("neq", 1))
  p2 <- ParameterSet$new(list(prm("d", "reals"), prm("e", "reals")))
  p2$trafo <- function(x, self) {
    x$d <- 2
    x
  }
  expect_equal(expect_warning(c(p1, p2), "Transformations"), p)
})

test_that("extract - no deps", {
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
  expect_error(p$extract(), "One argument")

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
  expect_warning(p$extract("par1", prefix = "A"), "argument ignored")

  prms <- list(
    prm("Pre1__par1", Set$new(1), 1, tags = "t1")
  )
  p2 <- ParameterSet$new(prms)
  expect_equal(p$extract("Pre1__par1"), p2)

  prms <- list(
    prm("Pre1__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre1__par2", "reals", 3, tags = "t2"),
    prm("Pre2__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre2__par2", "reals", 3, tags = "t2")
  )
  p <- ParameterSet$new(prms, list(linked = "t1", required = "t2"))

  prms <- list(
    prm("par1", Set$new(1), 1, tags = "t1"),
    prm("par2", "reals", 3, tags = "t2")
  )
  p2 <- ParameterSet$new(prms, list(linked = "t1", required = "t2"))
  expect_equal(p$extract(prefix = "Pre1"), p2)

  prms <- list(
    prm("par1", Set$new(1)),
    prm("par2", "reals")
  )
  p <- ParameterSet$new(prms)
  p$trafo <- function(x, self) list(par1 = 1)
  expect_warning(p["par1"], "Transformations")
})

test_that("extract - deps", {
  prms <- list(
    prm("a", Set$new(1), 1, tags = "t1"),
    prm("b", "reals", tags = "t1"),
    prm("d", "reals", 2, tags = "t2")
  )
  p <- ParameterSet$new(prms)
  p$add_dep("b", "a", cnd("eq", 1))
  p$add_dep("a", "d", cnd("gt", 0))

  expect_equal(p$extract("a")$deps, NULL)
  expect_equal(p$extract(letters[1:2])$deps,
               data.table::data.table(id = "b", on = "a",
                                      cond = list(cnd("eq", 1))))

  prms <- list(
    prm("Pre1__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre1__par2", "reals", 3, tags = "t2"),
    prm("Pre2__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre2__par2", "reals", 3, tags = "t2")
  )
  p <- ParameterSet$new(prms)
  p$add_dep("Pre1__par1", "Pre1__par2", cnd("eq", 3))
  expect_equal(p$extract(prefix = "Pre1")$deps,
               data.table::data.table(id = "par1", on = "par2",
                                      cond = list(cnd("eq", 3))))

})

test_that("deep clone", {
  prms <- list(
    prm("a", Set$new(1), 1, tags = "t1"),
    prm("b", "reals", 1.5, tags = "t1"),
    prm("d", "reals", 2, tags = "t2")
  )
  p <- pset(prms)
  p$add_dep("a", "b", cnd("eq", 1.5))
  p2 <- p$clone(deep = TRUE)
  p2$values$d <- 3
  expect_true(p$values$d != p2$values$d)

  p3 <- p
  p3$values$d <- 3
  expect_true(p$values$d == p3$values$d)
})
