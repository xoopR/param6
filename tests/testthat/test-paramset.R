test_that("ParameterSet constructor - silent", {
  prms <- list(
    prm("a", Set$new(1), 1, "t1"),
    prm("b", "reals", NULL),
    prm("d", "reals", 2)
  )
  expect_R6_class(ParameterSet$new(prms), "ParameterSet")

  prms <- list(
    prm("a", Set$new(1), 1),
    prm("b", "reals"),
    prm("d", "reals")
  )
  expect_R6_class(ParameterSet$new(prms), "ParameterSet")

  expect_R6_class(ParameterSet$new(), "ParameterSet")

  expect_R6_class(as.ParameterSet(prm("a", "reals")), "ParameterSet")
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
  expect_equal(
    lapply(p$supports, as.character),
    lapply(list(
      a = Set$new(1, 2), b = Reals$new(),
      d = Reals$new()
    ), as.character)
  )
})

test_that("immutable parameters are immutable", {
  prms <- pset(
    prm("a", "reals", 1, tags = "immutable"),
    prm("b", "reals", 2)
  )
  expect_equal(get_private(prms)$.immutable, list(a = 1))
  prms$values$a <- NULL
  expect_equal(prms$values, list(a = 1, b = 2))
  prms$values$a <- 2
  expect_equal(prms$values, list(a = 1, b = 2))
  prms$values$b <- 2
  expect_equal(prms$values, list(a = 1, b = 2))
})

test_that("don't check immutable parameters", {
   prms <- pset(
     prm("a", "logicals", TRUE, tags = "immutable")
   )
   prms$values$a <- 1
   expect_equal(prms$values$a, TRUE)
})

test_that("can't set unknown parameters", {
   prms <- pset(
     prm("a", "logicals", TRUE, tags = "immutable")
   )
   expect_error(prms$values$b <- 1, "You can't")
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
  pri <- get_private(p)

  expect_false(expect_warning(
    .check(p, pri, supports = TRUE, deps = FALSE, tags = FALSE,
           error_on_fail = FALSE, value_check = list(a = 3),
           support_check = get_private(p)$.isupports)))

  p$add_dep("b", "a", cnd("eq", 1))
  expect_false(expect_warning(
    .check(p, pri, supports = FALSE, deps = TRUE, tags = FALSE,
           error_on_fail = FALSE, value_check = list(b = 1, a = 3),
           dep_check = p$deps)))

  expect_false(expect_warning(
    .check(p, pri, supports = FALSE, deps = FALSE, tags = TRUE,
           id = c("b", "d"),
           error_on_fail = FALSE, value_check = list(b = 1, d = 1),
           tag_check = p$tag_properties)))
  expect_error(p$values <- list(a = 1, b = 1, d = 1), "Multiple linked")


  prms <- list(
    prm("b", "naturals", 1),
    prm("d", "naturals", 2)
  )
  p <- ParameterSet$new(prms)
  expect_error(p$values <- list(b = 0.5, d = 0.5), "One or")

  prms <- list(
    prm("a", "nnaturals", 1)
  )
  p <- ParameterSet$new(prms)
  expect_silent(p$values$a <- 2)
  expect_silent(p$values <- list(a = c(1, 2)))
  expect_error(p$values <- list(a = c(1, 0.5)), "does not lie")

  p <- pset(
    prm("prob", Interval$new(0, 1), 0.5, "required"),
    prm("qprob", Interval$new(0, 1))
  )
  expect_error(p$values$prob <- NULL, "Not all required")
})

test_that("ParamSet actives - tag properties", {
  prms <- list(
    prm("a", Set$new(1, 2), 1, c("t1", "t2")),
    prm("b", "reals", 2, "t2"),
    prm("d", "reals", NULL, "t3")
  )
  p <- ParameterSet$new(prms, list(linked = "t1", required = "t2"))

  expect_equal(p$tag_properties, list(linked = "t1", required = "t2"))
  expect_silent(p$tag_properties <- NULL)
  expect_silent(p$tag_properties$required <- "t2")
  expect_error(p$tag_properties <- list(required = "t3"))
  expect_error(p$tag_properties <- list(linked = "t2"))

  prms <- list(
    prm("a", "nreals", 1, tags = "t1"),
    prm("b", "nreals", 3, tags = "t2")
  )
  p <- ParameterSet$new(prms, tag_properties = list(unique = "t1"))
  expect_silent(p$values$a <- 2)
  expect_silent(p$values <- list(a = c(1, 2)))
  expect_error(p$values <- list(a = c(2, 2)), "duplicated")

  p <- pset(
    prm("prob", Interval$new(0, 1), 0.5, "probs"),
    prm("qprob", Interval$new(0, 1), tags = "probs"),
    tag_properties = list(required = "probs", linked = "probs")
  )
  expect_error(p$values$qprob <- 0.1, "Multiple linked")
  expect_error(p$values$prob <- NULL, "Not all required")
  p$values <- list(prob = NULL, qprob = 0.1)
  expect_error(p$values$prob <- 0.1, "Multiple linked")
  expect_error(p$values$qprob <- NULL, "Not all required")
  expect_equal(p$values, list(qprob = 0.1))
})

test_that("as.data.table.ParameterSet and print", {
  prms <- list(
    prm("a", Set$new(1), 1, c("t1", "t2")),
    prm("b", "reals", NULL),
    prm("d", "reals", 2)
  )
  p <- ParameterSet$new(prms)
  expect_equal_ps(as.data.table(p),
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
  expect_equal_ps(as.ParameterSet(dt), ParameterSet$new(prms))

  prms <- list(
    prm("a", "naturals", 1, c("t1", "t2")),
    prm("b", "reals", NULL),
    prm("d", "reals", 2)
  )
  dt <- data.table::data.table(Id = letters[c(1, 2, 4)],
                               Support = list("naturals", "reals", "reals"),
                               Value = list(1, NULL, 2),
                               Tags = list(c("t1", "t2"), NULL, NULL))
  expect_equal_ps(as.ParameterSet(dt), ParameterSet$new(prms))
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
  expect_equal(p$get_values("Pre1__"), list(Pre1__par1 = 1, Pre1__par2 = 2))
  expect_equal(p$get_values(c("Pre1__", "Pre2__")),
               list(Pre1__par1 = 1, Pre1__par2 = 2, Pre2__par1 = 1,
                    Pre2__par2 = NULL))
  expect_equal(p$get_values("par1"), list(Pre1__par1 = 1, Pre2__par1 = 1))

  p <- pset(
      prm("elements", "universal", 1),
      prm("probs", Interval$new(0, 1)^"n", 1)
    )
  p$values <- list(elements = c(1, 0), probs = c(0.4, 0.9))
  expect_equal(p$values, list(elements = c(1, 0), probs = c(0.4, 0.9)))
  expect_equal(p$get_values(), list(elements = c(1, 0), probs = c(0.4, 0.9)))
})

test_that("trafo", {
  prms <- list(
    prm("a", Set$new(1, 2), 1, tags = "t1"),
    prm("b", "reals", tags = "t1"),
    prm("d", "reals", tags = "t2")
  )
  p <- ParameterSet$new(prms)
  expect_equal(p$trafo, NULL)
  expect_equal(get_private(p)$.trafo, NULL)
  expect_error(p$trafo <- "a", "function")
  expect_error(p$trafo <- function(x, self) "a", "list")
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
  }, "One or more")
  expect_equal(p$get_values(inc_null = FALSE), list(a = 2, b = 3))

  prms <- list(
    prm("a", Set$new(1, exp(1)), 1, tags = "t1"),
    prm("b", "reals", 2, tags = "t1"),
    prm("d", "reals", tags = "t2")
  )
  p <- ParameterSet$new(prms)
  p$trafo <- function(x, self) {
    x <- lapply(self$get_values(tags = "t1", transform = FALSE), exp)
    x
  }
  expect_equal(p$get_values(inc_null = FALSE), list(a = exp(1), b = exp(2)))

  p <- ParameterSet$new(
  list(prm(id = "a", 2, support = Reals$new(), tags = "t1"),
       prm(id = "b", 3, support = Reals$new(), tags = "t1"),
       prm(id = "d", 4, support = Reals$new()))
)
  p$trafo <- function(x, self) {
    out <- lapply(
      self$get_values(tags = "t1", transform = FALSE),
      function(.x) 2^.x
    )
    out <- c(out, list(d = x$d))
    out
  }
  expect_equal(p$get_values(), list(a = 4, b = 8, d = 4))

  p <- pset(
    prm("prob", Interval$new(0, 1), 0.5, "probs"),
    prm("qprob", Interval$new(0, 1), tags = "probs"),
    tag_properties = list(linked = "probs"),
    trafo = function(x, self) {
      if (is.null(x$prob)) {
        x$prob <- 1 - x$qprob
      }
      if (is.null(x$qprob)) {
        x$qprob <- 1 - x$prob
      }
      x
    }
  )
  p$values$prob <- 0.2
  expect_equal(p$get_values(), list(prob = 0.2, qprob = 0.8))
})

test_that("rep", {
  prms <- list(
    prm("par1", Set$new(1), 1, tags = "t1"),
    prm("par2", "reals", 3, tags = "t2"),
    prm("par3", "reals", 4, tags = "immutable")
  )
  p1 <- ParameterSet$new(prms, tag_properties = list(required = "t1",
                                                     linked = "t2"))

  prms <- list(
    prm("Pre1__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre1__par2", "reals", 3, tags = "t2"),
    prm("Pre1__par3", "reals", 4, tags = "immutable"),
    prm("Pre2__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre2__par2", "reals", 3, tags = "t2"),
    prm("Pre2__par3", "reals", 4, tags = "immutable")
  )
  p2 <- ParameterSet$new(
    prms,
    tag_properties = list(required = "t1", linked = "t2")
  )

  expect_equal_ps(p1$rep(2, "Pre"), p2)
  expect_error(p1$rep(3, letters[1:2]), "either be")

  prms <- list(
    prm("par1", Set$new(1), 1, tags = "t1"),
    prm("par2", "reals", 3, tags = "t2"),
    prm("par3", "reals", 4, tags = "immutable")
  )
  p1 <- ParameterSet$new(prms, tag_properties = list(required = "t1",
                                                     linked = "t2"))
  expect_equal_ps(rep(p1, 2, "Pre"), p2)
  expect_equal(length(p1), 3)
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
  expect_error(p$values$a <- NULL, "failed")


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
  expect_error(p$values$a <- 2, "a < b")
  p$add_dep("a", "d", cnd("len", id = "d"))
  expect_error(p$values$d <- c(1, 2), "a len d")
  expect_error(p$add_dep("a", "d", cnd("len", id = "b")), "element of set")

  prms <- list(
    prm("a", "nreals", 1:2, tags = "t1"),
    prm("b", "nreals", 2, tags = "t1"),
    prm("d", "nreals", 3, tags = "t2")
  )
  p <- ParameterSet$new(prms)
  p$add_dep("b", "a", cnd("len", 2))
  expect_error(p$values$a <- 1, "b on 'a")
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
  p$trafo <- function(x, self) {
    x$d <- 2
    x
  }

  p1 <- ParameterSet$new(list(prm("a", "reals", 2), prm("b", "reals", 2)))
  p1$add_dep("a", "b", cnd("neq", 1))
  p2 <- ParameterSet$new(list(prm("d", "reals"), prm("e", "reals")))
  p2$trafo <- function(x, self) {
    x$d <- 2
    x
  }
  expect_equal_ps(c(p1, p2), p)
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
  expect_equal_ps(p$extract(prefix = "Pre1"), p2)
  expect_error(p$extract(), "One argument")

  prms <- list(
    prm("Pre1__par1", Set$new(1), 1),
    prm("Pre1__par2", "reals", 3),
    prm("Pre2__par1", Set$new(1), 1),
    prm("Pre2__par2", "reals", 3)
  )
  p3 <- ParameterSet$new(prms)
  prms <- list(
    prm("par1", Set$new(1), 1),
    prm("par2", "reals", 3)
  )
  p4 <- ParameterSet$new(prms)
  expect_equal_ps(p3$extract(prefix = "Pre1"), p4)

  prms <- list(
    prm("Pre1__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre2__par1", Set$new(1), 1, tags = "t1")
  )
  p2 <- ParameterSet$new(prms)
  expect_equal_ps(p$extract("par1"), p2)
  expect_warning(p$extract("par1", prefix = "A"), "argument ignored")

  prms <- list(
    prm("Pre1__par1", Set$new(1), 1, tags = "t1")
  )
  p2 <- ParameterSet$new(prms)
  expect_equal_ps(p$extract("Pre1__par1"), p2)

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
  expect_equal_ps(p$extract(prefix = "Pre1"), p2)

  prms <- list(
    prm("par1", Set$new(1)),
    prm("par2", "reals")
  )
  p <- ParameterSet$new(prms)
  p$trafo <- function(x, self) list(par1 = 1)
  expect_warning(p["par1", keep_trafo = FALSE], "Transformations")
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
  p <- pset(
    prm("a", Set$new(1), 1, tags = "t1"),
    prm("b", "reals", 1.5, tags = "t1"),
    prm("d", "reals", 2, tags = "t2")
  )
  p$add_dep("a", "b", cnd("eq", 1.5))
  p2 <- p$clone(deep = TRUE)
  p2$values$d <- 3
  expect_true(p$values$d != p2$values$d)

  p3 <- p
  p3$values$d <- 3
  expect_true(p$values$d == p3$values$d)
})


test_that("transformations error when expected and don't otherwise", {
  trafo <-  function(x, self) {
      size <- ifelse(is.null(x$size), self$values$size, x$size)
      if (!is.null(x$successes)) {
        x$failures <- size - x$successes
      } else if (!is.null(x$failures)) {
        x$successes <- size - x$failures
      }
      x
    }

  p <- pset(
    prm("size", "naturals", 50),
    prm("successes", Set$new(0:50, class = "integer")),
    prm("failures", Set$new(0:50, class = "integer"), 45),
    prm("draws", Set$new(0:50, class = "integer"), 10, tags = "required"),
    deps = list(
      list(id = "successes", on = "size", cond = cnd("leq", id = "size")),
      list(id = "failures", on = "size", cond = cnd("leq", id = "size"))
    ),
    trafo = trafo
  )
  p$values <- list(size = 40, failures = 2, draws = 5)
  expect_equal(p$values, list(draws = 5, failures = 2, size = 40))
  expect_error(p$values$failures <- 60, "does not lie in")
  expect_equal(p$trafo, trafo)

  trafo_bad <-  function(x, self) {
      x$failures <- Inf
      x
  }

  expect_error(p$trafo <- trafo_bad, "does not lie in")
  expect_equal(p$trafo, trafo)
})


test_that("transform types", {
  trafo_a <- function(x, self) {
    x$a <- x$a + 1
    x
  }
  trafo_b <- function(x, self) {
    x$b <- x$b + 1
    x
  }
  p <- pset(
    prm("a", "reals", 2),
    prm("b", "reals", 1),
    trafo = trafo_a
  )
  expect_equal(p$transform(), list(a = 3, b = 1))

  p$trafo <- list(trafo_a, trafo_b)
  expect_equal(p$transform(), list(a = 3, b = 2))
  expect_equal(p$transform(p$values), list(a = 3, b = 2))

  p$trafo <- NULL
  expect_equal(p$transform(), list(a = 2, b = 1))
})


test_that("rep cnd works", {
  p <- pset(
    prm("elements", "universal", 1, tags = "required"),
    prm("probs", Interval$new(0, 1)^"n", 1, tags = "required"),
    deps = list(
      list(id = "probs", on = "elements", cond = cnd("len", id = "elements"))
    )
  )$rep(2, "A")
  expect_error(p$values$A1__elements <- 1:2) # nolint
  new_p <- list(A1__elements = 1:2, A1__probs = runif(2), A2__elements = 1,
                A2__probs = 1)
  p$values <- new_p
  expect_equal(p$values, new_p)
})


test_that("can extract with trafo, properties, deps", {
  trafo_probs <- function(x, self) {
    probs <- x[grepl("prob", names(x))]
    qprobs <- x[grepl("qprob", names(x))]
    c(x,
      setNames(
        as.list(1 - unlist(probs)),
        gsub("prob", "qprob", names("prob"))
      )
    )
  }
  p <- pset(
    prm("prob", Interval$new(0, 1), 0.5, tags = c("probs", "r")),
    prm("qprob", Interval$new(0, 1), tags = c("probs", "r")),
    prm("size", "posnaturals", 10, tags = "r"),
    tag_properties = list(linked = "probs", required = "r"),
    trafo = trafo_probs,
    deps = list(
      list(id = "prob", on = "size", cond = cnd("len", id = "size")),
      list(id = "qprob", on = "size", cond = cnd("len", id = "size"))
    )
  )
  p2 <- p$clone(deep = TRUE)$rep(2, "p")
  p_ext <- p2[prefix = "p1"]
  expect_equal_ps(p, p_ext)
})


test_that("concatenate named list", {
  p <- pset(
    prm("a", "reals", 1),
    prm("b", "reals", 1)
  )
  lst <- list(a = p, b = p$clone(deep = TRUE))
  cp <- cpset(pss = lst)

  pexp <- pset(
    prm("a__a", "reals", 1),
    prm("a__b", "reals", 1),
    prm("b__a", "reals", 1),
    prm("b__b", "reals", 1)
  )

  expect_equal_ps(cp, pexp)
})


test_that("linked + required works as expected", {
  p <- pset(
    prm("prob", Interval$new(0, 1), 1, tags = c("linked", "required")),
    prm("qprob", Interval$new(0, 1), tags = c("linked", "required")),
    prm("size", "posnaturals", 10, tags = "required")
  )
  expect_error(p$values$prob <- NULL, "Not all required")
  expect_error(p$values <- list(size = 10, prob = NULL), "Not all required")
  p$values <- list(size = 1, prob = NULL, qprob = 1)
  expect_equal(p$values, list(qprob = 1, size = 1))
})


test_that("can update support", {
  p <- pset(
    prm("a", "reals", 1),
    prm("b", "reals", 1)
  )
  sup <- list(a = Interval$new(0, 5), b = Interval$new(1, 3))
  get_private(p)$.update_support(lst = sup)
  expect_equal(as.character(p$supports), as.character(sup))
})


test_that("can remove a parameter", {
  p1 <- pset(
    prm("a", "reals", 1),
    prm("b", "reals", 1)
  )
  p2 <- pset(
    prm("a", "reals", 1)
  )
  p3 <- pset(
    prm("c__a", "posreals", 1, c("required", "immutable")),
    prm("d__b", "reals", 1),
    trafo = list(c__a = function(x, self) x),
    deps = list(list(id = "c__a", on = "d__b", cond = cnd("eq", 1)))
  )
  p4 <- pset(
    prm("d__b", "reals", 1)
  )

  expect_equal_ps(p3$remove(prefix = "c"), p4)
  expect_equal_ps(p1$remove("b"), p2)
})


test_that("set_values", {
  p <- pset(
    prm("a", "reals", 1),
    prm("b", "reals", 1)
  )
  p$set_values(b = 2)
  expect_equal(p$values, list(a = 1, b = 2))
})
