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

  expect_error(prm("a", "Reals", 1, "a"), "does not exist")
  expect_error(prm("a", 1, 1, "a"), "character scalar")

  expect_equal(class(prm("a", "reals")), "prm6")
})

test_that("ParameterSet constructor - silent", {
  prms <- list(
    prm("a", Set$new(1), 1, "a"),
    prm("b", "reals", NULL),
    prm("c", "reals", 2)
  )
  expect_silent(ParameterSet$new(prms))

  prms <- list(
    prm("a", Set$new(1), 1),
    prm("b", "reals"),
    prm("c", "reals")
  )
  expect_silent(ParameterSet$new(prms))

  expect_silent(ParameterSet$new())
})

test_that("ParameterSet constructor - error", {

  prms <- list(
    prm("a", Set$new(1), 1, "a"),
    prm("a", "reals", NULL),
    prm("c", "reals", 2)
  )
  expect_error(ParameterSet$new(prms), "ids are not unique")

  prms <- list(
    prm("a", Set$new(1), 1, c("a", "c")),
    prm("b", "reals", NULL),
    prm("c", "reals", 2)
  )
  expect_error(ParameterSet$new(prms), "'c' is a")
})

test_that("ParamSet actives - not values", {
  prms <- list(
    prm("a", Set$new(1, 2), 1, c("a", "b")),
    prm("b", "reals", NULL, "d"),
    prm("c", "reals", 2)
  )
  p <- ParameterSet$new(prms)

  expect_equal(p$tags, list(a = c("a", "b"), b = "d"))
  expect_equal(p$ids, letters[1:3])
  expect_equal(length(p), 3)
  expect_equal(p$supports, list(a = Set$new(1, 2), b = Reals$new(), c = Reals$new()))
})

test_that("ParamSet actives - values", {
  prms <- list(
    prm("a", Set$new(1, 2), 1, c("a", "b")),
    prm("b", "reals", NULL, "d"),
    prm("c", "reals", 2)
  )
  p <- ParameterSet$new(prms)

  expect_equal(p$values, list(a = 1, c = 2))
  expect_silent(p$values$a <- 2)
  expect_equal(p$values$a, 2)
  expect_error(p$values$a <- 3, "3 does not")
  expect_equal(p$values$a, 2)
  expect_error(p$values <- list(a = 3, b = 1, c = 1), "3 does not")
  expect_equal(p$values, list(a = 2, c = 2))
  expect_silent(p$values <- list(a = 1))
  expect_equal(p$values, list(a = 1))
  expect_silent(p$values$a <- NULL)
})

test_that("as.data.table.ParameterSet and print", {
  prms <- list(
    prm("a", Set$new(1), 1, letters[1:2]),
    prm("b", "reals", NULL),
    prm("c", "reals", 2)
  )
  p <- ParameterSet$new(prms)
  expect_equal(as.data.table(p),
    data.table::data.table(Id = letters[1:3],
      Support = list(Set$new(1), Reals$new(), Reals$new()),
      Value = list(1, NULL, 2),
      Tags = list(letters[1:2], NULL, NULL)))

  expect_output(print(p))
})

test_that("add - error", {
  prms <- list(
    prm("a", Set$new(1), 1, letters[1:2]),
    prm("b", "reals", NULL),
    prm("c", "reals", 2)
  )
  p <- ParameterSet$new(prms)
  expect_error(p$add(), "At least")
  expect_error(p$add(list(prm("c", "reals", 2))), "ids are not")
})

test_that("add - silent", {
  prms <- list(
    prm("a", Set$new(1), 1, letters[1:2]),
    prm("b", "reals", NULL),
    prm("c", "reals", 2)
  )
  p <- ParameterSet$new(prms)
  expect_silent(p$add(list(
    prm("d", "reals", 2),
    prm("e", Set$new(3), 3, tags = c("a", "d"))
  )))
  expect_equal(as.data.table(p),
    data.table(Id = letters[1:5],
               Support = list(Set$new(1), Reals$new(), Reals$new(), Reals$new(), Set$new(3)),
               Value = list(1, NULL, 2, 2, 3),
               Tags = list(letters[1:2], NULL, NULL, NULL, c("a", "d"))))
})

test_that("get_values", {
 prms <- list(
    prm("a", Set$new(1), 1, tags = "t1"),
    prm("b", "reals", tags = "t1"),
    prm("c", "reals", tags = "t2")
  )
  p <- ParameterSet$new(prms)
  expect_equal(p$get_values(inc_null = TRUE), list(a = 1, b = NULL, c = NULL))
  expect_equal(p$get_values(inc_null = FALSE, simplify = FALSE), list(a = 1))

  expect_equal(p$get_values(inc_null = TRUE, tags = "t1"), list(a = 1, b = NULL))
  expect_equal(p$get_values(inc_null = FALSE, tags = "t1"), 1)
  expect_equal(p$get_values(inc_null = FALSE, tags = "t1", simplify = FALSE), list(a = 1))

  expect_equal(p$get_values(inc_null = TRUE, tags = "t2"), NULL)
  expect_equal(p$get_values(inc_null = FALSE, tags = "t2", simplify = FALSE), named_list())

  prms <- list(
    prm("Pre1__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre1__par2", "reals", 2, tags = "t2"),
    prm("Pre2__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre2__par2", "reals", tags = "t2")
  )
  p <- ParameterSet$new(prms)
  expect_equal(p$get_values("Pre1"), list(Pre1__par1 = 1, Pre1__par2 = 2))
  expect_equal(p$get_values(c("Pre1", "Pre2")),
    list(Pre1__par1 = 1, Pre1__par2 = 2, Pre2__par1 = 1, Pre2__par2 = NULL))
  expect_equal(p$get_values("par1"), list(Pre1__par1 = 1, Pre2__par1 = 1))
})

test_that("trafo", {
  prms <- list(
    prm("a", Set$new(1), 1, tags = "t1"),
    prm("b", "reals", tags = "t1"),
    prm("c", "reals", tags = "t2")
  )
  p <- ParameterSet$new(prms)
  expect_equal(p$trafo, NULL)
  expect_error({p$trafo <- "a"}, "function")
  expect_error({p$trafo <- function(x) "a"}, "list")
  expect_silent({
    p$trafo <- function(x) {
      x$a = x$a + 1
      x$b = 3
      x
    }
  })
  expect_equal(p$get_values(inc_null = FALSE), list(a = 2, b = 3))
})

test_that("rep", {
  prms <- list(
    prm("par1", Set$new(1), 1, tags = "t1"),
    prm("par2", "reals", 3, tags = "t2")
  )
  p1 <- ParameterSet$new(prms)

  prms <- list(
    prm("Pre1__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre1__par2", "reals", 3, tags = "t2"),
    prm("Pre2__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre2__par2", "reals", 3, tags = "t2")
  )
  p2 <- ParameterSet$new(prms)

  expect_equal(p1$rep(2, "Pre"), p2)

  prms <- list(
    prm("par1", Set$new(1), 1, tags = "t1"),
    prm("par2", "reals", 3, tags = "t2")
  )
  p1 <- ParameterSet$new(prms)
  expect_equal(rep(p1, 2, "Pre"), p2)
  expect_equal(length(p1), 2)
})

test_that("add_dep", {
  prms <- list(
    prm("a", Set$new(1), 1, tags = "t1"),
    prm("b", "reals", tags = "t1"),
    prm("c", "reals", tags = "t2")
  )
  p <- ParameterSet$new(prms)
  expect_error(p$add_dep("a", "b", cnd(1, "eq")), "failed")
  expect_silent(p$add_dep("b", "a", cnd(1, "eq")))
  p$values$b <- 3
  expect_error({ p$values$a <- NULL }, "failed")


  prms <- list(
    prm("Pre1__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre1__par2", "reals", 3, tags = "t2"),
    prm("Pre2__par1", Set$new(1), 1, tags = "t1"),
    prm("Pre2__par2", "reals", 3, tags = "t2")
  )
  p2 <- ParameterSet$new(prms)
  expect_error(p2$add_dep("par1", "par2", cnd(1:2, "any")), "Dependency of")
  expect_silent(p2$add_dep("par1", "par2", cnd(3, "eq")))
  expect_error(p2$add_dep("Pre1", "Pre2", cnd(3, "eq")), "Single dependency")
})



test_that("rbind", {
  p2$values <- list(splitrule = "C", sample.fraction = 0.1)
  expect_equal(as.data.table(rbind(p3, p2)), as.data.table(p1))
  expect_error(rbind(p3, ParameterSet$new(ntrees = LogicalSet$new())), "Must have unique")
})



test_that("remove", {
  p4 <- ParameterSet$new(
    a = Set$new("a") ~ "a", b = Set$new("b") ~ "b",
    c = Set$new("c") ~ "c", d = Set$new("d") ~ "d"
  )
  expect_equal(
    p4$remove("a", "b"),
    ParameterSet$new(c = Set$new("c") ~ "c", d = Set$new("d") ~ "d")
  )

  p4 <- ParameterSet$new(
    a = Set$new("a") ~ "a", b = Set$new("b") ~ "b",
    c = Set$new("c") ~ "c", d = Set$new("d") ~ "d"
  )
  expect_equal(
    p4$remove(c("a", "b")),
    ParameterSet$new(c = Set$new("c") ~ "c", d = Set$new("d") ~ "d")
  )

  p4 <- ParameterSet$new(
    a = Set$new("a") ~ "a", b = Set$new("b") ~ "b",
    c = Set$new("c") ~ "c", d = Set$new("d") ~ "d"
  )
  expect_equal(
    p4$remove(list("a", "g")),
    ParameterSet$new(b = Set$new("b") ~ "b", c = Set$new("c") ~ "c", d = Set$new("d") ~ "d")
  )

  p4 <- ParameterSet$new(
    a = Set$new("a") ~ "a", b = Set$new("b") ~ "b",
    c = Set$new("c") ~ "c", d = Set$new("d") ~ "d"
  )
  expect_equal(p4$remove("e"), p4)
})


test_that("as.ParameterSet", {
  expect_error(as.ParameterSet(data.table(Id = "a", Support = Set$new(), Val = 2,
                                          Tags = list(NULL))),
               "colnames")
  expect_error(as.ParameterSet(data.table(
    Id = c("a", "a"),
    Support = c(Set$new(), Set$new(2)),
    Value = c(2, NULL), Tags = c("train", "train")
  )), "unique names")
  expect_error(as.ParameterSet(data.table(
    Id = c("a", "b"),
    Support = c(Set$new(), Set$new(2)),
    Value = list(2, NULL),
    Tags = list("train", "predict")
  )), "does not lie")
  expect_equal(
    as.ParameterSet(data.table(
      Id = c("a", "b"),
      Support = list(Set$new(), Set$new(2)),
      Value = list(NULL, 2)
    )),
    ParameterSet$new(
      "a" = Set$new(),
      "b" = Set$new(2) ~ 2
    )
  )
})

test_that("subset", {
  expect_equal_ParameterSet(p1$subset("ntrees"), p3)
})


test_that("deep_clone", {
  a <- ParameterSet$new(lgl = LogicalSet$new() ~ TRUE)
  b <- a
  b$values$lgl <- FALSE
  expect_false(a$values$lgl)

  a <- ParameterSet$new(lgl = LogicalSet$new() ~ TRUE)
  b <- a$clone()
  b$values$lgl <- FALSE
  expect_true(a$values$lgl)
  b$values$lgl <- TRUE
  expect_equal(a, b)
})

test_that("checks", {
  p <- ParameterSet$new(
    a = Reals$new() ~ 0.5,
    b = Integers$new() ~ 2,
    c = Logicals$new() ~ FALSE
  )
  expect_equal(p$checks, NULL)
  expect_error(p$add_check("d", function(self) a == 1), "subset")
  expect_error(p$add_check("a", function(elf) a == 1), "formal")
  expect_silent(p$add_check("a", function(self) self$values$a == 0.5))
  expect_equal(p$checks, data.table(params = list("a"), fun = list(body(function(x) self$values$a == 0.5))))
  expect_true(p$check())
  expect_silent(p$add_check(c("a", "b"), function(self) self$values$a + self$values$b == 2.5))
  expect_true(p$check())
  expect_silent(p$add_check(c("c"), function(self) self$values$c == TRUE))
  expect_false(p$check())
})
