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
  expect_equal(p$get_values(inc.null = TRUE), list(a = 1, b = NULL, c = NULL))
  expect_equal(p$get_values(inc.null = FALSE), list(a = 1))

  expect_equal(p$get_values(inc.null = TRUE, tags = "t1"), list(a = 1, b = NULL))
  expect_equal(p$get_values(inc.null = FALSE, tags = "t1"), list(a = 1))

  expect_equal(p$get_values(inc.null = TRUE, tags = "t2"), list(c = NULL))
  expect_equal(p$get_values(inc.null = FALSE, tags = "t2"), named_list())

})

test_that("supports", {
  expect_equal(p1$supports, list(
    splitrule = Set$new("logrank", "extratrees", "C", "maxstat"),
    ntrees = PosIntegers$new(),
    sample.fraction = Interval$new(0, 1)
  ))
})

test_that("ids", {
  expect_equal(p1$ids, list("splitrule", "ntrees", "sample.fraction"))
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

test_that("as.data.table", {
  expect_equal(as.data.table(p1),
               data.table(Id = c("ntrees", "sample.fraction", "splitrule"),
                          Support = list(PosIntegers$new(), Interval$new(0, 1),
                                         Set$new("logrank", "extratrees", "C", "maxstat")),
                          Value = list(2, 0.1, "C"),
                          Tags = list(NULL, NULL, c("train", "predict"))))
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

test_that("alt constructor", {
  expect_error(ParameterSet$new(support = list()), "have names")
  expect_error(ParameterSet$new(support = Set$new(1), value = 2, tags = "train"), "names")
  expect_error(ParameterSet$new(support = list(Set$new(1)), value = list(2), tags = list("train")),
               "Must have names")
  expect_silent(ParameterSet$new(support = list(a = Set$new(1)), value = list(1),
                                 tags = list("train")))
  expect_error(ParameterSet$new(support = list(a = Set$new(1)), value = list(2),
                                tags = list("train")),
               "does not lie")
  expect_error(ParameterSet$new(support = list(a = Set$new(1)), value = list(2, 0),
                            tags = list("train")), "length")
  p5 <- ParameterSet$new(support = list(a = Set$new(1)), value = list(1), tags = list("train"))
  expect_equal(p5$ids, "a")
  expect_equal(p5$supports, list(a = Set$new(1)))
  expect_equal(p5$values, list(a = 1))
  expect_equal(p5$tags, list(a = "train"))
})

test_that("subset", {
  expect_equal_ParameterSet(p1$subset("ntrees"), p3)
})

test_that("length", {
  expect_equal(p1$length, 1)
  expect_equal(p2$length, 3)
})

test_that("deps", {
  expect_equal(p2$deps, data.table(id = character(0L), on = character(0L), type = character(0L),
                                   cond = list()))
  expect_silent(p2$add_dep("splitrule", "sample.fraction", "Equal", 0.2))
  expect_error(p2$add_dep("splitrule", "sample.fraction", "Equal", 0.2), "already depends")
  expect_equal(p2$deps, data.table(id = "splitrule", on = "sample.fraction", type = "Equal",
                                   cond = list(0.2)))
  expect_silent(p2$add_dep("ntrees", "sample.fraction", "Equal", 0.2))
  expect_error(p2$add_dep("splitrule", "splitrule", "Equal", 0.1), "Parameters cannot depend")
  expect_error(p2$add_dep("splitrule", "sfdsf", "Equal", 0.1), "Must be a subset")
})

test_that("trafo", {
  p <- ParameterSet$new(
    a = Reals$new() ~ 0.5,
    b = Integers$new() ~ 2,
    c = Logicals$new() ~ FALSE
  )
  expect_equal(p$trafos, list())
  expect_silent({
    p$add_trafo("a", round)
  })
  expect_equal(p$trafos, list(a = round))
  expect_silent({
    p$add_trafo("a", exp)
  })
  expect_error(p$add_trafo("d", exp), "not available")
  expect_silent({
    p$add_trafo("c", function(x) x^2)
  })
  expect_error(p$add_trafo("<Set>", fun = exp), "formal arguments")
  expect_error(p$add_trafo("<Set>", fun = function(param_set) exp), "formal arguments")
  expect_silent(p$add_trafo("<Set>", fun = function(param_set, x) exp))
  expect_equal(length(p$trafos), 4)
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
