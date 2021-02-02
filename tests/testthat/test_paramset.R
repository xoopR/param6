#----
p1 <- ParameterSet$new(
  splitrule = Set$new("logrank", "extratrees", "C", "maxstat") ~ tags("train", "predict") +
    "logrank",
  ntrees = PosIntegers$new(),
  sample.fraction = Interval$new(0, 1) ~ 0.5
)
p2 <- ParameterSet$new(
  splitrule = Set$new("logrank", "extratrees", "C", "maxstat") ~ tags("train", "predict") +
    "logrank",
  sample.fraction = Interval$new(0, 1) ~ 0.5
)
p3 <- ParameterSet$new(ntrees = PosIntegers$new() ~ 2)
#----

test_that("different constructors", {
  expect_equal(
    ParameterSet$new(r = Reals$new(), n = Naturals$new() ~ 1, l = Logicals$new() ~ FALSE +
      tags(log)),
    ParameterSet$new(
      id = list("r", "n", "l"),
      support = list(Reals$new(), Naturals$new(), Logicals$new()),
      value = list(NULL, 1, FALSE), tag = list(NULL, NULL, "log")
    )
  )
})

test_that("constructor", {
  expect_error(ParameterSet$new(a))
  expect_error(ParameterSet$new(a = 2), "This is not")
  expect_error(ParameterSet$new(a = "1"), "This is not")
  expect_silent(ParameterSet$new(a = Set$new() ~ tags(train)))
  expect_silent(ParameterSet$new(a = Set$new()))
  expect_silent(ParameterSet$new(a = Set$new(1) ~ tags(train) + 1))
  expect_error(ParameterSet$new(a = Set$new(1) ~ tags(train) + 2), "does not lie in")
  expect_silent(ParameterSet$new(a = Set$new(1) ~ tags(train), b = Set$new(2) ~ tags("train")))
  expect_silent(ParameterSet$new(
    a = Set$new(1) ~ 1,
    b = Set$new(2) ~ tags(predict),
    c = Set$new(3) ~ tags(train, predict)
  ))
  expect_silent(ParameterSet$new(a = Set$new(1) ~ tags(train) + 1, b = Set$new(2) ~ 2))
})

test_that("print", {
  expect_output(p1$print())
  expect_output(p1$print(NULL))
  expect_output(p1$print(hide_cols = "Id"))
  expect_error(p1$print(hide_cols = "sdsd"))
  p <- ParameterSet$new(a = Logicals$new(), b = Logicals$new())
  p$add_trafo("a", exp)
  p$add_dep("a", "b", "Equal", FALSE)
  expect_output(p$print(hide_cols = NULL))
})

test_that("values", {
  expect_silent({
    p1$values$asas <- 1
  })
  expect_silent({
    p1$values$ntrees <- 1
  })
  expect_silent({
    p1$values$ntrees <- NULL
  })
  expect_equal(p1$values$ntrees, NULL)
  expect_equal(p1$values$sample.fraction, 0.5)
  expect_silent({
    p1$values$ntrees <- 1
  })
  expect_equal(p1$values$ntrees, 1)
  expect_silent({
    p1$values <- list(splitrule = "C", sample.fraction = 0.1, ntrees = 2)
  })
  expect_error({p1$values$ntrees <- -5}, "does not lie in") # nolint
})

test_that("get_values", {
  support <- rep(list(PosIntegers$new()), 5)
  names(support) <- letters[1:5]
  p <- ParameterSet$new(
    support = support,
    value = list(1, 2, NULL, 4, NULL),
    tag = list("train", c("train", "predict"), NULL, NULL, "predict")
  )
  expect_equal(p$values, list(a = 1, b = 2, d = 4))
  expect_equal(p$get_values(), list(a = 1, b = 2, d = 4))
  expect_equal(p$get_values(tag = "predict"), list(b = 2))
  expect_equal(p$get_values(tag = "train"), list(a = 1, b = 2))
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

test_that("add", {
  expect_error(p2$add(splitrule = Set$new(1)), "duplicated")
  expect_equal_ParameterSet(p2$add(ntrees = PosIntegers$new() ~ 2), p1)
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
                          Tag = list(NULL, NULL, c("train", "predict"))))
})

test_that("as.ParameterSet", {
  expect_error(as.ParameterSet(data.table(Id = "a", Support = Set$new(), Val = 2,
                                          Tag = list(NULL))),
               "colnames")
  expect_error(as.ParameterSet(data.table(
    Id = c("a", "a"),
    Support = c(Set$new(), Set$new(2)),
    Value = c(2, NULL), Tag = c("train", "train")
  )), "unique names")
  expect_error(as.ParameterSet(data.table(
    Id = c("a", "b"),
    Support = c(Set$new(), Set$new(2)),
    Value = list(2, NULL),
    Tag = list("train", "predict")
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
  expect_error(ParameterSet$new(support = Set$new(1), value = 2, tag = "train"), "names")
  expect_error(ParameterSet$new(support = list(Set$new(1)), value = list(2), tag = list("train")),
               "Must have names")
  expect_silent(ParameterSet$new(support = list(a = Set$new(1)), value = list(1),
                                 tag = list("train")))
  expect_error(ParameterSet$new(support = list(a = Set$new(1)), value = list(2),
                                tag = list("train")),
               "does not lie")
  expect_error(ParameterSet$new(support = list(a = Set$new(1)), value = list(2, 0),
                            tag = list("train")), "length")
  p5 <- ParameterSet$new(support = list(a = Set$new(1)), value = list(1), tag = list("train"))
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
