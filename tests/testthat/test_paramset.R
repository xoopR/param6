context("ParamSet")

test_that("constructor",{
  expect_error(ParamSet$new(), "must be constructed")
  expect_error(ParamSet$new(a))
  expect_error(ParamSet$new(a = 2), "This is not")
  expect_error(ParamSet$new(a = "1"), "This is not")
  expect_silent(ParamSet$new(a = Set$new() ~ tags(train)))
  expect_silent(ParamSet$new(a = Set$new()))
  expect_silent(ParamSet$new(a = Set$new(1) ~ tags(train) + 1))
  expect_error(ParamSet$new(a = Set$new(1) ~ tags(train) + 2), "does not lie in")
  expect_silent(ParamSet$new(a = Set$new(1) ~ tags(train), b = Set$new(2) ~ tags("train")))
  expect_silent(ParamSet$new(a = Set$new(1) ~ 1,
                             b = Set$new(2) ~ tags(predict),
                             c = Set$new(3) ~ tags(train, predict)))
  expect_silent(ParamSet$new(a = Set$new(1) ~ tags(train) + 1, b = Set$new(2) ~ 2))
})

p = ParamSet$new(ntrees = PosIntegers$new(),
                 splitrule = Set$new("logrank","extratrees","C","maxstat") ~ tags("train","predict") + "logrank",
                 sample.fraction = Interval$new(0, 1) ~ 0.5)

test_that("values",{
  expect_silent({p$values$asas = 1})
  expect_silent({p$values$ntrees = 1})
  expect_silent({p$values$ntrees = NULL})
  expect_equal(p$values$ntrees, NULL)
  expect_equal(p$values$sample.fraction, 0.5)
  expect_silent({p$values$ntrees = 1})
  expect_equal(p$values$ntrees, 1)
  expect_silent({p$values = list(splitrule = "C", sample.fraction = 0.1, ntrees = 2)})
  expect_error({p$values$ntrees = -5}, "does not lie in")
})

test_that("params",{
  expect_equal(class(p$params)[1], "data.table")
  expect_error({p$params = "a"})
})

test_that("supports",{
  expect_equal(p$supports, list(ntrees = PosIntegers$new(),
                                splitrule = Set$new("logrank","extratrees","C","maxstat"),
                                sample.fraction = Interval$new(0, 1)))
})

test_that("ids",{
  expect_equal(p$ids, c("ntrees","splitrule","sample.fraction"))
})

p3 = ParamSet$new(a = LogicalSet$new() ~ tags(train) + TRUE,
                  b = LogicalSet$new() ~ FALSE)

test_that("rbind",{
  p1 = ParamSet$new(a = LogicalSet$new() ~ tags(train) + TRUE)
  p2 = ParamSet$new(b = LogicalSet$new() ~ FALSE)
  expect_equal(rbind(p1,p2), p3)
  expect_error(rbind(p1, ParamSet$new(a = LogicalSet$new())),"Must have unique")
})

test_that("add",{
  p1 = ParamSet$new(a = LogicalSet$new() ~ TRUE + tags(train))
  expect_error(p1$add(a = Set$new(1)), "ids must be")
  expect_equal(p1$add(b = LogicalSet$new() ~ FALSE), p3)

  p = ParamSet$new(a = Set$new("a") ~ "a", b = Set$new("b") ~ "b")
  expect_equal(p$add(c = Set$new("c") ~ "c", d = Set$new("d") ~ "d"),
               ParamSet$new(a = Set$new("a") ~ "a", b = Set$new("b") ~ "b",
                            c = Set$new("c") ~ "c", d = Set$new("d") ~ "d"))
})

test_that("remove",{
  p = ParamSet$new(a = Set$new("a") ~ "a", b = Set$new("b") ~ "b",
                   c = Set$new("c") ~ "c", d = Set$new("d") ~ "d")
  expect_equal(p$remove("a","b"),
               ParamSet$new(c = Set$new("c") ~ "c", d = Set$new("d") ~ "d"))

  p = ParamSet$new(a = Set$new("a") ~ "a", b = Set$new("b") ~ "b",
                   c = Set$new("c") ~ "c", d = Set$new("d") ~ "d")
  expect_equal(p$remove(c("a","b")),
               ParamSet$new(c = Set$new("c") ~ "c", d = Set$new("d") ~ "d"))

  p = ParamSet$new(a = Set$new("a") ~ "a", b = Set$new("b") ~ "b",
                   c = Set$new("c") ~ "c", d = Set$new("d") ~ "d")
  expect_equal(p$remove(list("a","g")),
               ParamSet$new(b = Set$new("b") ~ "b", c = Set$new("c") ~ "c", d = Set$new("d") ~ "d"))

  p = ParamSet$new(a = Set$new("a") ~ "a", b = Set$new("b") ~ "b",
                   c = Set$new("c") ~ "c", d = Set$new("d") ~ "d")
  expect_equal(p$remove("e"), p)
})

test_that("as.data.table",{
  expect_equal(as.data.table(p), p$params)
})

test_that("as.ParamSet",{
  expect_error(as.ParamSet(data.table(Id = "a", Support = Set$new(), Val = 2, Tag = list(NULL))), "colnames")
  expect_error(as.ParamSet(data.table(Id = c("a","a"),
                                      Support = c(Set$new(),Set$new(2)),
                                      Value = c(2,NULL), Tag = c("train","train"))), "unique names")
  expect_error(as.ParamSet(data.table(Id = c("a","b"),
                                      Support = c(Set$new(),Set$new(2)),
                                      Value = list(2,NULL),
                                      Tag = list("train","predict"))), "does not lie")
  expect_equal(as.ParamSet(data.table(Id = c("a","b"),
                                      Support = list(Set$new(),Set$new(2)),
                                      Value = list(NULL, 2))),
               ParamSet$new("a" = Set$new(),
                            "b" = Set$new(2)~2))
})

test_that("alt constructor",{
  expect_error(ParamSet$new(support = Set$new(1), value = 2, tag = "train"), "list")
  expect_error(ParamSet$new(support = list(Set$new(1)), value = list(2), tag = list("train")), "Must have names")
  expect_silent(ParamSet$new(support = list(a = Set$new(1)), value = list(1), tag = list("train")))
  expect_error(ParamSet$new(support = list(a = Set$new(1)), value = list(2), tag = list("train")), "does not lie")
  expect_error(ParamSet$new(support = list(a = Set$new(1)), value = list(2,0), tag = list("train")), "length")
  p = ParamSet$new(support = list(a = Set$new(1)), value = list(1), tag = list("train"))
  expect_equal(p$ids, "a")
  expect_equal(p$supports, list(a = Set$new(1)))
  expect_equal(p$values, list(a = 1))
  expect_equal(p$tags, list(a = "train"))
})


