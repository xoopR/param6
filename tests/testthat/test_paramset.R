context("ParamSet")

#----
p1 = ParamSet$new(ntrees = PosIntegers$new(),
                 splitrule = Set$new("logrank","extratrees","C","maxstat") ~ tags("train","predict") + "logrank",
                 sample.fraction = Interval$new(0, 1) ~ 0.5)
p2 = ParamSet$new(splitrule = Set$new("logrank","extratrees","C","maxstat") ~ tags("train","predict") + "logrank",
                  sample.fraction = Interval$new(0, 1) ~ 0.5)
p3 = ParamSet$new(ntrees = PosIntegers$new() ~ 2)
#----


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

test_that("print",{
  expect_output(p1$print())
})

test_that("values",{
  expect_silent({p1$values$asas = 1})
  expect_silent({p1$values$ntrees = 1})
  expect_silent({p1$values$ntrees = NULL})
  expect_equal(p1$values$ntrees, NULL)
  expect_equal(p1$values$sample.fraction, 0.5)
  expect_silent({p1$values$ntrees = 1})
  expect_equal(p1$values$ntrees, 1)
  expect_silent({p1$values = list(splitrule = "C", sample.fraction = 0.1, ntrees = 2)})
  expect_error({p1$values$ntrees = -5}, "does not lie in")
})

test_that("get_values",{
  support = rep(list(PosIntegers$new()), 5)
  names(support) = letters[1:5]
  p = ParamSet$new(support = support,
                   value = list(1,2,NULL,4,NULL),
                   tag = list("train",c("train","predict"),NULL,NULL,"predict"))
  expect_equal(p$values, list(a = 1, b = 2, d = 4))
  expect_equal(p$get_values(), list(a = 1, b = 2, c = NULL, d = 4, e = NULL))
  expect_equal(p$get_values(tag = "predict"), list(b = 2, e = NULL))
  expect_equal(p$get_values(tag = "train"), list(a = 1, b = 2))
})

test_that("params",{
  expect_equal(class(p1$params)[1], "data.table")
  expect_error({p1$params = "a"})
})

test_that("supports",{
  expect_equal(p1$supports, list(ntrees = PosIntegers$new(),
                                splitrule = Set$new("logrank","extratrees","C","maxstat"),
                                sample.fraction = Interval$new(0, 1)))
})

test_that("ids",{
  expect_equal(p1$ids, c("ntrees","splitrule","sample.fraction"))
})

test_that("rbind",{
  p2$values = list(splitrule = "C", sample.fraction = 0.1)
  expect_equal(as.data.table(rbind(p3,p2)), as.data.table(p1))
  expect_error(rbind(p3, ParamSet$new(ntrees = LogicalSet$new())),"Must have unique")
})

test_that("add",{
  expect_error(p2$add(splitrule = Set$new(1)), "ids must be")
  expect_equal_ParamSet(p2$add(ntrees = PosIntegers$new() ~ 2), p1)
})

test_that("remove",{
  p4 = ParamSet$new(a = Set$new("a") ~ "a", b = Set$new("b") ~ "b",
                   c = Set$new("c") ~ "c", d = Set$new("d") ~ "d")
  expect_equal(p4$remove("a","b"),
               ParamSet$new(c = Set$new("c") ~ "c", d = Set$new("d") ~ "d"))

  p4 = ParamSet$new(a = Set$new("a") ~ "a", b = Set$new("b") ~ "b",
                   c = Set$new("c") ~ "c", d = Set$new("d") ~ "d")
  expect_equal(p4$remove(c("a","b")),
               ParamSet$new(c = Set$new("c") ~ "c", d = Set$new("d") ~ "d"))

  p4 = ParamSet$new(a = Set$new("a") ~ "a", b = Set$new("b") ~ "b",
                   c = Set$new("c") ~ "c", d = Set$new("d") ~ "d")
  expect_equal(p4$remove(list("a","g")),
               ParamSet$new(b = Set$new("b") ~ "b", c = Set$new("c") ~ "c", d = Set$new("d") ~ "d"))

  p4 = ParamSet$new(a = Set$new("a") ~ "a", b = Set$new("b") ~ "b",
                   c = Set$new("c") ~ "c", d = Set$new("d") ~ "d")
  expect_equal(p4$remove("e"), p4)
})

test_that("as.data.table",{
  expect_equal(as.data.table(p1), p1$params)
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
  expect_error(ParamSet$new(support = list()), "must be constructed")
  expect_error(ParamSet$new(support = Set$new(1), value = 2, tag = "train"), "list")
  expect_error(ParamSet$new(support = list(Set$new(1)), value = list(2), tag = list("train")), "Must have names")
  expect_silent(ParamSet$new(support = list(a = Set$new(1)), value = list(1), tag = list("train")))
  expect_error(ParamSet$new(support = list(a = Set$new(1)), value = list(2), tag = list("train")), "does not lie")
  expect_error(ParamSet$new(support = list(a = Set$new(1)), value = list(2,0), tag = list("train")), "length")
  p5 = ParamSet$new(support = list(a = Set$new(1)), value = list(1), tag = list("train"))
  expect_equal(p5$ids, "a")
  expect_equal(p5$supports, list(a = Set$new(1)))
  expect_equal(p5$values, list(a = 1))
  expect_equal(p5$tags, list(a = "train"))
})

test_that("subset",{
  expect_equal_ParamSet(p1$subset("ntrees"), p3)
})

test_that("length",{
  expect_equal(p1$length, 1)
  expect_equal(p2$length, 3)
})

test_that("deps",{
  expect_false(p2$has_deps)
  expect_equal(p2$deps, data.table(id = character(0L), on = character(0L), type = character(0L), cond = list()))
  expect_silent(p2$add_dep("splitrule","sample.fraction","Equal",0.2))
  expect_error(p2$add_dep("splitrule","sample.fraction","Equal",0.2),"already depends")
  expect_equal(p2$deps, data.table(id="splitrule",on="sample.fraction",type="Equal",cond=list(0.2)))
  expect_silent(p2$add_dep("ntrees","sample.fraction","Equal",0.2))
  expect_true(p2$has_deps)
  expect_error(p2$add_dep("splitrule","splitrule","Equal",0.1),"param cannot depend")
  expect_error(p2$add_dep("splitrule","sfdsf","Equal",0.1), "Must be element of")
  expect_error(p2$add_dep("sdsd","sfdsf","splitrule",0.1), "Must be element of")
})
