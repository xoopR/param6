expect_equal_ParamSet = function(object, expected){
  expect_ParamSet(object)
  expect_ParamSet(expected)
  obj_ord = order(object$ids)
  exp_ord = order(expected$ids)
  expect_equal(sapply(object$supports[obj_ord], function(x) x$strprint),
               sapply(expected$supports[exp_ord], function(x) x$strprint))
  expect_equal(object$tags[obj_ord], expected$tags[exp_ord])
  expect_equal(object$values[order(names(object$values))], expected$values[order(names(expected$values))])
}

expect_ParamSet = function(object){
  expect(class(object)[1] == "ParamSet", "Object is not a ParamSet.")
}
