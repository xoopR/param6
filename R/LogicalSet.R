LogicalSet <- R6::R6Class("LogicalSet", inherit = Set,
                          public = list(
                            initialize = function(){
                              super$initialize(elements = list(TRUE, FALSE), type = "logical")
                            }
                          ))
