#' @title Create a ParameterSet Condition
#' @description Function to create a condition for [ParameterSet] dependencies
#' for use in the `$deps` public method.
#' @param type (`character(1)`) \cr
#' The condition `type` determines the type of dependency to create, options
#' are given in details.
#' @param value (`ANY`) \cr
#' If `id` is `NULL` then `value` should be a value in the support of the
#' parameter that the condition is testing, that will be passed to the condition
#' determined by `type`.
#' @param id (`character(1)`) \cr
#' If `value` is `NULL` then `id` should be the same as the id that the
#' condition is testing, and the condition then takes the currently set value
#' of the id in its argument.
#' @details
#' This function should never be used outside of creating a condition for
#' a dependency in a [ParameterSet]. Currently the following conditions are
#' supported based on the `type` argument, we refer to the parameter depended on
#' as in the independent parameter, and the other as the dependent:
#'
#' * `"eq"` - If `value` is not `NULL` then checks if the independent parameter
#' equals `value`, otherwise checks if the independent and dependent parameter
#' are equal.
#' * `"neq"` - If `value` is not `NULL` then checks if the independent parameter
#' does not equal `value`, otherwise checks if the independent and dependent
#' parameter are not equal.
#' * `"gt"/"lt"` - If `value` is not `NULL` then checks if the independent
#' parameter is greater/less than `value`, otherwise checks if the independent
#' parameter is greater/less than the dependent parameter.
#' * `"geq"/"leq"` - If `value` is not `NULL` then checks if the independent
#' parameter is greater/less than or equal to `value`, otherwise checks if the
#' independent parameter is greater/less than or equal to the dependent
#' parameter.
#' * `"any"` - If `value` is not `NULL` then checks if the independent parameter
#' equals any of `value`, otherwise checks if the independent parameter equals
#' any of dependent parameter.
#' * `"nany"` - If `value` is not `NULL` then checks if the independent
#' parameter does not equal any of `value`, otherwise checks if the independent
#' parameter does not equal any of dependent parameter.
#' * `"len"` - If `value` is not `NULL` then checks if the length of the
#' independent parameter equals `value`, otherwise checks if the independent
#' and dependent parameter are the same length.
#' @export
cnd <- function(type, value = NULL, id = NULL) {
  choice <- c("eq", "neq", "geq", "leq", "gt", "lt", "any", "nany", "len")
  sfun <- switch(type,
    "eq" = `==`,
    "neq" = `!=`,
    "geq" = `>=`,
    "leq" = `<=`,
    "gt" = `>`,
    "lt" = `<`,
    "any" = `%in%`,
    "nany" = `%nin%`,
    "len" = {
      if (!is.null(id)) {
        function(x, y) length(x) == length(y)
      } else {
        function(x, y) length(x) == y
      }
      },
    stop(sprintf("'type' must be one of %s.", string_as_set(choice)))
  )

  if (!is.null(id)) {
    if (!is.null(value)) {
      warning("'id' and 'value' are non-NULL, 'value' is ignored.")
    }
    fun <- substitute(function(on, idx, ...) {
      !any(is.null(on)) && all(sfun(unlist(idx), unlist(on)))
    })
  } else {
    fun <- substitute(function(x, ...)
      !any(is.null(x)) && all(sfun(unlist(x), value)))
  }

  char <- switch(type,
    "eq" = "==",
    "neq" = "!=",
    "geq" = ">=",
    "leq" = "<=",
    "gt" = ">",
    "lt" = "<",
    "any" = "%in%",
    "nany" = "%nin%",
    "len" = "len"
  )

  class(fun) <- "cnd"
  attr(fun, "value") <- value
  attr(fun, "id") <- id
  attr(fun, "type") <- char
  fun
}
