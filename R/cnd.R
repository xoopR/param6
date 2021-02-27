cnd <- function(value, type) {
  choice <- c("eq", "neq", "geq", "leq", "gt", "lt", "any", "nany", "len")
  fun <- switch(type,
    "eq" = `==`,
    "neq" = `!=`,
    "geq" = `>=`,
    "leq" = `<=`,
    "gt" = `>`,
    "lt" = `<`,
    "any" = `%in%`,
    "nany" = `%nin%`,
    "len" = function(x, y) length(x) == length(y),
    stop(sprintf("'type' must be one of %s.", string_as_set(choice)))
  )

  if (checkmate::testCharacter(value)) {
    fun <- substitute(function(on, idx, values) {
      onx <- values[[value]]
      !any(is.null(onx)) && all(fun(unlist(idx), unlist(onx)))
    })
  } else {
    fun <- substitute(function(x, ...)
      !any(is.null(x)) && all(fun(unlist(x), value)))
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
  attr(fun, "type") <- char
  fun
}
