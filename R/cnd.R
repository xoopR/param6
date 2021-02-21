cnd <- function(value, type) {
  fun <- switch(type,
    "eq" = `==`,
    "neq" = `!=`,
    "geq" = `>=`,
    "leq" = `<=`,
    "gt" = `>`,
    "lt" = `<`,
    "any" = `%in%`,
    "nany" = `%nin%`,
    stop("'type' must be one of {'eq', 'neq', 'geq', 'leq', 'gt', 'lt', 'any', 'nany'}.")
  )

  fun <- substitute(function(x) !any(is.null(x)) && all(fun(unlist(x), value)))

  char <- switch(type,
    "eq" = "==",
    "neq" = "!=",
    "geq" = ">=",
    "leq" = "<=",
    "gt" = ">",
    "lt" = "<",
    "any" = "%in%",
    "nany" = "%nin%"
  )

  class(fun) <- "cnd"
  attr(fun, "value") <- value
  attr(fun, "type") <- char
  fun
}
