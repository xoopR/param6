#' @export
as.ParameterSet <- function(x, ...) { # nolint
  UseMethod("as.ParameterSet")
}

#' @export
as.ParameterSet.data.table <- function(x, ...) { # nolint
  ParameterSet$new(as.prm(x))
}

#' @export
as.ParameterSet.prm <- function(x, ...) { # nolint
  ParameterSet$new(list(x))
}

#' @export
as.ParameterSet.list <- function(x, ...) { # nolint
  checkmate::assert_list(x, "prm", any.missing = FALSE)
  ParameterSet$new(x)
}

#' @export
length.ParameterSet <- function(x) {
  x$length
}

#' @export
rep.ParameterSet <- function(x, times, prefix, ...) {
  x <- x$clone(deep = TRUE)
  x$rep(times, prefix)
  x
}

#' @export
c.ParameterSet <- function(...) {
  ParameterSet$new(unlist(lapply(list(...), as.prm), FALSE))
}
