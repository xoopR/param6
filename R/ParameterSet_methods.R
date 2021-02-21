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

# FIXME - CONSIDER ADDING DEPS
#' @export
as.data.table.ParameterSet <- function(x, sort = TRUE, string = FALSE, ...) { # nolint
  if (length(x$deps) || length(x$trafos) || length(x$checks)) {
    warning("Dependencies, trafos, and checks are lost in coercion.")
  }
  dt = data.table::data.table(
    Id = x$ids,
    Support = x$supports,
    Value = expand_list(x$ids, x$values),
    Tags = expand_list(x$ids, x$tags)
  )
  if (sort) {
    Id = NULL # visible binding fix
    data.table::setorder(dt, Id)
  }
  dt
}