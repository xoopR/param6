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
c.ParameterSet <- function(..., pss = list(...)) {
  prms <- unlist(lapply(pss, as.prm), FALSE)
  props <- unlist(lapply(pss, "[[", "tag_properties"), recursive = FALSE)
  if (length(props)) {
    tprop <- list()
    req_props <- props[names(props) %in% "required"]
    if (length(req_props)) {
      tprop$required <- unique(unlist(req_props))
    }
    lin_props <- props[names(props) %in% "linked"]
    if (length(lin_props)) {
      tprop$linked <- unique(unlist(lin_props))
    }
    if (any(duplicated(unlist(tprop)))) {
      stop("Cannot merge inconsistent tag properties.")
    }
  } else {
    tprop <- NULL
  }

  ParameterSet$new(prms, tprop)
}

#' @export
as.data.table.ParameterSet <- function(x, sort = TRUE, string = FALSE, ...) { # nolint
  if (length(x$deps) || length(x$trafos) || length(x$checks)) {
    warning("Dependencies, trafos, and checks are lost in coercion.")
  }
  dt <- data.table::data.table(
    Id = x$ids,
    Support = x$supports,
    Value = expand_list(x$ids, x$values),
    Tags = expand_list(x$ids, x$tags)
  )
  if (sort) {
    Id <- NULL # binding fix
    data.table::setorder(dt, Id)
  }
  dt
}
