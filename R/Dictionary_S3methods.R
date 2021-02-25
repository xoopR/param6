#' @export
`[.Dictionary` <- function(object, i) {
  object$get_list(i)
}

#' @export
`%has%` <- function(object, x, ...) { # nolint
  UseMethod("%has%", object)
}

#' @export
`%has%.Dictionary` <- function(object, x, ...) { # nolint
  object$has(x)
}

#' @export
length.Dictionary <- function(x) {
  x$length
}

#' @export
summary.Dictionary <- function(object, n = 2, ...) {
  object$summary(n = n)
}

#' @export
as.character.Dictionary <- function(x, n = 2, ...) { # nolint
  keys <- x$keys
  values <- unname(unlist(sapply(x$values, as.character)))

  lng <- x$length
  if (lng > (2 * n)) {
    string <- paste0(paste(keys[1:n], values[1:n], sep = ": ",
                           collapse = ", "),
                     ", ..., ", paste(keys[(lng - n + 1):lng],
                                      values[(lng - n + 1):lng],
                                      sep = ": ", collapse = ", "))
  } else {
    string <- paste(keys, values, sep = ": ", collapse = ", ")
  }
  return(paste0("{", string, "}"))
}

#' @export
c.Dictionary <- function(...) {
  x <- list(...)
  types <- sapply(x, function(.x) list(.x$typed, length(.x$types), .x$types))
  # different typing
  if (length(unique(types[1, ])) > 1) {
    stop("Can only combine Dictionaries if all typed or all untyped.")
    # all typed or untyped
  } else {
    # untyped
    if (!unlist(types[1, 1])) {
      Dictionary$new(unlist(lapply(x, "[[", "items"), recursive = FALSE))
      # typed
    } else {
      # different type lengths
      if (length(unique(types[2, ])) > 1) {
        stop("Can only combine typed Dictionaries of the same type(s).")
      } else {
        if (length(unique(unlist(types[3, ]))) != types[2, 1][[1]]) {
          stop("Can only combine typed Dictionaries of the same type(s).")
        } else {
          Dictionary$new(unlist(lapply(x, "[[", "items"), recursive = FALSE),
                         types = unlist(types[3, 1]))
        }
      }
    }
  }
}
