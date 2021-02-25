#' @export
Dictionary <- R6Class("Dictionary",
  private = list(
    .items = list(),
    .types = NULL,
    deep_clone = function(name, value) {
      switch(name,
             ".items" = sapply(value, function(.x) {
               if (inherits(.x, "R6")) {
                 .x$clone(deep = TRUE)
               } else {
                 .x
               }
             }),
             value
      )
    }
  ),

  public = list(
    initialize = function(x = list(), types = NULL) {
      .Dictionary__initialize(self, private, x, types)
    },
    add = function(x = list(), keys = NULL, values = NULL) {
      .Dictionary__add(self, private, x, keys, values)
    },
    rekey = function(key, new_key) {
      .Dictionary__rekey(self, private, key, new_key)
    },
    remove = function(x) .Dictionary__remove(self, private, x),
    get = function(x) .Dictionary__get(self, private, x),
    get_list = function(x) .Dictionary__get_list(self, private, x),
    has = function(x) .Dictionary__has(self, private, x),
    has_value = function(x) .Dictionary__has_value(self, private, x),
    print = function(n = 2) .Dictionary__print(self, private, n),
    summary = function(n = 2) .Dictionary__summary(self, private, n),
    merge = function(x) .Dictionary__merge(self, private, x)
  ),

  active = list(
    keys = function() names(private$.items),
    values = function() .Dictionary__values(self, private),
    items = function(x) .Dictionary__items(self, private, x),
    length = function() length(private$.items),
    typed = function() !is.null(private$.types),
    types = function() private$.types
  )
)

# @export
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
