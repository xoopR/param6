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
            private$.items <- checkmate::assert_list(x, types = types, names = "unique")
            private$.types <- types
            invisible(self)
        },

        add = function(x = list()) {
            # signif quicker than first concatenating and then checking
            checkmate::assert_list(x, min.len = 1, types = private$.types)
            private$.items <- checkmate::assert_list(c(private$.items, x), names = "unique")
            invisible(self)
        },

        remove = function(x) {
            checkmate::assert_subset(x, self$keys)
            private$.items[x] <- NULL
            # catch all.equal named list vs list
            if (!length(private$.items)) private$.items <- list()
            invisible(self)
        },

        get = function(x) {
          if (length(private$.types) == 1 || length(x) == 1) {
            private$.items[checkmate::assert_subset(x, self$keys)][[1]]
          } else {
            stop("'get' can only be used if length of 'x' is '1' or if Dictionary has one type.")
          }
        },

        get_list = function(x) {
          private$.items[checkmate::assert_subset(x, self$keys)]
        },

        has = function(x) {
            x %in% self$keys
        },

        has_value = function(x) {
            x %in% self$values
        },

        print = function(n = 2) {
         cat(as.character(self, n = n),"\n")
        },

        summary = function(n = 2) {
          if (self$typed) {
            cat(sprintf("Typed dictionary of %d items and types: %s.\n",
                self$length, string_as_set(self$types)))
          } else {
            cat(sprintf("Untyped dictionary of %d items.\n", self$length))
          }
          cat(as.character(self, n = n), "\n")
        },

        rekey = function(key, new_key) {
          checkmate::assert_choice(key, self$keys)
          checkmate::assert_character(c(new_key, self$keys), unique = TRUE)
          names(private$.items)[which(names(private$.items) %in% key)] <- new_key
          invisible(self)
        },

        merge = function(x) {
          if (checkmate::test_class(x, "Dictionary")) {
            self$add(x$items)
          } else if (checkmate::test_list(x, "Dictionary")) {
            sapply(x, function(.x) self$add(.x$items))
          } else {
            stop("'x' should either be a Dictionary or list of Dictionaries.")
          }
          invisible(self)
        }
    ),

    active = list(
        keys = function() names(private$.items),
        values = function() {
          if (length(private$.types == 1)) {
            unname(unlist(private$.items))
          } else {
            unname(private$.items)
          }
        },
        items = function(x) {
            if (missing(x)) {
                private$.items
            } else {
              checkmate::assert_list(x, types = private$.types, names = "unique")
              private$.items <- x
            }
        },
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
    keys = x$keys
    values = unname(unlist(sapply(x$values, as.character)))

    lng = x$length
    if (lng > (2 * n)) {
        string <- paste0(paste(keys[1:n], values[1:n], sep = ": ", collapse = ", "),
        ", ..., ", paste(keys[(lng - n + 1):lng], values[(lng - n + 1):lng],
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