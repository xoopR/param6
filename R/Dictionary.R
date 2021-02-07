Dictionary <- R6Class("Dictionary",
    private = list(
        .items = list(),
        .types = NULL
    ),

    public = list(
        initialize = function(x = list(), types = NULL) {
            private$.items <- checkmate::assert_list(x, types = types, names = "strict")
            private$.types <- types
            invisible(self)
        },

        add = function(x = list()) {
            # signif quicker than first concatenating and then checking
            checkmate::assert_list(x, min.len = 1, types = private$.types, names = "strict")
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
            unname(unlist(private$.items[checkmate::assert_subset(x, self$keys)]))
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

        print = function(n = 2) {
            print(as.character(self, n = n))
        },

        summary = function(n = 2) {
          if (self$typed) {
            cat(sprintf("Typed dictionary with types %s and %d items.\n",
                string_as_set(self$types), self$length))
          } else {
            cat(sprintf("Untyped dictionary with %d items.\n", self$length))
          }
          cat(as.character(self, n = n), "\n")
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
              checkmate::assert_list(x, types = private$.types, names = "strict")
              private$.items <- x
            }
        },
        length = function() length(private$.items),
        typed = function() !is.null(private$.types),
        types = function() private$.types
    )
)

#' @export
`[.Dictionary` <- function(object, i) {
    object$get_list(i)
}

#' @export
`[[.Dictionary` <- function(object, i) {
    object$get(i)
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
as.character.Dictionary = function(x, n = 2, ...) { # nolint
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