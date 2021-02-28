.Dictionary__initialize <- function(self, private, x, types) { # nolint
   private$.items <- checkmate::assert_list(x, types = types,
                                            names = "unique")
   private$.types <- types
   invisible(self)
}

.Dictionary__add <- function(self, private, x, keys, values) { # nolint
   if (!length(x)) {
      if (is.null(keys) || is.null(values)) {
         stop("Either a named list or 'keys' and 'values' must be provided.") # nolint
      }
      x <- named_list(values, keys)
   }
   # signif quicker than first concatenating and then checking
   checkmate::assert_list(x, min.len = 1, types = private$.types)
   private$.items <- checkmate::assert_list(c(private$.items, x),
                                            names = "unique")
   invisible(self)
}

.Dictionary__remove <- function(self, private, x) { # nolint
   checkmate::assert_subset(x, self$keys)
   private$.items[x] <- NULL
   # catch all.equal named list vs list
   if (!length(private$.items)) private$.items <- list()
   invisible(self)
}

.Dictionary__get <- function(self, private, x) { # nolint
   if (length(private$.types) == 1 || length(x) == 1) {
      x <- private$.items[checkmate::assert_subset(x, self$keys)]
      if (length(x) == 1) {
         x <- x[[1]]
         if (checkmate::testR6(x)) {
            x <- x$clone(deep = TRUE)
         }
      } else {
         x <- unlist(x)
         x <- sapply(x, function(.x) {
            if (checkmate::testR6(.x)) {
               .x$clone(deep = TRUE)
            } else {
               .x
            }
         })
         if (!checkmate::testList(x)) {
            x <- unname(x)
         }
      }
      return(x)
   } else {
      stop("'get' can only be used if length of 'x' is '1' or if Dictionary has one type.") # nolint
   }
}

.Dictionary__get_list <- function(self, private, x) { # nolint
   lapply(private$.items[checkmate::assert_subset(x, self$keys)],
          function(.x) {
             if (checkmate::testR6(.x)) {
                .x$clone(deep = TRUE)
             } else {
                .x
             }
          })
}

.Dictionary__has <- function(self, private, x) { # nolint
   x %in% self$keys
}

.Dictionary__has_value <- function(self, private, x) { # nolint
   x %in% self$values
}

.Dictionary__print <- function(self, private, n) { # nolint
   cat(as.character(self, n = n), "\n")
}

.Dictionary__summary <- function(self, private, n) { # nolint
   if (self$typed) {
      cat(sprintf("Typed dictionary of %d items and types: %s.\n",
                  self$length, string_as_set(self$types)))
   } else {
      cat(sprintf("Untyped dictionary of %d items.\n", self$length))
   }
   cat(as.character(self, n = n), "\n")
}

.Dictionary__rekey <- function(self, private, key, new_key) { # nolint
   checkmate::assert_choice(key, self$keys)
   checkmate::assert_character(c(new_key, self$keys), unique = TRUE)
   names(private$.items)[which(names(private$.items) %in% key)] <-
      new_key
   invisible(self)
}

.Dictionary__merge <- function(self, private, x) { # nolint
   if (checkmate::test_class(x, "Dictionary")) {
      self$add(x$items)
   } else if (checkmate::test_list(x, "Dictionary")) {
      sapply(x, function(.x) self$add(.x$items))
   } else {
      stop("'x' should either be a Dictionary or list of Dictionaries.")
   }
   invisible(self)
}

.Dictionary__values <- function(self, private) { # nolint
   if (length(private$.types == 1)) {
      unname(unlist(private$.items))
   } else {
      unname(private$.items)
   }
}

.Dictionary__items <- function(self, private, x) { # nolint
   if (missing(x)) {
      private$.items
   } else {
      checkmate::assert_list(x, types = private$.types,
                             names = "unique")
      private$.items <- x
   }
}
