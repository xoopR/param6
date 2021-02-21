.get_field <- function(self, x, id = NULL, tags = NULL, inc_null = TRUE) {

  if (inc_null) {
    x <- expand_list(self$ids, x)
  }

  if (!is.null(tags)) {
    which <- names(self$tags)[(grepl(tags, self$tags))]
    x <- x[match(which, names(x), 0L)]
  }

  if (!is.null(id)) {
    if (length(id)) {
      x <- x[grepl(paste0(id, collapse = "|"), names(x))]
    } else {
      x <- named_list()
    }
  }

  x
}

.get_values <- function(self, private, values, id = NULL, tags = NULL, transform = TRUE,
                      inc_null = TRUE, simplify = TRUE) {

      values <- .get_field(self, values, id = id, tags = tags,
                          inc_null = inc_null)

      if (transform && !is.null(private$.trafo)) {
        values <- private$.trafo(values, self)
      }

      if (simplify) {
        if (length(values) == 0) {
          values <- NULL
        } else if (length(values) == 1) {
          values <- values[[1]]
        }
      }

      values
}

.check <- function(self, supports = TRUE, custom = TRUE, deps = TRUE, id = NULL,
                  error_on_fail = TRUE, value_check = NULL, support_check = NULL, dep_check = NULL,
                  custom_check = NULL) {

  # 1. Containedness checks
  if (supports && length(self)) {
    x <- .check_supports(self, value_check, support_check, id, error_on_fail)
  }

  if (!x) {
    return(FALSE)
  }

  # 2. Dependencies
  if (deps && !is.null(dep_check)) {
    x <- .check_deps(self, value_check, dep_check, id, error_on_fail)
  }

  if (!x) {
    return(FALSE)
  }

  # 3. Custom checks
  if (custom && !is.null(custom_check)) {
    x <- .check_custom(self, value_check, custom_check, id, error_on_fail)
  }

  if (!x) {
    return(FALSE)
  }

  invisible(TRUE)
}

.check_supports <- function(self, values, supports, id, error_on_fail) {

  for (i in seq_along(supports)) {
    ids <- supports[[i]]
    if (!is.null(id)) {
      ids <- intersect(id, ids)
    }

    value <- .get_values(self, get_private(self), values, ids, inc_null = FALSE)

    set <- support_dictionary$get(names(supports)[[i]])
    if (!set$contains(value, all = TRUE)) {
      msg <- sprintf("%s does not lie in %s.", value, as.character(set))
      if (error_on_fail) {
        stop(msg)
      } else {
        warning(msg)
        return(FALSE)
      }
    }
  }
  TRUE
}

.check_deps <- function(self, values, deps, id, error_on_fail) {
  if (!is.null(deps) && nrow(deps)) {
    for (i in nrow(deps)) {
      id <- deps[i, 1][[1]]
      on <- deps[i, 2][[1]]
      cnd <- deps[i, 3][[1]][[1]]
      fun <- eval(cnd)
      id_value <- .get_values(self, get_private(self), values, id, inc_null = FALSE)
      on_value <- .get_values(self, get_private(self), values, on, inc_null = FALSE)
      if (length(id_value)) {
        ok <- fun(on_value)
        if (!ok) {
          msg <- sprintf("Dependency of %s on '%s %s %s' failed.", id, on,
                        attr(cnd, "type"), string_as_set(attr(cnd, "value")))
          if (error_on_fail) {
            stop(msg)
          } else {
            warning(msg)
            return(FALSE)
          }
        }
      }
    }
  } else {
    TRUE
  }
}

.check_custom <- function(self, values, checks, id, error_on_fail) {
  if (!is.null(checks) && nrow(checks)) {
    if (!is.null(id)) {
      checks <- subset(checks, grepl(paste0(id, collapse = "|"), params))
    }

    all(vapply(checks$fun, function(.y) {
      as.function(list(x = values, self = self, .y))()
    }, logical(1)))
  } else {
    TRUE
  }
}