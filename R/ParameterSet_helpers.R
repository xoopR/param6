.get_field <- function(self, x, id = NULL, tags = NULL, inc_null = TRUE) {
  if (inc_null) {
    x <- expand_list(self$ids, x)
  }

  tagx <- idx <- named_list()

  if (!is.null(tags)) {
    which <- names(self$tags)[(grepl(paste0(tags, collapse = "|"), self$tags))]
    tagx <- x[match(which, names(x), 0L)]
  }

  if (!is.null(id)) {
    idx <- x[grepl(paste0(id, collapse = "|"), names(x))]
  }

  if (!is.null(tags) || !is.null(id)) {
    x <- unique_nlist(c(idx, tagx))
  }

  x
}

.get_values <- function(self, private, values, id = NULL, tags = NULL,
                        transform = TRUE, inc_null = TRUE, simplify = TRUE) {

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

.check <- function(self, supports = TRUE, custom = TRUE, deps = TRUE,
                   tags = TRUE, id = NULL, error_on_fail = TRUE,
                   value_check = NULL, support_check = NULL, dep_check = NULL,
                   custom_check = NULL, tag_check = NULL) {
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

  # 3. Tags
  if (tags && !is.null(tag_check)) {
    x <- .check_tags(self, value_check, tag_check, id, error_on_fail)
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

  return(TRUE)
}

.check_supports <- function(self, values, supports, id, error_on_fail) {
  for (i in seq_along(supports)) {
    ids <- supports[[i]]
    if (!is.null(id)) {
      ids <- intersect(id, ids)
    }
    if (length(ids)) {
      value <- .get_values(self, get_private(self), values, ids,
                           inc_null = FALSE)

      set <- support_dictionary$get(names(supports)[[i]])
      if (!set$contains(value, all = TRUE)) {
        return(.return_fail(
          msg = sprintf("%s does not lie in %s.", value, as.character(set)),
          error_on_fail
        ))
      }
    }
  }
  return(TRUE)
}

.check_deps <- function(self, values, deps, id, error_on_fail) {
  if (!is.null(deps) && nrow(deps)) {
    for (i in nrow(deps)) {
      id <- deps[i, 1][[1]]
      on <- deps[i, 2][[1]]
      cnd <- deps[i, 3][[1]][[1]]
      fun <- eval(cnd)
      id_value <- .get_values(self, get_private(self), values, id,
                              inc_null = FALSE)
      on_value <- .get_values(self, get_private(self), values, on,
                              inc_null = FALSE)
      if (length(id_value)) {
        ok <- fun(on_value)
        if (!ok) {
          return(.return_fail(
            msg = sprintf("Dependency of %s on '%s %s %s' failed.", id, on,
                          attr(cnd, "type"), string_as_set(attr(cnd, "value"))),
            error_on_fail
          ))
        }
      }
    }
  }

  return(TRUE)
}

.check_tags <- function(self, values, tags, id, error_on_fail) {
  if (length(tags)) {
    nok <- "required" %in% names(tags) &&
      any(vapply(.get_field(self, values, id, tags = tags[["required"]]),
                 is.null, logical(1)))
    if (nok) {
      return(.return_fail(
        msg = "Not all required parameters are set.",
        error_on_fail
      ))
    }

    nok <- "linked" %in% names(tags) &&
      length(.get_values(self, get_private(self), values, id,
                         tags = tags[["linked"]],
                         inc_null = FALSE)) > length(tags[["linked"]])
    if (nok) {
      return(.return_fail(
        msg = "Multiple linked parameters are set.",
        error_on_fail
      ))
    }
  }

  return(TRUE)
}

.check_custom <- function(self, values, checks, id, error_on_fail) {
  if (!is.null(checks) && nrow(checks)) {
    if (!is.null(id)) {
      ids <- NULL
      checks <- subset(checks, grepl(paste0(id, collapse = "|"), ids))
    }

    # `for` instead of `vapply` allows early breaking
    for (i in seq_along(checks$fun)) {
      .y <- checks$fun[[i]]
      ok <- as.function(list(x = values, self = self, .y))()
      if (!ok) {
        return(.return_fail(sprintf("Check on '%s' failed.", deparse(.y)),
                            error_on_fail))
      }
    }
  }

  return(TRUE)
}

assert_no_cycles <- function(lookup) {
  check <- data.table::data.table(lookup[, 2])
  checks <- data.table::data.table(lookup[, 1])

  for (i in seq_len(ncol(lookup))) {
    check <- merge(check, lookup, by.x = "on", by.y = "id", sort = FALSE)[, 2]
    colnames(check) <- "on"
    checks <- cbind(checks, check)

    checker <- apply(checks, 1, function(x) any(duplicated(x)) & !any(is.na(x)))
    if (any(checker)) {
      stop(sprintf("Cycles detected starting from id(s): %s",
                   paste0("{", paste0(checks$id[checker],
                                      collapse = ","), "}")))
    }


  }
}

assert_condition <- function(id, support, cond) {

  val <- attr(cond, "value")

  if (attr(cond, "type") %in% c("eq", "geq", "leq", "gt", "lt", "any")) {
    msg <- sprintf("%s does not lie in support of %s (%s). Condition is not possible.", # nolint
                   val, id, as.character(support))
  } else {
    msg <- sprintf("%s does not lie in support of %s (%s). Condition is redundant.", # nolint
                   val, id, as.character(support))
  }

  if (!(testContains(support, val))) {
    stop(msg)
  } else {
    invisible(val)
  }
}

.return_fail <- function(msg, error_on_fail) {
  if (error_on_fail) {
    stop(msg)
  } else {
    warning(msg)
    return(FALSE)
  }
}
