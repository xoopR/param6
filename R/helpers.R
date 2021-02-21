assert_contains <- function(set, value, name) {
  if (set$contains(value, all = TRUE)) {
    invisible(value)
  } else {
    if (!missing(name)) {
      stop(sprintf("%s does not lie in support of %s (%s).", value, name, as.character(set)))
    } else {
      stop(sprintf("%s does not lie in %s.", value, as.character(set)))
    }
  }
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
                   paste0("{", paste0(checks$id[checker], collapse = ","), "}")))
    }


  }
}

assert_condition <- function(id, support, cond) {

  val <- attr(cond, "value")

  if (attr(cond, "type") %in% c("eq", "geq", "leq", "gt", "lt", "any")) {
    msg <- sprintf("%s does not lie in support of %s (%s). Condition is not possible.",
                   val, id, as.character(support))
  } else {
    msg <- sprintf("%s does not lie in support of %s (%s). Condition is redundant.",
                   val, id, as.character(support))
  }

  if (!(testContains(support, val))) {
    stop(msg)
  } else {
    invisible(val)
  }
}

string_as_set <- function(str) {
  if (!is.null(str)) {
    paste0("{", paste0(str, collapse = ", "), "}")
  }
}

sort_named_list <- function(lst, ...) {
  lst[order(names(lst), ...)]
}

named_list <- function(values, names) {
  if (missing(values) && missing(names)) {
    x <- list()
    names <- character()
  } else {
    x <- list(values)
  }

  names(x) <- names
  x
}

as_named_list <- function(values, names) {
  if (missing(values) && missing(names)) {
    x <- list()
    names <- character()
  } else {
    x <- as.list(values)
  }

  names(x) <- names
  x
}

expand_list <- function(names, named_var) {
  checkmate::assert_character(names)
  checkmate::assert_list(named_var)

  x <- vector("list", length(names))
  names(x) <- names
  x[names(x) %in% names(named_var)] <- named_var
  return(x)
}

get_private <- function(x) {
  x$.__enclos_env__$private
}

invert_names <- function(x) {
  uvalues <- unique(x)
  inv_x <- lapply(uvalues, function(.x) names(x)[x == .x])
  names(inv_x) <- uvalues
  inv_x
}

# if results in empty list and rm.names = TRUE then unnames
un_null_list <- function(x, rm.names = TRUE) {
  x[vapply(x, is.null, logical(1))] <- NULL
  if (!length(x)) {
    x <- unname(x)
  }
  x
}

# append and assign a variable in an environment
env_append <- function(env, var, values) {
  env[[var]] <- c(env[[var]], values)
  invisible(NULL)
}

`%nin%` <- function(x, table) {
  !(x %in% table)
}