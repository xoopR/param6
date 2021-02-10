assert_contains <- function(set, value, name) {
  if (set$contains(value, all = TRUE)) {
    invisible(value)
  } else {
    if (!missing(name)) {
      stop(sprintf("%s does not lie in support of %s (%s).", value, name, set$strprint()))
    } else {
      stop(sprintf("%s does not lie in %s.", value, set$strprint()))
    }
  }
}

assert_no_cycles <- function(lookup) {
  check <- data.table(lookup[, 2])
  checks <- data.table(lookup[, 1])

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

assert_condition <- function(id, support, type, cond) {
  if (type %in% c("Equal", "AnyOf")) {
    msg <- sprintf("%s does not lie in support of %s (%s). Condition is not possible.", cond, id,
                   support$strprint())
  } else {
    msg <- sprintf("%s does not lie in support of %s (%s). Condition is redundant.", cond, id,
                   support$strprint())
  }

  if (!(testContains(support, cond))) {
    stop(msg)
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
  x <- list(values)
  names(x) <- names
  x
}

as_named_list <- function(values, names) {
  x <- as.list(values)
  names(x) <- names
  x
}

partial_list <- function(names, named_list) {
  lst <- vector("list", length(names))
  names(lst) <- names
  lst[names(lst) %in% names(named_list)] <- named_list
  return(lst)
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
