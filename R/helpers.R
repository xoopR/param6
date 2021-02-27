assert_contains <- function(set, value, name) {
  if (!is.null(set$power) && set$power == "n") {
    value <- as.Tuple(value)
  }
  if (set$contains(value, all = TRUE)) {
    invisible(value)
  } else {
    if (!missing(name)) {
      stop(sprintf("%s does not lie in support of %s (%s).", value, name,
                   as.character(set)))
    } else {
      stop(sprintf("%s does not lie in %s.", value, as.character(set)))
    }
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

unprefix <- function(x, split = "__") {
  start <- vapply(x, function(.x) regexpr("__", .x, fixed = TRUE)[[1]][[1]],
                  integer(1)) + 2
  substr(x, start, 1e3)
}

get_prefix <- function(x, split = "__") {
  vapply(x, function(.x) unlist(strsplit(.x, split, TRUE))[[1]], character(1),
         USE.NAMES = FALSE)
}

unique_nlist <- function(x) {
  x[!duplicated(names(x))]
}
