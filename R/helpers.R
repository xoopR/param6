assert_contains <- function(set, value, name) {
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
  if (length(lst)) {
    lst[order(names(lst), ...)]
  } else {
    lst
  }
}

named_list <- function(values, names) {
  if (missing(values) && missing(names)) {
    setNames(list(), character())
  } else {
    setNames(list(values), names)
  }
}

as_named_list <- function(values, names) {
  if (missing(values) && missing(names)) {
    setNames(list(), character())
  } else {
    setNames(as.list(values), names)
  }
}

expand_list <- function(names, named_var) {
  checkmate::assert_character(names)
  checkmate::assert_list(named_var)

  mtc <- match(names(named_var), names)
  if (any(is.na(mtc))) {
    stop("ids in 'names' not in 'named_var'")
  }

  x <- setNames(vector("list", length(names)), names)
  x[mtc] <- named_var
  x
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
  gsub(sprintf("([[:alnum:]]+)%s(\\S*)", split), "\\2", x)
}

get_prefix <- function(x) {
  gsub("([[:alnum:]]+)__(\\S*)", "\\1", x)
}

unique_nlist <- function(x) {
  x[!duplicated(names(x))]
}


drop_null <- function(x) {
  x[vapply(x, function(.x) length(.x) > 0, logical(1))]
}


assert_alphanum <- function(x) {
  if (any(grepl("[^[:alnum:]]", x))) {
    stop("'x' must be alphanumeric")
  }
  invisible(x)
}

list_element <- function(x, name) {
  x[grepl(name, names(x))]
}


give_prefix <- function(x, prefix) {
  sprintf("%s__%s", prefix, x)
}


prefix_list <- function(x, prefix) {
  if (length(x)) {
    setNames(x, give_prefix(names(x), prefix))
  } else {
    x
  }
}


unprefix_list <- function(x) {
  if (length(x)) {
    setNames(x, unprefix(names(x)))
  } else {
    x
  }
}


rlapply <- function(x, fun) {
  if (startsWith(fun, ".")) {
    lapply(x, function(.x) get_private(.x)[[fun]])
  } else {
    lapply(x, "[[", fun)
  }
}
