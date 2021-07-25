#---------------
# Public Methods
#---------------

.ParameterSet__initialize <- function(self, private, prms, tag_properties) { # nolint

  if (length(prms)) {
    checkmate::assert_list(prms, "prm", any.missing = FALSE)
    prms <- unname(prms)

    ids <- vapply(prms, "[[", character(1), "id")
    if (any(duplicated(ids))) {
      stop("ids are not unique.")
    } else {
      names(prms) <- ids
      private$.id <- ids
    }

    private$.supports <- vapply(prms, "[[", character(1), "support")
    private$.isupports <- invert_names(private$.supports)

    private$.value <- un_null_list(lapply(prms, "[[", "value"))

    tag_list <- lapply(prms, "[[", "tags")
    if (length(tag_list)) {
      private$.tags <- un_null_list(tag_list)
      private$.tag_properties <-
        .assert_tag_properties(tag_properties, unique(unlist(tag_list)), self)

      if ("immutable" %in% private$.tag_properties) {
        private$.immutable <- self$get_values(
          tags = self$tag_properties["immutable"], simplify = FALSE
        )
      }

      if (any(duplicated(c(private$.id, unique(unlist(private$.tags)))))) {
        stop("ids and tags must have different names.")
      }
    }
  } else {
    private$.value <- list()
    private$.id <- list()
  }

  invisible(self)
}

.ParameterSet__print <- function(self, private, sort) { # nolint
  dt <- suppressWarnings(as.data.table(self, sort = sort))
  if (nrow(dt)) {
    dt$Support <- vapply(dt$Support, function(x) x$strprint(), character(1))
  }
  print(dt)
}

.ParameterSet__get_values <- function(self, private, id, tags, transform, # nolint
                                      inc_null, simplify) {
  .get_values(self, private, private$.value, id, tags, transform, inc_null,
              simplify)
}

.ParameterSet__add_dep <- function(self, private, id, on, cnd) { # nolint
  checkmate::assert_class(cnd, "cnd")
  all_ids <- unique(c(self$ids, unprefix(self$ids)))
  checkmate::assert_subset(id, all_ids)
  checkmate::assert_subset(on, all_ids)

  if (!is.null(attr(cnd, "id"))) {
    checkmate::assert_choice(attr(cnd, "id"), on)
  }

  if (id == on) {
    stop("Parameters cannot depend on themselves.")
  }

  # hacky fix
  aid <- id
  aon <- on

  nok <- !is.null(private$.deps) &&
    nrow(subset(private$.deps, grepl(aid, id) & grepl(aon, on)))
  if (nok) {
    stop(sprintf("%s already depends on %s.", id, on))
  }

  support <- unique(
    unlist(private$.supports[grepl(on, names(private$.supports))]))

  support <- support_dictionary$get(support)

  if (is.null(self$deps)) {
    deps <- data.table(id = character(0L), on = character(0L),
                       cond = list())
  } else {
    deps <- self$deps
  }

  new_dt <- rbind(deps,
                  data.table(id = id, on = on,
                             cond = list(assert_condition(on, support, cnd))))

  assert_no_cycles(new_dt)

  .check_deps(self, self$values, new_dt, id, TRUE)

  private$.deps <- new_dt

  invisible(self)
}

.ParameterSet__rep <- function(self, private, times, prefix) { # nolint

  if (length(prefix) == 1) {
    prefix <- paste0(prefix, seq_len(times))
  } else if (length(prefix) != times) {
    stop(sprintf("'prefix' should either be length '1' or same as 'times' (%d)", times)) # nolint
  }

  assert_alphanum(prefix)

  lng <- length(self)

  private$.id <- paste(rep(prefix, each = lng), rep(private$.id),
                       sep = "__")

  private$.isupports <- lapply(private$.isupports,
                               function(x) paste(rep(prefix,
                                                     each = length(x)),
                                                 rep(x, times), sep = "__"))

  private$.supports <- rep(private$.supports, times)
  names(private$.supports) <- paste(rep(prefix, each = lng),
                                    names(private$.supports), sep = "__")

  values <- rep(private$.value, times)
  names(values) <- paste(rep(prefix, each = length(private$.value)),
                         names(values), sep = "__")
  private$.value <- values

  tags <- rep(private$.tags, times)
  names(tags) <- paste(rep(prefix, each = length(private$.tags)),
                       names(tags), sep = "__")
  private$.tags <- tags

  if (!is.null(private$.immutable)) {
    imm <- rep(private$.immutable, times)
    names(imm) <- paste(rep(prefix, each = length(private$.immutable)),
                        names(imm), sep = "__")
    private$.immutable <- imm
  }

  invisible(self)
}

.ParameterSet__extract <- function(self, private, id, tags, prefix, # nolint
                                   keep_trafo) {

  if (is.null(id) && is.null(prefix) && is.null(tags)) {
    stop("One argument must be non-NULL.")
  } else if (!is.null(id) && !is.null(prefix)) {
    warning("'prefix' argument ignored.")
    prefix <- NULL
  }

  if (!is.null(prefix)) {
    prefix <- sprintf("^%s__", assert_alphanum(prefix))
    ids <- names(.filter_field(self, private$.value, prefix))
    unfix_ids <- unprefix(ids)
  } else {
    ids <- names(.filter_field(self, private$.value, id, tags))
    unfix_ids <- NULL
  }

  supports <- unname(.filter_field(self, private$.supports, id = ids,
                                inc_null = FALSE))
  values <- unname(.filter_field(self, private$.value, id = ids))

  if (length(private$.tags)) {
    tag <- unname(.filter_field(self, private$.tags, id = ids))
    if (!is.null(prefix)) {
      unfix_tags <- lapply(tag, unprefix)
    }
  } else {
    tag <- unfix_tags <- rep(list(NULL), length(values))
  }

  trafo <- NULL
  if (!is.null(private$.trafo)) {
    if (keep_trafo) {
      trafo <- self$trafo
      if (length(prefix) && checkmate::test_list(trafo) &&
          grepl("__", names(trafo), fixed = TRUE)) {
        trafo <- trafo[grepl(paste0(prefix, collapse = "|"), names(trafo))]
        names(trafo) <- unprefix(names(trafo))
      }
    } else {
      warning("Transformations removed in extraction.")
    }
  }

  deps <- NULL
  if (!is.null(private$.deps)) {
    ## first check for prefixes
    which_ids <- paste0(ids, collapse = "|")
    deps <- subset(private$.deps,
                   grepl(which_ids, id) & grepl(which_ids, on))
    if (nrow(deps)) {
      ## remove prefix if extracting by prefix
      if (!is.null(unfix_ids)) {
        deps$id <- unfix_ids[match(deps$id, ids)]
        deps$on <- unfix_ids[match(deps$on, ids)]
        at <- attr(deps$cond[[1]], "id")
        if (!is.null(at)) {
          attr(deps$cond[[1]], "id") <- unprefix(at)
        }
      }
    ## check if no prefix but extracting
    } else if (length(unfix_ids)) {
      which_ids <- paste0(unfix_ids, collapse = "|")
      deps <- subset(private$.deps,
                    grepl(which_ids, id) & grepl(which_ids, on))
    }

    if (nrow(deps) == 0) {
      deps <- NULL
    }
  }

  props <- NULL
  if (length(private$.tag_properties)) {
    props <- list()
    for (i in c("linked", "required", "unique", "immutable")) {
      if (length(private$.tag_properties[[i]])) {
        tp <- private$.tag_properties[[i]][private$.tag_properties[[i]] %in%
          unlist(tag)]
        if (!is.null(prefix)) {
          tp <- unprefix(tp)
        }
        props[[i]] <- tp
      }
    }
  }

  if (length(unfix_ids)) {
    ids <- unfix_ids
    tags <- unfix_tags
  }

  ps <- as.ParameterSet(
    Map(prm,
      id = ids,
      support = supports,
      value = values,
      tags = tag,
      .check = FALSE
    ),
    trafo = trafo,
    tag_properties = props
  )
  pri <- get_private(ps)
  pri$.deps <- deps

  ps
}


# This is incomplete and needs better support for deps, trafo, checks.
#  However as above I am unsure if these methods are ever needed.
.ParameterSet__remove <- function(self, private, id, prefix) { # nolint

  if (sum(is.null(id) + is.null(prefix)) != 1) {
    stop("Exactly one argument must be non-NULL.")
  }

  if (!is.null(prefix)) {
    stopifnot(length(prefix) == 1)
    pars <- self$ids[grepl(prefix, get_prefix(self$ids))]
  } else {
    pars <- id
  }

  names(private)

  private$.immutable[pars] <- NULL
  if (length(private$.immutable) == 0) {
    private$.immutable <- NULL
  }
  if (!is.null(private$.deps)) {
    private$.deps <- subset(
      private$.deps,
      !(grepl(id, pars) | grepl(on, pars))
    )
    if (nrow(private$.deps) == 0) {
      private$.deps <- NULL
    }
  }
  private$.trafo[c(prefix, pars)] <- NULL
  if (length(private$.trafo) == 0) {
    private$.trafo <- NULL
  } else if (is.list(private$.trafo) && length(private$.trafo) == 1) {
    private$.trafo <- private$.trafo[[1]]
  }
  private$.tags[pars] <- NULL
  if (length(private$.tags) == 0) {
    private$.tags <- list()
    private$.tag_properties <- NULL
  }

  ## TODO: Consider adding removal of tag property

  private$.value[pars] <- NULL
  if (length(private$.value) == 0) {
    private$.value <- NULL
  }
  private$.supports <- private$.supports[setdiff(names(private$.supports),
                                                 pars)]
  if (length(private$.supports) == 0) {
    private$.supports <- NULL
  }

  which <- grepl(pars, private$.isupports)
  private$.isupports[which] <- lapply(private$.isupports[which],
                                      function(.x) setdiff(.x, pars))
  private$.isupports <- drop_null(private$.isupports)
  if (length(private$.isupports) == 0) {
    private$.isupports <- NULL
  }

  private$.id <- setdiff(private$.id, pars)
  if (length(private$.id) == 0) {
    private$.id <- NULL
  }

  invisible(self)
}


.ParameterSet__transform <- function(self, private, x) { # nolint

  trafo <- self$trafo
  if (is.null(trafo)) {
    return(x)
  }

  if (checkmate::test_function(trafo)) {
    x <- trafo(x, self)
  } else {
    if (is.null(nms <- names(trafo))) {
      for (i in seq_along(trafo)) {
        x <- trafo[[i]](x, self)
      }
    } else {
      newx <- x[!grepl(paste0(sprintf("%s__", nms), collapse = "|"), names(x))]
      for (i in seq_along(trafo)) {
        ## if unnamed then apply to all
        if (nms[[i]] == "") {
          newx <- append(newx, trafo[[i]](x, self))
        } else {
          which <- grepl(sprintf("%s__", nms[[i]]), names(x))
          newx <- append(newx, trafo[[i]](x[which], self))
        }
      }
      x <- newx
    }
  }

  x
}


#---------------
# Active Bindings
#---------------

.ParameterSet__supports <- function(self, private) { # nolint
  sups <- support_dictionary$get_list(private$.supports)
  names(sups) <- self$ids
  sups
}

.ParameterSet__tag_properties <- function(self, private, x) { # nolint
  if (missing(x)) {
    private$.tag_properties
  } else {
    private$.tag_properties <-
      .assert_tag_properties(x, unlist(self$tags), self)
    invisible(self)
  }
}

.ParameterSet__values <- function(self, private, x) { # nolint

  if (missing(x)) {
    return(sort_named_list(private$.value))
  } else {
    x <- un_null_list(x)
    bad_nms <- names(x) %nin% self$ids
    if (any(bad_nms)) {
      stop(
        sprintf("You can't set ids that don't exist in the parameter set: %s",
                string_as_set(names(x)[bad_nms]))
      )
    }
    if (length(x)) {
      .check(self, private,
        id = names(x), value_check = x,
        support_check = private$.isupports, dep_check = self$deps,
        tag_check = self$tag_properties
      )
    } else if (!is.null(self$tag_properties) &&
                "required" %in% names(self$tag_properties)) {
      stop("Not all required parameters are set")
    } else if (!is.null(self$tag_properties) &&
                "immutable" %in% names(self$tag_properties)) {
      stop("Immutable parameters cannot be updated after construction")
    }
    which <- intersect(names(private$.immutable), names(x))
    x[which] <- private$.immutable[which]
    x <- c(x, private$.immutable[names(private$.immutable) %nin% names(x)])
    private$.value <- x
    invisible(self)
  }
}

.ParameterSet__trafo <- function(self, private, x) { # nolint

  if (missing(x)) {
    private$.trafo
  } else {
    if (length(x)) {
      if (checkmate::test_list(x)) {
        x <- unlist(x, recursive = FALSE)
        if (!is.null(names(x))) {
          names(x) <- gsub(".", "__", names(x), fixed = TRUE)
        }
        x <- x[!duplicated(x)]
        lapply(x, checkmate::assert_function, args = c("x", "self"),
               ordered = TRUE)
        if (length(x) == 1 && (is.null(names(x)) || is.na(names(x)))) {
          x <- x[[1]]
        }
      } else {
        checkmate::assert_function(x, args = c("x", "self"), TRUE)
      }
    } else {
      x <- NULL
    }

    otrafo <- private$.trafo
    private$.trafo <- x
    vals <- checkmate::assert_list(self$transform(self$values))

    tryCatch(.check(self, private, id = names(vals), value_check = vals,
                    support_check = private$.isupports,
                    dep_check = self$deps, transform = FALSE),
            error = function(e) {
              private$.trafo <- otrafo
              stop(e)
            })

    invisible(self)
  }
}


#---------------
# Private Methods
#---------------
.ParameterSet__.update_support <- function(self, private, x) { # nolint
  ## get sets as strings
  strs <- vapply(x, as.character, character(1), n = Inf)

  ## add to dictionary as required
  miss <- !support_dictionary$has(strs)
  if (any(miss)) {
    support_dictionary$add(setNames(x[miss], strs[miss]))
  }

  ## update supports
  private$.supports[names(x)] <- strs
  private$.isupports <- invert_names(private$.supports)

  invisible(self)
}
