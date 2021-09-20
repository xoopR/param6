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

    tag_list <- un_null_list(lapply(prms, "[[", "tags"))
    if (length(tag_list)) {
      private$.tags <- tag_list
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
    } else {
      private$.tags <- list()
    }
  } else {
    private$.value <- list()
    private$.id <- list()
    private$.tags <- list()
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

.ParameterSet__extract <- function(self, private, id, tags, prefix) { # nolint

  if (is.null(id) && is.null(prefix) && is.null(tags)) {
    stop("One argument must be non-NULL.")
  } else if ((!is.null(id) || !is.null(tags)) && !is.null(prefix)) {
    stop("'prefix' must be NULL if 'id' or 'tags' is non-NULL")
  }

  if (!is.null(prefix)) {
    ids <- names(.filter_field(self, private$.value,
                               sprintf("^%s__", assert_alphanum(prefix))))
  } else {
    ids <- names(.filter_field(self, private$.value, id, tags))
  }

  rm_ids <- setdiff(self$ids, ids)

  ## create new parameterset
  pnew <- self$clone(deep = TRUE)
  ## remove non-extracted ids
  pnew$remove(rm_ids)

  ## remove prefix if required
  if (!is.null(prefix)) {
    get_private(pnew)$.unprefix(prefix)
  }

  pnew
}


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

  if (setequal(pars, self$ids)) {
    stop("Can't remove all parameters")
  }

  mtc_pars <- paste0(pars, collapse = "|")

  private$.immutable[pars] <- NULL
  if (length(private$.immutable) == 0) {
    private$.immutable <- NULL
  }
  if (!is.null(private$.deps)) {
    private$.deps <- private$.deps[!(id %in% pars | on %in% pars), ]
    if (nrow(private$.deps) == 0) {
      private$.deps <- NULL
    }
  }

  if (is.list(private$.trafo)) {
    private$.trafo[c(prefix, pars)] <- NULL
    if (length(private$.trafo) == 0) {
      private$.trafo <- NULL
    } else if (checkmate::test_list(private$.trafo, len = 1) &&
            (is.null(names(private$.trafo)) || names(private$.trafo) == "")) {
      private$.trafo <- private$.trafo[[1]]
    }
  }

  private$.tags[pars] <- NULL
  if (length(private$.tags) == 0) {
    private$.tags <- list()
    private$.tag_properties <- NULL
  }

  ## TODO: Consider adding removal of tag property

  private$.value[pars] <- NULL
  if (length(private$.value) == 0) {
    private$.value <- list()
  }
  private$.supports <- private$.supports[setdiff(names(private$.supports),
                                                 pars)]

  which <- grepl(mtc_pars, private$.isupports)
  private$.isupports[which] <- lapply(private$.isupports[which],
                                      function(.x) setdiff(.x, pars))
  private$.isupports <- drop_null(private$.isupports)

  private$.id <- setdiff(private$.id, pars)

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
        if (is.na(nms[[i]]) || nms[[i]] == "") {
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
    uni <- !duplicated(strs[miss])
    support_dictionary$add(setNames(x[miss][uni], strs[miss][uni]))
  }

  ## update supports
  private$.supports[names(x)] <- strs
  private$.isupports <- invert_names(private$.supports)

  invisible(self)
}


.ParameterSet__.prefix <- function(self, private, prefix) { # nolint

  private$.id <- give_prefix(self$ids, prefix)
  private$.immutable <- prefix_list(private$.immutable, prefix)
  private$.tags <- prefix_list(private$.tags, prefix)
  private$.value <- prefix_list(private$.value, prefix)
  private$.supports <- prefix_list(private$.supports, prefix)
  private$.isupports <- invert_names(private$.supports)

  if (is.list(private$.trafo)) {
    private$.trafo <- prefix_list(private$.trafo, prefix)
  }

  if (length(private$.deps)) {
    private$.deps[, id := give_prefix(id, prefix)]
    private$.deps[, on := give_prefix(on, prefix)]
    private$.deps$cond <- lapply(private$.deps$cond, function(.x) {
      at <- attr(.x, "id")
      if (!is.null(at)) {
        attr(.x, "id") <- give_prefix(at, prefix)
      }
      .x
    })
  }


  if (length(private$.tag_properties) &&
        "linked" %in% names(private$.tag_properties)) {
    tags <- private$.tag_properties$linked
    private$.tag_properties$linked <-
      give_prefix(private$.tag_properties$linked, prefix)
    which <- grepl(paste0(tags, collapse = "|"), private$.tags)
    if (any(which)) {
      for (i in seq_along(private$.tags[which])) {
        iwhich <- private$.tags[which][[i]] %in% tags
        private$.tags[which][[i]][iwhich] <-
          give_prefix(private$.tags[which][[i]][iwhich], prefix)
      }
    }
  }

  invisible(self)
}


.ParameterSet__.unprefix <- function(self, private, prefix) { # nolint

  private$.id <- unprefix(self$ids)
  private$.immutable <- unprefix_list(private$.immutable)
  private$.tags <- unprefix_list(private$.tags)
  private$.value <- unprefix_list(private$.value)
  private$.supports <- unprefix_list(private$.supports)
  private$.isupports <- invert_names(private$.supports)

  if (is.list(private$.trafo)) {
    private$.trafo <- unprefix_list(private$.trafo)
  }

  if (length(private$.deps)) {
    private$.deps[, id := unprefix(id)]
    private$.deps[, on := unprefix(on)]
    private$.deps$cond <- lapply(private$.deps$cond, function(.x) {
      at <- attr(.x, "id")
      if (!is.null(at)) {
        attr(.x, "id") <- unprefix(at)
      }
      .x
    })
  }


  if (length(private$.tag_properties) &&
        "linked" %in% names(private$.tag_properties)) {
    tags <- private$.tag_properties$linked
    private$.tag_properties$linked <-
      unprefix(private$.tag_properties$linked)
    which <- private$.tags %in% tags
    if (any(which)) {
      for (i in seq_along(private$.tags[which])) {
        iwhich <- private$.tags[which][[i]] %in% tags
        private$.tags[which][[i]][[iwhich]] <-
          unprefix(private$.tags[which][[i]][[iwhich]])
      }
    }
  }

  invisible(self)
}