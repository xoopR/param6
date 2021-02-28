#---------------
# Public Methods
#---------------

.ParameterSet__initialize <- function(self, private, prms, tag_properties) { # nolint
  if (length(prms)) {
    checkmate::assert_list(prms, "prm", any.missing = FALSE)

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

      if (any(duplicated(c(private$.id, unique(unlist(private$.tags)))))) {
        stop("ids and tags must have different names.")
      }
    }
  }

  invisible(self)
}

.ParameterSet__print <- function(self, private, sort) { # nolint
  dt <- suppressWarnings(as.data.table(self, sort = sort))
  dt$Support <- sapply(dt$Support, function(x) x$strprint())
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

  invisible(self)
}

.ParameterSet__extract <- function(self, private, id, tags, prefix) { # nolint

  if (!is.null(private$.trafo)) {
    warning("Transformations are not included in extraction.")
  }

  if (is.null(id) && is.null(prefix) && is.null(tags)) {
    stop("One argument must be non-NULL.")
  } else if (!is.null(id) && !is.null(prefix)) {
    warning("'prefix' argument ignored.")
    prefix <- NULL
  }

  if (!is.null(prefix)) {
    ids <- names(.get_field(self, private$.value, prefix))
    unfix_ids <- unprefix(ids)
  } else {
    ids <- names(.get_field(self, private$.value, id, tags))
    unfix_ids <- NULL
  }

  which_ids <- paste0(ids, collapse = "|")
  supports <- unname(.get_field(self, private$.supports, id = ids,
                                inc_null = FALSE))
  values <- unname(.get_field(self, private$.value, id = ids))

  tag <- unname(.get_field(self, private$.tags, id = ids))
  if (!is.null(prefix)) {
    unfix_tags <- unprefix(tag)
  }

  if (length(unfix_ids)) {
    ps <- as.ParameterSet(
      unname(Map(prm,
                 id = unfix_ids,
                 support = supports,
                 value = values,
                 tags = unfix_tags,
                 .check = FALSE
      ))
    )
  } else {
    ps <- as.ParameterSet(
      unname(Map(prm,
                 id = ids,
                 support = supports,
                 value = values,
                 tags = tag,
                 .check = FALSE
      ))
    )
  }

  if (!is.null(private$.deps)) {
    deps <- subset(private$.deps,
                   grepl(which_ids, id) & grepl(which_ids, on))
    if (nrow(deps)) {
      if (!is.null(unfix_ids)) {
        deps$id <- unfix_ids[match(deps$id, ids)]
        deps$on <- unfix_ids[match(deps$on, ids)]
      }
      pri <- get_private(ps)
      pri$.deps <- deps
    }
  }

  if (length(private$.tag_properties)) {
    new_props <- list()
    for (i in c("linked", "required", "unique")) {
      if (length(private$.tag_properties[[i]])) {
      new_props[[i]] <-
        unprefix(
          private$.tag_properties[[i]][private$.tag_properties[[i]] %in%
                                           tag]
        )
      }
    }
    ps$tag_properties <- new_props
  }


  ps
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
    return(private$.value)
  } else {
    x <- un_null_list(x)
    if (length(x)) {
      .check(self,
             id = names(x), value_check = x,
             support_check = private$.isupports, dep_check = self$deps,
             tag_check = self$tag_properties
      )
    }

    private$.value <- x
    invisible(self)
  }
}

.ParameterSet__trafo <- function(self, private, x) { # nolint
  if (missing(x)) {
    private$.trafo
  } else {
    checkmate::assert_function(x, args = c("x", "self"), TRUE)
    vals <- x(self$values, self)
    checkmate::assert_list(vals)

    tryCatch(.check(self, id = names(vals), value_check = vals,
                    support_check = private$.isupports,
                    dep_check = self$deps),
             error = function(e) {
               stop("Transformation results in values outside of supports.")
             })

    private$.trafo <- x
    invisible(self)
  }
}
