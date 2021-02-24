.ParameterSet__initialize <- function(self, private, prms, tag_properties) {
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
        utags <- unique(unlist(tag_list))
        private$.tag_properties <- lapply(utags, function(.x) {
        ids <- names(tag_list)[grepl(.x, tag_list)]
        if (!is.null(tag_properties) && .x %in% names(tag_properties)) {
          properties <- tag_properties[[.x]]
        } else {
          properties <- NULL
        }
        list(id = ids, properties = properties)
      })
      names(private$.tag_properties) <- utags
      }

      if (any(duplicated(c(private$.id, unique(unlist(private$.tags)))))) {
         stop("ids and tags must have different names.")
      }
    }

    invisible(self)
}

.ParameterSet__print <- function(self, private, sort) {
  dt <- suppressWarnings(as.data.table(self, sort = sort))
  dt$Support <- sapply(dt$Support, function(x) x$strprint())
  print(dt)
}

# FIXME - ADD TAG PROPERTIES
.ParameterSet__get_values <- function(self, private, id, tags, transform, inc_null, simplify) {
    .get_values(self, private, private$.value, id, tags, transform, inc_null, simplify)
}

.ParameterSet__add_dep <- function(self, private, id, on, cnd) {
    checkmate::assert_class(cnd, "cnd")
      all_ids <- unique(c(self$ids, unprefix(self$ids)))
      checkmate::assert_subset(id, all_ids)
      checkmate::assert_subset(on, all_ids)

      if (id == on) {
        stop("Parameters cannot depend on themselves.")
      }

      # hacky fix
      aid <- id
      aon <- on

      if (!is.null(private$.deps) &&
          nrow(subset(private$.deps, grepl(aid, id) & grepl(aon, on)))) {
        stop(sprintf("%s already depends on %s.", id, on))
      }

      support <- unique(unlist(private$.supports[grepl(on, names(private$.supports))]))

      if (length(support) > 1) {
        stop("Single dependency cannot be added on multiple supports.")
      }

      support <- support_dictionary$get(support)

      assert_condition(on, support, cnd)

      if (is.null(self$deps)) {
        deps <- data.table::data.table(id = character(0L), on = character(0L), cond = list())
      } else {
        deps <- self$deps
      }

      new_dt <- rbind(deps, data.table::data.table(id = id, on = on, cond = list(cnd)))

      assert_no_cycles(new_dt)

      .check_deps(self, self$values, new_dt, id, TRUE)

      private$.deps <- new_dt

      invisible(self)
}

.ParameterSet__transform <- function(self, private) {
    if (!is.null(private$.trafo)) {
        private$.value <- private$.trafo(self$values, self)
      }

      invisible(self)
}

.ParameterSet__add_check <- function(self, private, fun, ids, tags) {
      if (is.null(self$checks)) {
        checks <- data.table(ids = list(), tags = list(), fun = list())
      } else {
        checks <- self$checks
      }

      if (is.null(ids) && is.null(tags)) {
        stop("At least one of 'ids' and 'tags' must be non-NULL.")
      }

      checkmate::assert_subset(ids, unique(c(self$ids, unprefix(self$ids))))
      checkmate::assert_subset(tags, unlist(self$tags))


      checkmate::assert_function(fun, c("x", "self"))
      if (!checkmate::test_logical(fun(self$values, self), len = 1)) {
        stop("'fun' should evaluate to a scalar logical.")
      }
      private$.checks <- rbind(private$.checks,
                               data.table::data.table(ids = list(ids),
                                                      tags = list(tags),
                                                      fun = list(body(fun))))

      invisible(self)
}

# FIXME - ADD TAG PROPERTIES
.ParameterSet__check <- function(self, private, supports, custom, deps, id, error_on_fail) {
    .check(self, supports, custom, deps, id, error_on_fail, value_check = self$values,
             support_check = private$.isupports, dep_check = self$deps, custom_check = self$checks)
}

# FIXME - ADD TAG PROPERTIES
.ParameterSet__rep <- function(self, private, times, prefix) {
      if (length(prefix) == 1) {
        prefix <- paste0(prefix, seq_len(times))
      } else if (length(prefix) != times) {
        stop(sprintf("'prefix' should either be length '1' or same as 'times' (%d)", times))
      }

      lng <- length(self)

      private$.id <- paste(rep(prefix, each = lng), rep(private$.id), sep = "__")

      private$.isupports <- lapply(private$.isupports,
                                  function(x) paste(rep(prefix, each = length(x)),
                                                    rep(x, times), sep = "__"))

      private$.supports <- rep(private$.supports, times)
      names(private$.supports) <- paste(rep(prefix, each = lng),
                                        names(private$.supports), sep = "__")

      values <- rep(private$.value, times)
      names(values) <- paste(rep(prefix, each = length(private$.value)), names(values), sep = "__")
      private$.value <- values

      tags <- rep(private$.tags, times)
      names(tags) <- paste(rep(prefix, each = length(private$.tags)), names(tags), sep = "__")
      private$.tags <- tags

      invisible(self)
}

# FIXME - ADD TAG PROPERTIES
.ParameterSet__extract <- function(self, private, id, tags, prefix) {
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
        supports <- .get_field(self, private$.supports, id = ids, inc_null = FALSE)
        values <- .get_field(self, private$.value, id = ids)
        tag <- .get_field(self, private$.tags, id = ids)

        ps <- as.ParameterSet(
            unname(Map(prm,
              id = ids,
              support = supports,
              value = values,
              tags = tag,
              .check = FALSE
            ))
          )

        if (!is.null(private$.deps)) {
          deps <- subset(private$.deps, grepl(which_ids, id) | grepl(which_ids, on))
          if (nrow(deps)) {
            if (!is.null(unfix_ids)) {
              deps$id <- unfix_ids[match(deps$id, ids)]
              deps$on <- unfix_ids[match(deps$on, ids)]
            }
            pri <- get_private(ps)
            pri$.deps <- deps
          }
        }

        if (!is.null(private$.checks)) {
          which_checks <- vapply(seq(nrow(private$.checks)), function(i) {
            .x <- private$.checks[i, ]
            cids <- unlist(.x[[1]])
            ctags <- unlist(.x[[2]])
            if (!is.null(id) && !is.null(cids) && all(cids %in% id)) {
              TRUE
            } else if (!is.null(tags) && !is.null(ctags) && all(ctags %in% tags)) {
              TRUE
            } else {
              FALSE
            }
          }, logical(1))

          checks <- subset(private$.checks, which_checks)
          if (nrow(checks)) {
            if (!is.null(unfix_ids)) {
              checks$ids[match(checks$ids, ids, 0)] <- unfix_ids[match(checks$ids, ids, 0)]
            }
            pri <- get_private(ps)
            pri$.checks <- checks
          }
        }

        ps
}