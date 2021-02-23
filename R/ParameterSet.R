#' @export
ParameterSet <- R6::R6Class("ParameterSet",
  public = list(
    initialize = function(prms = list()) {

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
        private$.tags <- un_null_list(lapply(prms, "[[", "tags"))

        if (any(duplicated(c(private$.id, unique(private$.tags))))) {
          stop("ids and tags must have different names.")
        }
      }

      invisible(self)
    },

    # FIXME - ADD HIDE COLS FOR TRAFOS, DEPS, CHECKS
    print = function(sort = TRUE) {
      # hide_cols = c("Parent", "Trafo"),
      #checkmate::assert_subset(hide_cols, c("Id", "Support", "Value", "Tags", "Parent", "Trafo"))

      dt <- suppressWarnings(as.data.table(self, sort = sort))
      dt$Support <- sapply(dt$Support, function(x) x$strprint())

      # if (!("Parent" %in% hide_cols) & length(self$deps)) {
      #   deps <- aggregate(on ~ id, data = self$deps, FUN = string_as_set)
      #   colnames(deps) <- c("Id", "Parent")
      #   dt <- merge(dt, deps)
      # }

      # if (!("Trafo" %in% hide_cols) & length(self$trafos)) {
      #   dt$Trafo <- ""
      #   dt[Id %in% self$trafos$id, "Trafo"] <- TRUE
      # }

      #print(dt[, setdiff(colnames(dt), hide_cols), with = FALSE])
      print(dt)
    },

    # FIXME - DOCUMENT
    add = function(prms = list()) {

      if (length(prms)) {
        checkmate::assert_list(prms, "prm6", any.missing = FALSE)

        ids <- vapply(prms, "[[", character(1), "id")
        names(prms) <- ids
        if (any(duplicated(c(ids, private$.id)))) {
          stop("ids are not unique or already existed in ParameterSet.")
        } else {
          env_append(private, ".id", ids)
          env_append(private, ".supports", vapply(prms, "[[", character(1), "support"))
          env_append(private, ".value", un_null_list(lapply(prms, "[[", "value")))
          env_append(private, ".tags", un_null_list(lapply(prms, "[[", "tags")))
          private$.isupports <- invert_names(private$.supports)
        }
      } else {
        stop("At least one parameter must be added.")
      }

      invisible(self)
    },

    # FIXME - ADD DEPS, TRAFOS, CHECKS
    remove = function(...) {

      if (!is.null(private$.trafo)) {
        warning("Rransformations are not included in extraction.")
      }

      pars <- unlist(list(...))
      private$.support[pars] <- NULL
      private$.value[pars] <- NULL
      private$.tags[pars] <- NULL
      # fix for vectorised pars
      private$.deps <- subset(private$.deps, !(grepl(id, pars) | grepl(on, pars)))
      private$.trafo[pars] <- NULL
      # FIXME - remove checks
      subset(private$.checks, grepl(pars, params))


      invisible(self)
    },

    # FIXME - DOCUMENT
    get_values = function(id = NULL, tags = NULL, transform = TRUE, inc_null = TRUE,
                          simplify = TRUE) {
      .get_values(self, private, private$.value, id, tags, transform, inc_null, simplify)
    },

    # FIXME - DOCUMENT
    add_dep = function(id, on, cnd) {

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
    },

    # FIXME - DOCUMENT
    transform = function() {
      if (!is.null(private$.trafo)) {
        private$.value <- private$.trafo(self$values, self)
      }

      invisible(self)
    },

    # FIXME - ADD TESTS & DOCUMENT
    add_check = function(fun, ids = NULL, tags = NULL) {

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
    },

    # FIXME - DOCUMENT
    check = function(supports = TRUE, custom = TRUE, deps = TRUE, id = NULL,
                     error_on_fail = TRUE) {
      .check(self, supports, custom, deps, id, error_on_fail, value_check = self$values,
             support_check = private$.isupports, dep_check = self$deps, custom_check = self$checks)
    },

    # FIXME - DOCUMENT
    rep = function(times, prefix) {
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
    },

    # FIXME - ADD CHECKS TO EXTRACT
    extract = function(id = NULL, tags = NULL, prefix = NULL) {

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
  ),

  active = list(
    #' @field supports
    #' Get supports from the parameter set.
    supports = function() {
      sups <- support_dictionary$get_list(private$.supports)
      names(sups) <- self$ids
      sups
    },

    #' @field tags
    #' Get tags from the parameter set.
    tags = function() {
      private$.tags
    },

    #' @field ids
    #' Get ids from the parameter set.
    ids = function() {
      private$.id
    },

    #' @field length
    #' Get the length of the parameter set as the number of parameters.
    length = function() {
      length(self$ids)
    },

    # FIXME - DOCUMENT
    values = function(vals) {
      if (missing(vals)) {
        return(private$.value)
      } else {
        .check(self, id = names(vals), value_check = vals,
               support_check = private$.isupports, dep_check = self$deps,
               custom_check = self$checks)

        private$.value <- vals
        invisible(self)
      }
    },

    #' @field deps
    #' Get parameter dependencies, NULL if none.
    deps = function() {
      private$.deps
    },

    #' @field checks
    #' Get custom parameter checks, NULL if none.
    checks = function() {
      private$.checks
    },

    # FIXME - DOCUMENT
    trafo = function(x) {
      if (missing(x)) {
        private$.trafo
      } else {
        checkmate::assert_function(x, args = c("x", "self"))
        vals <- x(self$values, self)
        checkmate::assert_list(vals)

        tryCatch(.check(self, id = names(vals), value_check = vals,
                        support_check = private$.isupports, dep_check = self$deps,
                        custom_check = self$checks),
                 error = function(e) {
                   stop("Transformation results in values outside of supports.")
                 })

        private$.trafo <- x
        invisible(self)
      }
    }
  ),

  private = list(
    .id = list(),
    .isupports = list(),
    .supports = list(),
    .value = list(),
    .tags = list(),
    .trafo = NULL,
    .deps = NULL,
    .checks = NULL,
    deep_clone = function(name, value) {
      switch(name,
        ".deps" = {
          if (!is.null(value)) {
            data.table::copy(value)
          }
        },
        ".checks" = {
          if (!is.null(value)) {
            data.table::copy(value)
          }
        },
        value
      )
    }
  )
)

#' @export
pset <- function(prms) {
  ParameterSet$new(prms)
}
