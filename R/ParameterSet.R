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
      }

      invisible(self)
    },

    # TODO
    # similar to paradox. calls params, merges trafos and dependencies. prints requested columns
    print = function(sort = TRUE) { # hide_cols = c("Parent", "Trafo"),
      #checkmate::assert_subset(hide_cols, c("Id", "Support", "Value", "Tags", "Parent", "Trafo"))

      dt <- as.data.table(self, sort = sort)
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

    # TODO
    # takes character arguments specifying parameter ids and removes associated values.
    # needs more work as currently ignores deps and trafos
    remove = function(...) {
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

    # FIXME - subsets the ParameterSet. needs further work as trafos and deps ignored
    subset = function(ids) {
      checkmate::assert_subset(ids, self$ids)
      ids <- intersect(self$ids, ids)
      private$.support <- self$supports[names(self$supports) %in% ids]
      private$.value <- self$values[names(self$values) %in% ids]
      private$.tags <- self$tags[names(self$tags) %in% ids]
      private$.deps <- subset(private$.deps, id %in% ids)
      private$.trafo <- self$trafos[names(self$trafos) %in% ids]

      invisible(self)
    },

    # FIXME - DOCUMENT
    add_dep = function(id, on, cnd) {

      if (!requireNamespace("data.table", quietly = TRUE)) {
        stop("{data.table} required for adding dependencies.")
      }

      checkmate::assert_class(cnd, "cnd")
      if (id == on) {
        stop("Parameters cannot depend on themselves.")
      }

      # hacky fix
      aid <- id
      aon <- on

      if (nrow(subset(private$.deps, grepl(aid, id) & grepl(aon, on)))) {
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

    # Used to compare parameter values between each other. One function calling `self`, boolean
    # conditions 'added' together to form a single function.
    # add_check = function(params, fun) {
    #   if (is.null(self$checks)) {
    #     checks <- data.table(params = character(0L), fun = list())
    #   } else {
    #     checks <- self$checks
    #   }
    #       private$.checks <- rbind(private$.checks,
    #                               data.table(params = list(checkmate::assertSubset(params, self$ids)),
    #                                         fun = list(body(checkmate::assertFunction(fun, "self")))))
    #       invisible(self)
    # },

    # FIXME - DOCUMENT
    check = function(supports = TRUE, custom = TRUE, deps = TRUE, id = NULL,
                     error_on_fail = TRUE) {
      .check(self, supports, custom, deps, id, error_on_fail, support_check = private$.isupports,
             dep_check = self$deps, custom_check = self$checks)
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

    # FIXME - DOCUMENT
    extract = function(id = NULL, rm_prefix = TRUE) {

      which_ids <- paste0(id, collapse = "|")
      ids <- self$ids[grepl(which_ids, self$ids)]
      supports <- private$.supports[grepl(which_ids, names(private$.supports))]
      values <- expand_list(self$ids, self$values)
      values <- values[grepl(which_ids, names(values))]
      tags <- expand_list(self$ids, self$tags)
      tags <- tags[grepl(which_ids, names(tags))]

      if (isTRUE(rm_prefix) && any(grepl("__", ids, fixed = TRUE))) {
        prefixes <- vapply(strsplit(ids, "__", fixed = TRUE), function(x) {
          if (length(x) > 1) {
            x[[1]]
          } else {
            NA_character_
          }
        }, character(1))
        if (any(id %in% prefixes)) {
          ids <- vapply(strsplit(ids, paste0(prefixes, "__")), function(x) {
            tryCatch(x[[2]], error = function(e) x[[1]])
          }, character(1))
        }

      } else if (checkmate::test_character(rm_prefix) &&
                  any(grepl("__", ids, fixed = TRUE)) &&
                  any(grepl(paste0(rm_prefix, collapse = "|"), ids))) {

        for (i in seq_along(rm_prefix)) {
          .x <- rm_prefix[[i]]
           which <- grepl(.x, ids)
          if (any(which)) {
            ids[which] <- substr(ids[which], nchar(.x) + 3, 1000)
          }
        }
        rm(.x)
      }

      as.ParameterSet(
        unname(Map(prm,
          id = ids,
          support = supports,
          value = values,
          tags = tags,
          .check = FALSE
        ))
      )
    }
    ),

  active = list(
    # FIXME - DOCUMENT
    supports = function() {
      sups <- support_dictionary$get_list(private$.supports)
      names(sups) <- self$ids
      sups
    },

    # FIXME - DOCUMENT
    tags = function() {
      private$.tags
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

    # FIXME - DOCUMENT
    ids = function() {
      private$.id
    },

    # FIXME - DOCUMENT
    length = function() {
      length(self$ids)
    },

    # FIXME - DOCUMENT
    deps = function() {
      .x = private$.deps
      if (!is.null(.x)) {
        .x
      } else {
        NULL
      }
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
    },

    # FIXME - DOCUMENT
    checks = function() {
      .x = private$.checks
      if (!is.null(.x)) {
        .x
      } else {
        NULL
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
as.data.table.ParameterSet <- function(x, sort = TRUE, string = FALSE, ...) { # nolint
  if (length(x$deps) || length(x$trafos) || length(x$checks)) {
    warning("Dependencies, trafos, and checks are lost in coercion.")
  }
  dt = data.table::data.table(
    Id = x$ids,
    Support = x$supports,
    Value = expand_list(x$ids, x$values),
    Tags = expand_list(x$ids, x$tags)
  )
  if (sort) {
    Id = NULL # visible binding fix
    data.table::setorder(dt, Id)
  }
  dt
}

#' @export
pset <- function(prms) {
  ParameterSet$new(prms)
}

#' @export
as.ParameterSet <- function(x, ...) { # nolint
  UseMethod("as.ParameterSet")
}

#' @export
as.ParameterSet.data.table <- function(x, ...) { # nolint
  ParameterSet$new(as.prm(x))
}

#' @export
as.ParameterSet.prm <- function(x, ...) { # nolint
  ParameterSet$new(list(x))
}

#' @export
as.ParameterSet.list <- function(x, ...) { # nolint
  checkmate::assert_list(x, "prm", any.missing = FALSE)
  ParameterSet$new(x)
}

#' @export
length.ParameterSet <- function(x) {
  x$length
}

#' @export
rep.ParameterSet <- function(x, times, prefix, ...) {
  x <- x$clone(deep = TRUE)
  x$rep(times, prefix)
  x
}

#' @export
c.ParameterSet <- function(...) {
  ParameterSet$new(unlist(lapply(list(...), as.prm), FALSE))
}
