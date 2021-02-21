#' @export
ParameterSet <- R6::R6Class("ParameterSet",
  public = list(
    initialize = function(prms = list()) {

      if (length(prms)) {
        checkmate::assert_list(prms, "prm6", any.missing = FALSE)

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
        if ("c" %in% unlist(private$.tags)) {
          stop("'c' is a reserved tag in param6.")
        }
      }

      self$check(deps = FALSE, custom = FALSE)

      invisible(self)
    },

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

    # adds new parameters to the set. provides formula construction only
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

      self$check(custom = FALSE)

      invisible(self)
    },

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
      # FIXME - hacky concatenation fix
      if ("c" %in% pars) {

      } else {
        # fix for vectorised pars
        subset(private$.checks, grepl(pars, params))
      }


      invisible(self)
    },

    # different from $values as $values returns a list of values (which may be NULL) whereas
    # this returns only set values, which can be filtered by tags.
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

    # adds dependencies to ParameterSet. instead of creating a new Condition object, just allows a
    # small number of possible conditions and each has identical RHS formulation.
    # assert_no_cycles prevents a cycle of dependencies, see helpers.R
    # assert_condition ensures that the condition is either possible (if 'Equal' or 'AnyOf') or
    # # just not redundant (if 'NotEqual' or 'NotAnyOf'), see helpers.R
    add_dep = function(id, on, cnd) {
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

      newDT <- rbind(private$.deps, data.table::data.table(id = id, on = on, cond = list(cnd))) # nolint
      assert_no_cycles(newDT)

      .check_deps(self, self$values, newDT, id, TRUE)

      private$.deps <- newDT

      invisible(self)
    },

    transform = function() {
      private$.trafo(self)
    },

    # Used to compare parameter values between each other. One function calling `self`, boolean
    # conditions 'added' together to form a single function.
    # add_check = function(params, fun) {
    #       private$.checks <- rbind(private$.checks,
    #                               data.table(params = list(checkmate::assertSubset(params, self$ids)),
    #                                         fun = list(body(checkmate::assertFunction(fun, "self")))))
    #       invisible(self)
    # },

    check = function(supports = TRUE, custom = TRUE, deps = TRUE, id = NULL,
                     error_on_fail = TRUE) {
      .check(self, supports, custom, deps, id, error_on_fail, support_check = private$.isupports,
             dep_check = self$deps, custom_check = self$checks)
    },

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

      if (!is.null(self$trafo)) {
        warning("'trafo' should be replicated manually.")
      }

      invisible(self)

        # FIXME - ADD DEPS AND CHECKS
        # .deps = data.table(id = character(0L), on = character(0L), type = character(0L), cond = list()),
        # .checks = data.table(params = character(0L), fun = list()),
    }
    ),

  active = list(
    supports = function() {
      sups <- support_dictionary$get_list(private$.supports)
      names(sups) <- self$ids
      sups
    },

    tags = function() {
      private$.tags
    },

    values = function(vals) {
      if (missing(vals)) {
        return(private$.value)
      } else {
        .check(self, id = names(vals), value_check = vals,
               support_check = private$.isupports, dep_check = self$deps,
               custom_check = self$checks)

        private$.value <- vals
      }
    },

    ids = function() {
      private$.id
    },

    length = function() {
      length(self$ids)
    },

    deps = function() {
      .x = private$.deps
      if (nrow(.x)) {
        .x
      } else {
        NULL
      }
    },

    trafo = function(x) {
      if (missing(x)) {
        private$.trafo
      } else {
        checkmate::assert_function(x, args = "x")
        checkmate::assert_list(x(self$values))
        private$.trafo <- x
        invisible(self)
      }
    },

    checks = function() {
      .x = private$.checks
      if (nrow(.x)) {
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
    .deps = data.table(id = character(0L), on = character(0L), cond = list()),
    .checks = data.table(params = character(0L), fun = list()),
    deep_clone = function(name, value) {
      switch(name,
        ".deps" = data.table::copy(value),
        ".checks" = data.table::copy(value),
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
  dt = data.table(
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
as.ParameterSet <- function(x, ...) { # nolint
  UseMethod("as.ParameterSet")
}
#' @export
as.ParameterSet.data.table <- function(x, ...) { # nolint
  checkmate::assertSubset(colnames(x), c("Id", "Support", "Value", "Tags"))
  ParameterSet$new(id = x$Id,
                   support = x$Support,
                   value = x$Value,
                   tags = x$tags)
}

# FIXME
# less efficient than $add, needs work
#' @export
# rbind.ParameterSet <- function(...) {
#   ps <- list(...)
#   as.ParameterSet(data.table::rbindlist(lapply(ps, as.data.table)))
# }

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
