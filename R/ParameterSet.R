prm <- function(id, support, value = NULL, tags = NULL) {
  checkmate::assert_character(id, len = 1)
  # if character check to see if exists in dictionary otherwise error
  if (checkmate::test_character(support, len = 1)) {
    if (!support_dictionary$has(support)) {
      stop("'suppport' given as character but does not exist in support_dictionary.")
    }
  # if Set check to see if exists in dictionary otherwise add and return string
  } else if (checkmate::test_class(support, "Set")) {
    str_support <- as.character(support)
    if (!support_dictionary$has(str_support)) {
      orig_uni <- set6::useUnicode()
      set6::useUnicode(FALSE)
      support_dictionary$add(keys = str_support, values = support)
      set6::useUnicode(orig_uni)
    }
    support <- str_support
  } else {
    stop("'support' should be given as a character scalar or Set.")
  }
  if (!is.null(tags)) {
    checkmate::assert_character(tags, null.ok = TRUE)
  }
  param <- list(id = id, support = support, value = value, tags = tags)
  class(param) <- "prm6"
  param
}

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

      self$check(custom = FALSE)

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
      # hacky concatenation fix
      if ("c" %in% pars) {

      } else {
        # fix for vectorised pars
        subset(private$.checks, grepl(pars, params))
      }


      invisible(self)
    },

    # different from $values as $values returns a list of values (which may be NULL) whereas
    # this returns only set values, which can be filtered by tags.
    get_values = function(tags = NULL, inc.null = TRUE) {

      values <- self$values
      if (inc.null) {
        values <- expand_list(self$ids, self$values)
      }
      if (!is.null(tags)) {
        which <- names(self$tags)[(grepl(tags, self$tags))]
        values <- values[match(which, names(values), 0L)]
      }

      values
    },

    # subsets the ParameterSet. needs further work as trafos and deps ignored
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
    # add_dep = function(id, on, type = c("Equal", "NotEqual", "AnyOf", "NotAnyOf"), cond) {
    #   checkmate::assert_subset(c(id, on), self$ids)
    #   if (id == on) {
    #     stop("Parameters cannot depend on themselves.")
    #   }
    #   type <- match.arg(type)

    #   # hacky fix
    #   aid <- id
    #   aon <- on
    #   if (nrow(subset(private$.deps, id == aid & on == aon)) > 0) {
    #     stop(sprintf("%s already depends on %s.", id, on))
    #   }

    #   assert_condition(on, self$supports[on][[1]], type, cond)

    #   newDT <- rbind(private$.deps, data.table(id = id, on = on, type = type, cond = cond)) # nolint
    #   assert_no_cycles(newDT)
    #   private$.deps <- newDT

    #   invisible(self)
    # },

    # adds trafo either to given parameter ids or to the whole ParameterSet "<Set>".
    # currently no feasibility checks, e.g. if LogicalSet has trafo 'exp'
    # add_trafo = function(id, fun) {
    #   if (checkmate::test_names(id, identical.to = "<Set>")) {
    #     checkmate::assert_function(fun, args = c("x", "param_set"), null.ok = TRUE)
    #     private$.trafo <- c(private$.trafo, list("<Set>" = fun))
    #   } else {
    #     nin <- !(id %in% c(self$ids))
    #     if (any(nin)) {
    #       stop(sprintf(
    #         "Parameter(s) %s not available. Must be a valid id or <Set>.",
    #         string_as_set(id[nin])
    #       ))
    #     } else {
    #       checkmate::assert_function(fun)
    #       lst = list(fun)
    #       names(lst) = id
    #       private$.trafo <- c(private$.trafo, lst)
    #     }
    #   }
    #   invisible(self)
    # },

    # Used to compare parameter values between each other. One function calling `self`, boolean
    # conditions 'added' together to form a single function.
    # add_check = function(params, fun) {
    #       private$.checks <- rbind(private$.checks,
    #                               data.table(params = list(checkmate::assertSubset(params, self$ids)),
    #                                         fun = list(body(checkmate::assertFunction(fun, "self")))))
    #       invisible(self)
    # },

    check = function(supports = TRUE, custom = TRUE) {
      # 1. Containedness checks
      if (supports && length(self)) {
        for (i in seq_along(private$.isupports)) {
          value <- private$.value[names(private$.value) %in% private$.isupports[[i]]]
          if (!length(value)) {
            value <- NULL
          }
          assert_contains(
            set = support_dictionary$get(names(private$.isupports)[[i]]),
            value = value
          )
        }
      }

    # 2. Custom checks
    if (custom && length(self$checks)) {
      all(sapply(self$checks$fun, function(x) {
      as.function(list(self = self, x))()
      }))
        } else {
          TRUE
        }
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
        old <- private$.value
        private$.value <- vals
        tryCatch(self$check(),
          error = function(e) {
            private$.value <- old
            stop(e)
        })
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

    trafos = function() {
      .x = private$.trafo
      if (length(.x)) {
        .x
      } else {
        NULL
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
    .trafo = list(),
    .deps = data.table(id = character(0L), on = character(0L), type = character(0L), cond = list()),
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

# less efficient than $add, needs work
#' @export
rbind.ParameterSet <- function(...) {
  ps <- list(...)
  as.ParameterSet(data.table::rbindlist(lapply(ps, as.data.table)))
}

#' @export
length.ParameterSet <- function(x) {
  x$length
}
