#' @examples
#' library(set6)
#'
#' # Method 1: Construct ParameterSet with formula interface
#' # One parameter, supported on Reals with value 1
#' ParameterSet$new(a = Reals$new() ~ 1)
#'
#' # One named parameter, supported on Reals with value 1 and two tags
#' ParameterSet$new(a = Reals$new() ~ 1 + tags(t1, t2))
#'
#' # Multiple parameters
#' ParameterSet$new(
#'  r = Reals$new(),
#'  n = Naturals$new() ~ 1,
#'  l = Logicals$new() ~ FALSE + tags(log)
#' )
#'
#' # Method 2: Construct ParameterSet with other arguments
#' # One parameter, supported on Reals with no value
#' ParameterSet$new(id = 'a', support = Reals$new())
#'
#' # One parameter, supported on Reals with value 1
#' ParameterSet$new(id = 'a', support = Reals$new(), value = 1)
#'
#' # One named parameter, supported on Reals with value 1 and two tags
#' ParameterSet$new(id = 'a', support = Reals$new(), value = 1, tag = c("t1", "t2"))
#'
#' # Multiple parameters
#' ParameterSet$new(
#'  id = list('r', 'n', 'l'),
#'  support = list(Reals$new(), Naturals$new(), Logicals$new()),
#'  value = list(NULL, 1, FALSE),
#'  tag = list(NULL, NULL, 'log')
#' )
#'
#' @export
ParameterSet <- R6::R6Class("ParameterSet",
  public = list(
    initialize = function(..., id = NULL, support = NULL, value = NULL, tag = NULL) {

      if (is.null(is) && !...length()) {
        invisible(self)
      } else {
        if (...length()) {
          params <- param_formula_to_list(list(...))
          if (is.null(unlist(params[2, ]))) value = NULL else value = params[2, ]
          if (is.null(unlist(params[3, ]))) tag = NULL else tag = params[3, ]
          params <- list(
            id = checkmate::assertNames(names(list(...)), "strict"),
            support = params[1, ],
            value = value,
            tag = tag
          )
        } else {
          lng <- length(id)
          if (!is.null(value)) value <- checkmate::assertList(value, len = lng)
          if (!is.null(tag)) tag <- checkmate::assertList(tag, len = lng)
          params <- list(
            id = checkmate::assertNames(unlist(id), "strict"),
            support = checkmate::assertList(support, "Set", len = lng),
            value = value,
            tag = tag
          )
        }

        private$.id = params$id
        private$.support = params$support
        names(private$.support) = private$.id

        # assert value
        if (!is.null(params$value)) {
          mapply(
            function(x, y) if (!is.null(y)) assert_contains(x, y),
            params$support, params$value
          )
          private$.value <- params$value
          names(private$.value) <- private$.id
          # remove NULL
          private$.value[sapply(private$.value, is.null)] <- NULL
        }

        if (!is.null(params$tag)) {
          private$.tag <- params$tag
          names(private$.tag) <- private$.id
          # remove NULL
          private$.tag[sapply(private$.tag, is.null)] <- NULL
        }
      }

      invisible(self)
    },

    # similar to paradox. calls params, merges trafos and dependencies. prints requested columns
    print = function(hide_cols = c("Parent", "Trafo"), sort = TRUE) {
      checkmate::assert_subset(hide_cols, c("Id", "Support", "Value", "Tag", "Parent", "Trafo"))

      dt <- as.data.table(self, sort = sort)
      dt$Support <- sapply(dt$Support, function(x) x$strprint())

      if (!("Parent" %in% hide_cols) & length(self$deps)) {
        deps <- aggregate(on ~ id, data = self$deps, FUN = string_as_set)
        colnames(deps) <- c("Id", "Parent")
        dt <- merge(dt, deps)
      }

      if (!("Trafo" %in% hide_cols) & length(self$trafos)) {
        dt$Trafo <- ""
        dt[Id %in% self$trafos$id, "Trafo"] <- TRUE
      }

      print(dt[, setdiff(colnames(dt), hide_cols), with = FALSE])
    },

    # adds new parameters to the set. provides formula construction only
    add = function(...) {

      params <- param_formula_to_list(list(...))
      if (is.null(unlist(params[2, ]))) value = NULL else value = params[2, ]
      if (is.null(unlist(params[3, ]))) tag = NULL else tag = params[3, ]
      params <- list(
        id = checkmate::assertNames(names(list(...)), "strict"),
        support = params[1, ],
        value = value,
        tag = tag
      )

      # check new names unique
      checkmate::assert(!any(params$id %in% private$.id))
      new_ids = params$id
      new_support = params$support

      # assert value
      if (!is.null(params$value)) {
        mapply(
           function(x, y) if (!is.null(y)) assert_contains(x, y),
           params$support, params$value
        )
        new_values = params$value
        names(new_values) <- new_ids
        new_values[sapply(new_values, is.null)] <- NULL
      } else {
          new_values <- NULL
      }

      if (!is.null(params$tag)) {
        new_tags <- params$tag
        names(new_tags) <- new_ids
        new_tags[sapply(new_tags, is.null)] <- NULL
      } else {
        new_tags <- NULL
      }

      private$.id <- c(private$.id, new_ids)
      private$.support <- c(private$.support, new_support)
      private$.value <- c(private$.value, new_values)
      private$.tag <- c(private$.tag, new_tags)

      invisible(self)
    },

    # takes character arguments specifying parameter ids and removes associated values.
    # needs more work as currently ignores deps and trafos
    remove = function(...) {
      pars <- unlist(list(...))
      private$.support[pars] <- NULL
      private$.value[pars] <- NULL
      private$.tag[pars] <- NULL
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
    get_values = function(tag = NULL) {
      if (is.null(tag)) {
        return(self$values[!sapply(self$values, is.null)])
      } else {
        return(self$values[names(self$values) %in% names(self$tags)[grepl(tag, self$tags)]])
      }
    },

    # subsets the ParameterSet. needs further work as trafos and deps ignored
    subset = function(ids) {
      checkmate::assert_subset(ids, self$ids)
      ids <- intersect(self$ids, ids)
      private$.support <- self$supports[names(self$supports) %in% ids]
      private$.value <- self$values[names(self$values) %in% ids]
      private$.tag <- self$tags[names(self$tags) %in% ids]
      private$.deps <- subset(private$.deps, id %in% ids)
      private$.trafo <- self$trafos[names(self$trafos) %in% ids]

      invisible(self)
    },

    # adds dependencies to ParameterSet. instead of creating a new Condition object, just allows a
    # small number of possible conditions and each has identical RHS formulation.
    # assert_no_cycles prevents a cycle of dependencies, see helpers.R
    # assert_condition ensures that the condition is either possible (if 'Equal' or 'AnyOf') or
    # just not redundant (if 'NotEqual' or 'NotAnyOf'), see helpers.R
    add_dep = function(id, on, type = c("Equal", "NotEqual", "AnyOf", "NotAnyOf"), cond) {
      checkmate::assert_subset(c(id, on), self$ids)
      if (id == on) {
        stop("Parameters cannot depend on themselves.")
      }
      type <- match.arg(type)

      # hacky fix
      aid <- id
      aon <- on
      if (nrow(subset(private$.deps, id == aid & on == aon)) > 0) {
        stop(sprintf("%s already depends on %s.", id, on))
      }

      assert_condition(on, self$supports[on][[1]], type, cond)

      newDT <- rbind(private$.deps, data.table(id = id, on = on, type = type, cond = cond)) # nolint
      assert_no_cycles(newDT)
      private$.deps <- newDT

      invisible(self)
    },

    # adds trafo either to given parameter ids or to the whole ParameterSet "<Set>".
    # currently no feasibility checks, e.g. if LogicalSet has trafo 'exp'
    add_trafo = function(id, fun) {
      if (checkmate::test_names(id, identical.to = "<Set>")) {
        checkmate::assert_function(fun, args = c("x", "param_set"), null.ok = TRUE)
        private$.trafo <- c(private$.trafo, list("<Set>" = fun))
      } else {
        nin <- !(id %in% c(self$ids))
        if (any(nin)) {
          stop(sprintf(
            "Parameter(s) %s not available. Must be a valid id or <Set>.",
            string_as_set(id[nin])
          ))
        } else {
          checkmate::assert_function(fun)
          lst = list(fun)
          names(lst) = id
          private$.trafo <- c(private$.trafo, lst)
        }
      }
      invisible(self)
    },

    # Used to compare parameter values between each other. One function calling `self`, boolean
    # conditions 'added' together to form a single function.
add_check = function(params, fun) {
      private$.checks <- rbind(private$.checks,
                               data.table(params = list(checkmate::assertSubset(params, self$ids)),
                                        fun = list(body(checkmate::assertFunction(fun, "self")))))
      invisible(self)
    },

check = function() {
  if (length(self$checks)) {
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
      private$.support
    },

    tags = function() {
      private$.tag
    },

    values = function(vals) {
      if (missing(vals)) {
        return(private$.value)
      } else {
        vals <- vals[names(vals) %in% self$ids]
        mapply(function(x, y) if (!is.null(y)) assert_contains(x, y), self$supports[names(vals)],
               vals)
        private$.value <- vals[order(names(vals))]
      }
    },

    # change to public and add filters?
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
    .support = list(),
    .value = list(),
    .tag = list(),
    .trafo = list(),
    .deps = data.table(id = character(0L), on = character(0L), type = character(0L), cond = list()),
    .checks = data.table(params = character(0L), fun = list()),
    deep_clone = function(name, value) {
      switch(name,
        ".support" = sapply(value, function(x) x$clone(deep = TRUE)),
        ".deps" = data.table::copy(value),
        ".checks" = data.table::copy(value),
        value
      )
    }
  )
)

#' @export
as.data.table.ParameterSet <- function(x, sort = TRUE, ...) { # nolint
  if (length(x$deps) || length(x$trafos) || length(x$checks)) {
    warning("Dependencies, trafos, and checks are lost in coercion.")
  }
  dt = data.table(
    Id = x$ids,
    Support = x$supports,
    Value = partial_list(x$ids, x$values),
    Tag = partial_list(x$ids, x$tags)
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
  checkmate::assertSubset(colnames(x), c("Id", "Support", "Value", "Tag"))
  ParameterSet$new(id = x$Id,
                   support = x$Support,
                   value = x$Value,
                   tag = x$Tag)
}

# less efficient than $add, needs work
#' @export
rbind.ParameterSet <- function(...) {
  ps <- list(...)
  as.ParameterSet(data.table::rbindlist(lapply(ps, as.data.table)))
}
