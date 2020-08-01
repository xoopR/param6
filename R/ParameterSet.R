#' @export
ParameterSet <- R6::R6Class("ParameterSet",
  public = list(
    initialize = function(..., support = NULL, value = NULL, tag = NULL) {

      if (is.null(support) & !...length()) {
        invisible(self)
      }

      if (...length() || !is.null(support)) {
        if (!is.null(support)) {

          if (checkmate::testList(support)) {
            assertSetList(support)
          }
          checkmate::assert_names(names(support), type = "strict")
          private$.support <- support[order(names(support))]

          lng <- length(support)

          if (!is.null(tag)) {
            checkmate::assertList(tag, len = lng)
            names(tag) <- names(support)
            private$.tag <- tag
          }

          if (!is.null(value)) {
            checkmate::assertList(value, len = lng)
            names(value) <- names(support)
            mapply(function(x, y) if (!is.null(y)) assert_contains(x, y), support, value)
            private$.value <- value
          }

        } else {
          params <- sapply(list(...), make_param)
          if (ncol(params) > 1) {
            params = params[, order(colnames(params))]
          }
          private$.support <- params[1, ]
          names(private$.support) <- colnames(params)
          private$.value <- params[2, ]
          names(private$.value) <- colnames(params)
          if (!all(is.null(unlist(params[3, ])))) {
            private$.tag <- params[3, ]
            names(private$.tag) <- colnames(params)
          }

        }

        private$.tag <- private$.tag[!sapply(private$.tag, is.null)]
        private$.value <- private$.value[!sapply(private$.value, is.null)]
      }

      invisible(self)
    },

    # similar to paradox. calls params, merges trafos and dependencies. prints requested columns
    print = function(hide_cols = c("Parent", "Trafo")) {
      checkmate::assert_subset(hide_cols, c("Id", "Support", "Value", "Tag", "Parent", "Trafo"))

      dt <- as.data.table(self)
      dt$Support <- sapply(dt$Support, function(x) x$strprint())

      if (!("Parent" %in% hide_cols) & nrow(self$deps)) {
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
    # code is not great here
    add = function(...) {
      psnew <- ParameterSet$new(...)
      sets <- list(self, psnew)
      checkmate::assert_names(unlist(sapply(sets, function(x) x$ids)), type = "strict")

      private$.support <- sort_named_list(unlist(lapply(
        sets, function(.x) .x$supports), recursive = FALSE))
      private$.value <- sort_named_list(unlist(lapply(
        sets, function(.x) .x$values), recursive = FALSE))
      private$.tag <- sort_named_list(unlist(lapply(
        sets, function(.x) .x$tags), recursive = FALSE))

      invisible(self)
    },

    # takes character arguments specifying parameter ids and removes associated values.
    # needs more work as currently ignores deps and trafos
    remove = function(...) {
      params <- unlist(list(...))
      private$.support[params] <- NULL
      private$.value[params] <- NULL
      private$.tag[params] <- NULL

      invisible(self)
    },

    # different from $values as $values returns a list of set values whereas this returns a list
    # of all parameters with either the set value or NULL. in construction defaults are set as
    # values.
    get_values = function(tag = NULL) {
      if (is.null(tag)) {
        return(self$values)
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
    add_check = function(fun) {
      if (is.null(self$checks)) {
        private$.checks <- checkmate::assertFunction(fun, "self")
      } else {
        f1 <- self$checks
        f2 <- checkmate::assertFunction(fun, "self")
        body(f1) <- substitute(b1 && b2, list(b1 = body(f1), b2 = body(f2)))
        private$.checks <- f1
      }

      invisible(self)
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
      names(private$.support)
    },

    length = function() {
      length(self$ids)
    },

    deps = function() {
      private$.deps
    },

    trafos = function() {
      private$.trafo
    },

    checks = function() {
      private$.checks
    }
  ),

  private = list(
    .support = list(),
    .value = list(),
    .tag = list(),
    .trafo = list(),
    .deps = data.table(id = character(0L), on = character(0L), type = character(0L), cond = list()),
    .checks = NULL,
    deep_clone = function(name, value) {
      switch(name,
        ".support" = sapply(value, function(x) x$clone(deep = TRUE)),
        ".deps" = data.table::copy(value),
        value
      )
    }
  )
)

#' @export
as.data.table.ParameterSet <- function(x, ...) { # nolint
  Id = x$ids
  lst = vector("list", length(Id))
  names(lst) = Id
  Value = Tag = lst
  Value[Id %in% names(x$values)] = x$values
  Tag[Id %in% names(x$tags)] = x$tags

  data.table(
    Id = Id, Support = x$supports,
    Value = Value, Tag = Tag
  )
}

#' @export
as.ParameterSet <- function(x, ...) { # nolint
  UseMethod("as.ParameterSet")
}
#' @export
as.ParameterSet.data.table <- function(x, ...) { # nolint
  checkmate::assertSubset(colnames(x), c("Id", "Support", "Value", "Tag"))
  assertSetList(x$Support)
  checkmate::assert_names(x$Id, type = "strict")
  support <- x$Support

  value <- x$Value
  if (!is.null(value)) names(value) <- x$Id
  tag <- x$Tag
  if (!is.null(tag)) names(tag) <- x$Id

  names(support) <- x$Id
  ParameterSet$new(support = support, value = value, tag = tag)
}

# less efficient than $add, needs work
#' @export
rbind.ParameterSet <- function(...) {
  ps <- list(...)
  as.ParameterSet(data.table::rbindlist(lapply(ps, as.data.table)))
}
