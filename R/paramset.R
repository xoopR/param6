ParameterSet <- R6::R6Class("ParameterSet",
  public = list(
    # provides two constructor methods, one as formulas, the other as lists. if lists are chosen
    # only the support needs to have names, value and tag are copied.
    # makeParams 'constructs' parameters, see makeParam.R
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
          private$.support <- support
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
          params <- sapply(list(...), makeParam)

          private$.support <- params[1, ]
          names(private$.support) <- colnames(params)
          private$.value <- params[2, ]
          names(private$.value) <- colnames(params)
          if (!all(is.null(unlist(params[3, ])))) {
            private$.tag <- params[3, ]
            names(private$.tag) <- colnames(params)
          }
        }

        private$.value <- private$.value[!sapply(private$.value, is.null)]
      }

      invisible(self)
    },

    # similar to paradox. calls params, merges trafos and dependencies. prints requested columns
    print = function(hide_cols = c("Parent", "Trafo")) {
      checkmate::assert_subset(hide_cols, c("Id", "Support", "Value", "Tag", "Parent", "Trafo"))

      dt <- self$params
      dt$Support <- sapply(dt$Support, function(x) x$strprint())
      ftag <- sapply(dt$Tag, function(x) if (!is.null(x)) paste0("{", paste0(x, collapse = ", "), "}"))
      if (length(ftag) != 1 | !is.null(ftag[[1]])) {
        dt$Tag <- ftag
      }

      if (!("Parent" %in% hide_cols) & self$has_deps) {
        deps <- aggregate(on ~ id, data = self$deps, FUN = string_as_set)
        colnames(deps) <- c("Id", "Parent")
        dt <- merge(dt, deps)
      }

      if (!("Trafo" %in% hide_cols) & self$has_trafos) {
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

      # messy needs fixing
      self$.__enclos_env__$private$.support <- unlist(lapply(sets, function(x) x$.__enclos_env__$private$.support), recursive = FALSE)
      self$.__enclos_env__$private$.value <- unlist(lapply(sets, function(x) x$.__enclos_env__$private$.value), recursive = FALSE)
      self$.__enclos_env__$private$.tag <- unlist(lapply(sets, function(x) x$.__enclos_env__$private$.tag), recursive = FALSE)

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
    # of all parameters with either the set value or NULL. in construction defaults are set as values.
    get_values = function(tag) {
      values <- private$.value
      vals <- vector("list", length(self$ids))
      names(vals) <- self$ids
      vals[match(names(values), self$ids, 0)] <- values
      if (!missing(tag)) {
        return(vals[grepl(tag, private$.tag)])
      } else {
        return(vals)
      }
    },

    # similar to paradox, subsets the ParameterSet. needs further work as trafos and deps ignored
    subset = function(ids) {
      checkmate::assert_subset(ids, self$ids)
      ids <- intersect(self$ids, ids)
      private$.support <- self$supports[names(self$supports) %in% ids]
      private$.value <- self$values[names(self$values) %in% ids]
      private$.tag <- self$tags[names(self$tags) %in% ids]

      invisible(self)
    },

    # adds dependencies to ParameterSet. instead of creating a new Condition object, just allows a
    # small number of possible conditions and each has identical RHS formulation.
    # assert_no_cycles prevents a cycle of dependencies, see helpers.R
    # assert_condition ensures that the condition is either possible (if 'Equal' or 'AnyOf') or
    # just not redundant (if 'NotEqual' or 'NotAnyOf'), see helpers.R
    add_dep = function(id, on, type = c("Equal", "NotEqual", "AnyOf", "NotAnyOf"), cond) {
      checkmate::assert_choice(id, self$ids)
      checkmate::assert_choice(on, self$ids)
      if (id == on) {
        stop("A param cannot depend on itself!")
      }
      type <- match.arg(type)

      # hacky fix
      aid <- id
      aon <- on
      if (nrow(subset(private$.deps, id == aid & on == aon)) > 0) {
        stop(sprintf("%s already depends on %s.", id, on))
      }

      assert_condition(on, self$supports[on][[1]], type, cond)

      newDT <- rbind(private$.deps, data.table(id = id, on = on, type = type, cond = cond))
      assert_no_cycles(newDT)
      private$.deps <- newDT

      invisible(self)
    },

    # adds trafo either to given parameter ids or to the whole ParameterSet "<Set>".
    # currently no feasibility checks, e.g. if LogicalSet has trafo 'exp'
    add_trafo = function(id, fun) {
      if (checkmate::test_names(id, identical.to = "<Set>")) {
        checkmate::assert_function(fun, args = c("x", "param_set"), null.ok = TRUE)
        private$.trafo <- rbind(private$.trafo, data.table(id = "<Set>", fun = fun))
      } else {
        nin <- !(id %in% c(self$ids))
        if (any(nin)) {
          stop(sprintf(
            "Parameter(s) %s not available. Must be a valid id or <Set>.",
            string_as_set(id[nin])
          ))
        } else {
          checkmate::assert_function(fun)
          private$.trafo <- rbind(private$.trafo, data.table(id = id, fun = fun))
        }
      }
    }
  ),

  active = list(
    # change to public and add filters?
    params = function() {
      data.table::data.table(
        Id = self$ids, Support = private$.support,
        Value = self$get_values(), Tag = private$.tag
      )
    },

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
        mapply(function(x, y) if (!is.null(y)) assert_contains(x, y), self$supports[names(vals)], vals)
        private$.value <- vals
      }
    },

    # change to public and add filters?
    ids = function() {
      names(private$.support)
    },

    length = function() {
      nrow(self$params)
    },

    deps = function() {
      private$.deps
    },

    has_deps = function() {
      nrow(private$.deps) > 0L
    },

    trafos = function() {
      private$.trafo
    },

    has_trafos = function() {
      nrow(private$.trafo) > 0L
    }
  ),

  private = list(
    .support = list(),
    .value = list(),
    .tag = list(),
    .trafo = data.table(id = character(0L), fun = list()),
    .deps = data.table(id = character(0L), on = character(0L), type = character(0L), cond = list()),
    deep_clone = function(name, value) {
      switch(name,
        ".support" = sapply(value, function(x) x$clone(deep = TRUE)),
        ".deps" = copy(value),
        value
      )
    }
  )
)

#' @export
as.data.table.ParameterSet <- function(x, ...) {
  x$params
}

as.ParameterSet <- function(x, ...) {
  UseMethod("as.ParameterSet")
}
as.ParameterSet.data.table <- function(x, ...) {
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
  as.ParameterSet(rbindlist(lapply(ps, as.data.table)))
}
