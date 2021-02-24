#' @title Parameter Set
#' @description `ParameterSet` objects store parameters ([prm] objects) and add internal validation
#' checks and methods for:
#'
#' * Getting and setting parameter values
#' * Transforming parameter values
#' * Providing dependencies of parameters on each other
#' * Tagging parameters, which may enable further properties
#' * Storing subsets of parameters under prefixes
#'
#' @examples
#' ## $value examples
#' p <- ParameterSet$new(list(prm(id = "a", support = Reals$new())))
#' p$values$a <- 2
#' p$values
#'
#' ## $trafo examples
#' p <- ParameterSet$new(list(prm(id = "a", 2, support = Reals$new())))
#' p$trafo
#'
#' # simple transformation
#' p$get_values()
#' p$trafo <- function(x, self) {
#'  x$a <- exp(x$a)
#'  x
#' }
#' p$get_values()
#'
#' # more complex transformation on tags
#' p <- ParameterSet$new(
#'   list(prm(id = "a", 2, support = Reals$new(), tags = "t1"),
#'        prm(id = "b", 3, support = Reals$new(), tags = "t1")),
#'        prm(id = "c", 4, support = Reals$new())))
#' p$trafo <- function(x, self) {
#'  out <- 2^self$get_values(tags = "t1")
#'  out <- c(out, list(c = x$c))
#'  out
#' }
#' p$get_values()
#'
#' @template param_prms
#' @template param_sort
#' @export
ParameterSet <- R6::R6Class("ParameterSet",
  public = list(
    #' @description Constructs a `ParameterSet` object.
    #' @examples
    #' prms <- list(
    #'  prm("a", Set$new(1), 1, tags = "t1"),
    #'  prm("b", "reals", 1.5, tags = "t1"),
    #'  prm("d", "reals", 2, tags = "t2")
    #' )
    #' ParameterSet$new(prms)
    initialize = function(prms = list(), tag_properties = NULL) {
      .ParameterSet__initialize(self, private, prms, tag_properties)
    },

    #' @description Prints the `ParameterSet` after coercion with [as.data.table.ParameterSet].
    #' @examples
    #' prms <- list(
    #'  prm("a", Set$new(1), 1, tags = "t1"),
    #'  prm("b", "reals", 1.5, tags = "t1"),
    #'  prm("d", "reals", 2, tags = "t2")
    #' )
    #' p <- ParameterSet$new(prms)
    #' p$print()
    #' print(p)
    #' p
    print = function(sort = TRUE) {
      .ParameterSet__print(self, private, sort)
    },

    # FIXME - DOCUMENT
    get_values = function(id = NULL, tags = NULL, transform = TRUE, inc_null = TRUE,
                          simplify = TRUE) {
      .ParameterSet__get_values(self, private, id, tags, transform, inc_null, simplify)
    },

    # FIXME - DOCUMENT
    add_dep = function(id, on, cnd) {
      .ParameterSet__add_dep(self, private, id, on, cnd)
    },

    # FIXME - DOCUMENT
    transform = function() {
      .ParameterSet__transform(self, private)
    },

    # FIXME - ADD TESTS & DOCUMENT
    add_check = function(fun, ids = NULL, tags = NULL) {
      .ParameterSet__add_check(self, private, fun, ids, tags)
    },

    # FIXME - DOCUMENT
    check = function(supports = TRUE, custom = TRUE, deps = TRUE, id = NULL,
                     error_on_fail = TRUE) {
      .ParameterSet__check(self, private, supports, custom, deps, id, error_on_fail)
    },

    # FIXME - DOCUMENT
    rep = function(times, prefix) {
      .ParameterSet__rep(self, private, times, prefix)
    },

    # FIXME - DOCUMENT
    extract = function(id = NULL, tags = NULL, prefix = NULL) {
      .ParameterSet__extract(self, private, id, tags, prefix)
    }
  ),

  active = list(
    #' @field supports None -> `named_list()` \cr
    #' Get supports from the parameter set.
    supports = function() {
      sups <- support_dictionary$get_list(private$.supports)
      names(sups) <- self$ids
      sups
    },

    #' @field tags None -> `named_list()` \cr
    #' Get tags from the parameter set.
    tags = function() {
      private$.tags
    },

    #' @field tags None -> `named_list()` \cr
    #' Get tag properties and ids with the tag.
    tag_properties = function() {
      private$.tag_properties
    },

    #' @field ids None -> `character()` \cr
    #' Get ids from the parameter set.
    ids = function() {
      private$.id
    },

    #' @field length None -> `integer(1)` \cr
    #' Get the length of the parameter set as the number of parameters.
    length = function() {
      length(self$ids)
    },

    # @field values `list() -> self` / None -> `list()` \cr
    #' If `vals` is missing then returns the set (non-NULL) values without transformation or
    #' filtering; use `$get_values` for a more sophisticated getter of values. \cr
    #' If `vals` is not missing then used to set values of parameters, which are first checked
    #' internally with the `$check` method before setting the new values. \cr
    #' See examples at end.
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

    #' @field deps None -> [data.table::data.table]
    #' Get parameter dependencies, NULL if none.
    deps = function() {
      private$.deps
    },

    #' @field checks None -> [data.table::data.table]
    #' Get custom parameter checks, NULL if none.
    checks = function() {
      private$.checks
    },

    #' @field trafo `function() -> self` / None -> `function()` \cr
    #' If `x` is missing then returns a transformation function if previously set,
    #' otherwise `NULL`. \cr
    #' If `x` is not missing then it should be a function with arguments `x` and `self`, which
    #' internally correspond to `self` being the `ParameterSet` the transformation is being added
    #' to, and `x <- self$values`. The transformation function is automatically called after a call
    #' to `self$get_values()` and is used to transform set values, it should therefore result in a
    #' list. \cr
    #' See examples at end.
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
    .tag_properties = list(),
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

#' @title Convenience Function for Constructing a ParameterSet
#' @description See [ParameterSet] for full details.
#' @param prms (`list()`) \cr List of [prm] objects.
#' @examples
#' prms <- list(
#'  prm("a", Set$new(1), 1, tags = "t1"),
#'  prm("b", "reals", 1.5, tags = "t1"),
#'  prm("d", "reals", 2, tags = "t2")
#' )
#' p <- pset(prms)
#' @export
pset <- function(prms) {
  ParameterSet$new(prms)
}
