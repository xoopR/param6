#' @title Parameter Set
#' @description `ParameterSet` objects store parameters ([prm] objects) and add
#' internal validation checks and methods for:
#'
#' * Getting and setting parameter values
#' * Transforming parameter values
#' * Providing dependencies of parameters on each other
#' * Tagging parameters, which may enable further properties
#' * Storing subsets of parameters under prefixes
#'
#' @examples
#' library(set6)
#'
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
#'        prm(id = "b", 3, support = Reals$new(), tags = "t1"),
#'        prm(id = "d", 4, support = Reals$new()))
#' )
#' # make sure `transform = FALSE` to prevent infinite recursion
#' p$trafo <- function(x, self) {
#'  out <- lapply(self$get_values(tags = "t1", transform = FALSE),
#'                function(.x) 2^.x)
#'  out <- c(out, list(d = x$d))
#'  out
#' }
#' p$get_values()
#'
#' @template param_prms
#' @template param_tag_properties
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

    #' @description Prints the `ParameterSet` after coercion with
    #' [as.data.table.ParameterSet].
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
    print = function(sort = TRUE) .ParameterSet__print(self, private, sort),

    #' @description Gets values from the `ParameterSet` with options to filter
    #' by specific IDs and tags, and also to transform the values.
    #' @param id (`character()`) \cr
    #' If not NULL then returns values for given `ids`.
    #' @param tags (`character()`) \cr
    #' If not NULL then returns values for given `tags`.
    #' @param transform (`logical(1)`) \cr
    #' If `TRUE` (default) and `$trafo` is not `NULL` then runs the set
    #' transformation function before returning the values.
    #' @param inc_null (`logical(1)`) \cr
    #' If `TRUE` (default) then returns values for all ids even if `NULL`.
    #' @param simplify (`logical(1)`) \cr
    #' If `TRUE` (default) then unlists scalar values, otherwise always
    #' returns a list.
    #' @examples
    #' prms <- list(
    #'  prm("a", "reals", 1, tags = "t1"),
    #'  prm("b", "reals", 1.5, tags = "t1"),
    #'  prm("d", "reals", tags = "t2")
    #' )
    #' p <- ParameterSet$new(prms)
    #' p$trafo <- function(x, self) {
    #'  x$a <- exp(x$a)
    #'  x
    #' }
    #' p$get_values()
    #' p$get_values(inc_null = FALSE)
    #' p$get_values(id = "a")
    #' p$get_values(tags = "t1")
    get_values = function(id = NULL, tags = NULL, transform = TRUE,
                          inc_null = TRUE, simplify = TRUE) {
      .ParameterSet__get_values(self, private, id, tags, transform, inc_null,
                                simplify)
    },

    #' @description Gets values from the `ParameterSet` with options to filter
    #' by specific IDs and tags, and also to transform the values.
    #' @param id (`character(1)`) \cr
    #' The dependent variable for the condition that depends on the given
    #' variable, `on`, being a particular value. Should be in `self$ids`.
    #' @param on (`character(1)`) \cr
    #' The independent variable for the condition that is depended on by the
    #' given variable, `id`. Should be in `self$ids`.
    #' @param cnd (`cnd(1)`) \cr
    #' The condition defined by [cnd] which determines how `id` depends on `on`.
    #' @examples
    #' # not run as errors
    #' \dontrun{
    #' # Dependency on specific value
    #' prms <- list(
    #'  prm("a", "reals", NULL),
    #'  prm("b", "reals", 1)
    #' )
    #' p <- ParameterSet$new(prms)
    #' p$add_dep("a", "b", cnd("eq", 2))
    #' # 'a' can only be set if 'b' equals 2
    #' p$values$a <- 1
    #' p$values <- list(a = 1, b = 2)
    #'
    #' # Dependency on variable value
    #' prms <- list(
    #'  prm("a", "reals", NULL),
    #'  prm("b", "reals", 1)
    #' )
    #' p <- ParameterSet$new(prms)
    #' p$add_dep("a", "b", cnd("eq", id = "b"))
    #' # 'a' can only be set if it equals 'b'
    #' p$values$a <- 2
    #' p$values <- list(a = 2, b = 2)
    #' }
    add_dep = function(id, on, cnd) {
      .ParameterSet__add_dep(self, private, id, on, cnd)
    },

    #' @description Replicate the `ParameterSet` with identical parameters.
    #' In order to avoid duplicated parameter ids, every id in the
    #' `ParameterSet` is given a `prefix` in the format `prefix__id`. In
    #' addition, linked tags are also given the same prefix to prevent
    #' incorrectly linking parameters.
    #'
    #' The primary use-case of this method is to treat the `ParameterSet` as a
    #' collection of identical `ParameterSet` objects.
    #'
    #' Note that this mutates the `ParameterSet`, if you want to instead create
    #' a new object then use [rep.ParameterSet] instead (or copy and deep clone)
    #' first.
    #' @param times (`integer(1)`) \cr
    #' Numer of times to replicate the `ParameterSet`.
    #' @param prefix (`character(1)|character(length(times))`) \cr
    #' The prefix to add to ids and linked tags. If length `1` then is
    #' internally coerced to `paste0(prefix, seq(times))`, otherwise the length
    #' should be equal to `times`.
    rep = function(times, prefix) {
      .ParameterSet__rep(self, private, times, prefix)
    },

    #' @description Creates a new `ParameterSet` by extracting the given
    #' parameters.
    #' @param id (`character()`) \cr
    #' If not `NULL` then specifies the parameters by id to extract. Should be
    #' `NULL` if `prefix` is not `NULL`.
    #' @param tags (`character()`) \cr
    #' If not `NULL` then specifies the parameters by tag to extract. Should be
    #' `NULL` if `prefix` is not `NULL`.
    #' @param prefix (`character()`) \cr
    #' If not `NULL` then extracts parameters according to their prefix and
    #' additionally removes the prefix from the id. A prefix is determined as
    #' the string before `"__"` in an id.
    #' @param keep_trafo (`logical(1)`) \cr
    #' If `TRUE` (default) then transformations are kept in extraction,
    #' otherwise removed with warning.
    #'
    #' @examples
    #' # extract by id
    #' prms <- list(
    #'  prm("a", "reals", NULL),
    #'  prm("b", "reals", 1)
    #' )
    #' p <- ParameterSet$new(prms)
    #' p$extract("a")
    #' # equivalently
    #' p["a"]
    #'
    #' # extract by prefix
    #' prms <- list(
    #'   prm("Pre1__par1", Set$new(1), 1, tags = "t1"),
    #'   prm("Pre1__par2", "reals", 3, tags = "t2"),
    #'   prm("Pre2__par1", Set$new(1), 1, tags = "t1"),
    #'   prm("Pre2__par2", "reals", 3, tags = "t2")
    #' )
    #' p <- ParameterSet$new(prms)
    #' p$extract(tags = "t1")
    #' p$extract(prefix = "Pre1")
    #' # equivalently
    #' p[prefix = "Pre1"]
    extract = function(id = NULL, tags = NULL, prefix = NULL,
                       keep_trafo = TRUE) {
      .ParameterSet__extract(self, private, id, tags, prefix, keep_trafo)
    },


    #' @description Removes the given parameters from the set.
    #' @param id (`character()`) \cr
    #' If not `NULL` then specifies the parameters by id to extract. Should be
    #' `NULL` if `prefix` is not `NULL`.
    #' @param prefix (`character()`) \cr
    #' If not `NULL` then extracts parameters according to their prefix and
    #' additionally removes the prefix from the id. A prefix is determined as
    #' the string before `"__"` in an id.
    remove = function(id = NULL, prefix = NULL) {
      .ParameterSet__remove(self, private, id, prefix)
    },

    #' @description Deprecated method added for distr6 compatibility.
    #' Use $values/$get_values() in the future.
    #' Will be removed in 0.3.0.
    #' @param id Parameter id
    #' @param ... Unused
    getParameterValue = function(id, ...) {
      # nocov start
      warning("Deprecated. In the future please use $values/$get_values(). Will be removed in 0.3.0.") # nolint
      self$get_values(id)
      # nocov end
    },

    #' @description Deprecated method added for distr6 compatibility.
    #' Use $set_values in the future.
    #' Will be removed in 0.3.0.
    #' @param ... Parameter ids
    #' @param lst List of parameter ids
    setParameterValue = function(..., lst = list(...)) {
      # nocov start
      warning("Deprecated. In the future please use $values. Will be removed in 0.3.0.") # nolint
      self$values <- unique_nlist(c(lst, self$values))
      # nocov end
    },

    #' @description Convenience function for setting multiple parameters
    #' without changing or accidentally removing others.
    #' @param ... Parameter ids
    #' @param lst List of parameter ids
    set_values = function(..., lst = list(...)) {
      self$values <- unique_nlist(c(lst, self$values))
      invisible(self)
    },

    #' @description Deprecated method added for distr6 compatibility.
    #' Use $print/as.data.table() in the future.
    #' Will be removed in 0.3.0.
    #' @param ... Unused
    parameters = function(...) {
      # nocov start
      warning("Deprecated. In the future please use $print/as.data.table(). Will be removed in 0.3.0.") # nolint
      self
      # nocov end
    },

    #' @description Applies the internal transformation function.
    #' If no function has been passed to `$trafo` then `x` is returned
    #' unchanged. If `$trafo` is a function then `x` is passed directly to
    #' this. If `$trafo` is a list then `x` is evaluated and passed down the
    #' list iteratively.
    #' @param x (`named list(1)`) \cr
    #' List of values to transform.
    #' @return `named list(1)`
    transform = function(x = self$values) {
      .ParameterSet__transform(self, private, x)
    }
  ),

  active = list(
    #' @field tags None -> `named_list()` \cr
    #' Get tags from the parameter set.
    tags = function() private$.tags,

    #' @field ids None -> `character()` \cr
    #' Get ids from the parameter set.
    ids = function() private$.id,

    #' @field length None -> `integer(1)` \cr
    #' Get the length of the parameter set as the number of parameters.
    length = function() length(self$ids),

    #' @field deps None -> [data.table::data.table]
    #' Get parameter dependencies, NULL if none.
    deps = function() private$.deps,

    #' @field supports None -> `named_list()` \cr
    #' Get supports from the parameter set.
    supports = function() .ParameterSet__supports(self, private),

    #' @field tag_properties `list() -> self` / None -> `list()` \cr
    #' If `x` is missing then returns tag properties if any. \cr
    #' If `x` is not missing then used to tag properties. Currently properties
    #' can either be: \cr
    #' i) 'required' - parameters with this tag must have set (non-NULL)
    #' values; if a parameter is both 'required' and 'linked' then exactly
    #' one parameter in the 'linked' tag must be tagged;\cr
    #' ii) 'linked' - parameters with 'linked' tags are dependent on one another
    #' and only one can be set (non-NULL at a time);\cr
    #' iii) 'unique' - parameters with this tag must have no duplicated
    #' elements, therefore this tag only makes sense for vector parameters;\cr
    #' iv) 'immutable' - parameters with this tag cannot be updated after
    #' construction.
    tag_properties = function(x) {
      .ParameterSet__tag_properties(self, private, x)
    },

    #' @field values `list() -> self` / None -> `list()` \cr
    #' If `x` is missing then returns the set (non-NULL) values without
    #' transformation or filtering; use `$get_values` for a more sophisticated
    #' getter of values. \cr
    #' If `x` is not missing then used to set values of parameters, which are
    #' first checked internally with the `$check` method before setting the new
    #' values. \cr
    #' See examples at end.
    values = function(x) .ParameterSet__values(self, private, x),

    #' @field trafo `function()|list() -> self` / None -> `function()|list()`
    #' \cr
    #' If `x` is missing then returns a transformation function if previously
    #' set, a list of transformation functions, otherwise `NULL`. \cr
    #' If `x` is not missing then it should either be:
    #'
    #' * a function with arguments `x` and `self`, which internally correspond
    #' to `self` being the `ParameterSet` the transformation is being added to,
    #' and `x <- self$values`.
    #' * a list of functions like above
    #'
    #' The transformation function is automatically called after a call to
    #' `self$get_values()` and is used to transform set values, it should
    #' therefore result in a list. If using `self$get_values()` within the
    #' transformation function, make sure to set `transform = FALSE` to prevent
    #' infinite recursion, see examples at end.
    #'
    #' It is generally safer to call the transformation with
    #' `$transform(self$values)` as this will first check to see if `$trafo`
    #' is a function or list. If the latter then each function in the list is
    #' applied, one after the other.
    trafo = function(x) .ParameterSet__trafo(self, private, x)
    ),

  private = list(
    .id = NULL,
    .isupports = NULL,
    .supports = NULL,
    .value = NULL,
    .tags = NULL,
    .tag_properties = NULL,
    .trafo = NULL,
    .deps = NULL,
    .immutable = NULL,
    .update_support = function(..., lst = list(...)) {
      .ParameterSet__.update_support(self, private, lst)
    },
    deep_clone = function(name, value) {
      switch(name,
             ".deps" = {
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
#' @param ... ([prm]) \cr [prm] objects.
#' @param prms (`list()`) \cr List of [prm] objects.
#' @template param_tag_properties
#' @param deps (`list()`) \cr List of lists where each element is passed to
#'  `$add_dep`. See examples.
#' @param trafo (`function()`) \cr Passed to `$trafo`. See examples.
#' @examples
#' library(set6)
#'
#' # simple example
#' prms <- list(
#'  prm("a", Set$new(1), 1, tags = "t1"),
#'  prm("b", "reals", 1.5, tags = "t1"),
#'  prm("d", "reals", 2, tags = "t2")
#' )
#' p <- pset(prms = prms)
#'
#' # with properties, deps, trafo
#' p <- pset(
#'  prm("a", Set$new(1), 1, tags = "t1"),
#'  prm("b", "reals", 1.5, tags = "t1"),
#'  prm("d", "reals", 2, tags = "t2"),
#'  tag_properties = list(required = "t2"),
#'  deps = list(
#'    list(id = "a", on = "b", cond = cnd("eq", 1.5))
#'  ),
#'  trafo = function(x, self) return(x)
#' )
#' @export
pset <- function(..., prms = list(...), tag_properties = NULL, deps = NULL,
                 trafo = NULL) {

  ps <- ParameterSet$new(prms, tag_properties)

  if (!is.null(deps)) {
    checkmate::assert_list(deps)
    lapply(deps, function(x) {
      cnd <- if (checkmate::test_list(x$cond)) x$cond[[1]] else x$cond
      ps$add_dep(x$id, x$on, cnd)
    })
  }

  ps$trafo <- trafo

  ps
}
