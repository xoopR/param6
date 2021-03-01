#' @title Coercions to ParameterSet
#' @param x (`ANY`) \cr Object to coerce.
#' @param ... (`ANY`) \cr Other arguments passed to [ParameterSet], such as
#' `tag_properties`.
#' @export
as.ParameterSet <- function(x, ...) { # nolint
  UseMethod("as.ParameterSet")
}

#' @rdname as.ParameterSet
#' @export
as.ParameterSet.data.table <- function(x, ...) { # nolint
  ParameterSet$new(as.prm(x), ...)
}

#' @rdname as.ParameterSet
#' @export
as.ParameterSet.prm <- function(x, ...) { # nolint
  ParameterSet$new(list(x), ...)
}

#' @rdname as.ParameterSet
#' @export
as.ParameterSet.list <- function(x, ...) { # nolint
  checkmate::assert_list(x, "prm", any.missing = FALSE)
  ParameterSet$new(x, ...)
}

#' @title Length of a ParameterSet
#' @description Gets the number of parameters in the [ParameterSet].
#' @param x ([ParameterSet])
#' @export
length.ParameterSet <- function(x) {
  x$length
}

#' @title Replicate a ParameterSet
#' @description In contrast to the `$rep` method in [ParameterSet], this method
#' deep clones the [ParameterSet] and returns a new object.
#' @details In order to avoid duplicated parameter ids, every id in the
#' [ParameterSet] is given a `prefix` in the format `prefix__id`. In
#' addition, linked tags are also given the same prefix to prevent
#' incorrectly linking parameters.
#'
#' The primary use-case of this method is to treat the [ParameterSet] as a
#' collection of identical [ParameterSet] objects.
#'
#' @param x ([ParameterSet])
#' @param times (`integer(1)`) \cr
#' Numer of times to replicate the `ParameterSet`.
#' @param prefix (`character(1)|character(length(times))`) \cr
#' The prefix to add to ids and linked tags. If length `1` then is
#' internally coerced to `paste0(prefix, seq(times))`, otherwise the length
#' should be equal to `times`.
#' @param ... (`ANY`) \cr Other arguments, currently unused.
#' @export
rep.ParameterSet <- function(x, times, prefix, ...) {
  x <- x$clone(deep = TRUE)
  x$rep(times, prefix)
  x
}

#' @title Concatenate ParameterSet Objects
#' @description Concatenate multiple [ParameterSet] objects into a single
#' [ParameterSet].
#' @details Concatenates ids, tags, tag properties and dependencies,
#' but not transformations.
#' @param ... ([ParameterSet]s) \cr [ParameterSet] objects to concatenate.
#' @param pss (`list()`) \cr Alternatively pass a list of [ParameterSet]
#' objects.
#' @export
c.ParameterSet <- function(..., pss = list(...)) {

  prms <- unlist(lapply(pss, as.prm), FALSE)

  if (any(!is.null(unlist(lapply(pss, "[[", "trafo"))))) {
    warning("Transformations are lost in concatenation.")
  }

  props <- unlist(lapply(pss, "[[", "tag_properties"), recursive = FALSE)
  if (length(props)) {
    tprop <- list()
    req_props <- props[names(props) %in% "required"]
    if (length(req_props)) {
      tprop$required <- unique(unlist(req_props))
    }
    lin_props <- props[names(props) %in% "linked"]
    if (length(lin_props)) {
      tprop$linked <- unique(unlist(lin_props))
    }
    un_props <- props[names(props) %in% "unique"]
    if (length(lin_props)) {
      tprop$unique <- unique(unlist(un_props))
    }
    if (any(duplicated(unlist(tprop)))) {
      stop("Cannot merge inconsistent tag properties.")
    }
  } else {
    tprop <- NULL
  }

  ps <- ParameterSet$new(prms, tprop)
  private <- get_private(ps)

  deps <- lapply(pss, "[[", "deps")
  if (any(!is.null(unlist(deps)))) {
    private$.deps <- data.table::rbindlist(deps)
  }

  ps
}

#' @title Coerce a ParameterSet to a data.table
#' @description Coercion from [ParameterSet] to [data.table::data.table].
#' Dependencies, transformations, and tag properties are all lost in
#' coercion.
#' @param x ([ParameterSet])
#' @param sort (`logical(1)`) \cr If `TRUE`(default) sorts the [ParameterSet]
#' alphabetically by id.
#' @param ... (`ANY`) \cr Other arguments, currently unused.
#' @export
as.data.table.ParameterSet <- function(x, sort = TRUE, ...) { # nolint
  if (length(x$deps) || length(x$trafo)) {
    warning("Dependencies and trafos are lost in coercion.")
  }

  vals <- expand_list(x$ids, x$values)
  tags <- expand_list(x$ids, x$tags)

  dt <- data.table::data.table(
    Id = x$ids,
    Support = x$supports,
    Value = vals[match(names(vals), x$ids)],
    Tags = tags[match(names(tags), x$ids)]
  )
  if (sort) {
    Id <- NULL # binding fix
    data.table::setorder(dt, Id)
  }
  dt
}

#' @title Extract a sub-ParameterSet by Parameters
#' @description Creates a new [ParameterSet] by extracting the given
#' parameters. S3 method for the `$extract` public method.
#' @param object ([ParameterSet])
#' @param i (`character()`) \cr
#' If not `NULL` then specifies the parameters by id to extract. Should be
#' `NULL` if `prefix` is not `NULL`.
#' @param tags (`character()`) \cr
#' If not `NULL` then specifies the parameters by tag to extract. Should be
#' `NULL` if `prefix` is not `NULL`.
#' @param prefix (`character()`) \cr
#' If not `NULL` then extracts parameters according to their prefix and
#' additionally removes the prefix from the id. A prefix is determined as
#' the string before `"__"` in an id.
#' @param ... (`ANY`) \cr Other arguments, currently unused.
#' @export
`[.ParameterSet` <- function(object, i = NULL, tags = NULL, prefix = NULL,
                             ...) {
  object$extract(i, tags, prefix)
}
