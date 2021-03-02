# This method is finished and can be added at anytime. However I am
#  generally unsure if/when `add`/`remove` methods are required.
.ParameterSet__add <- function(self, private, prms) { # nolint
    if (length(prms)) {
        checkmate::assert_list(prms, "prm6", any.missing = FALSE)

        ids <- vapply(prms, "[[", character(1), "id")
        names(prms) <- ids
        if (any(duplicated(c(ids, private$.id)))) {
          stop("ids are not unique or already existed in ParameterSet.")
        } else {
          env_append(private, ".id", ids)
          env_append(private, ".supports",
                     vapply(prms, "[[", character(1), "support"))
          env_append(private, ".value",
                     un_null_list(lapply(prms, "[[", "value")))
          env_append(private, ".tags",
                     un_null_list(lapply(prms, "[[", "tags")))
          private$.isupports <- invert_names(private$.supports)
        }
      } else {
        stop("At least one parameter must be added.")
      }

      invisible(self)
}

# This is incomplete and needs better support for deps, trafo, checks.
#  However as above I am unsure if these methods are ever needed.
.ParameterSet__remove <- function(self, private, ...) { # nolint
    if (!is.null(private$.trafo)) {
       warning("Rransformations are not included in extraction.")
    }

    pars <- unlist(list(...))
    private$.support[pars] <- NULL
    private$.value[pars] <- NULL
    private$.tags[pars] <- NULL
    # fix for vectorised pars
    private$.deps <- subset(private$.deps,
                            !(grepl(id, pars) | grepl(on, pars)))
    private$.trafo[pars] <- NULL
    # FIXME - remove checks
    subset(private$.checks, grepl(pars, params))

    invisible(self)
}

.ParameterSet__add_check <- function(self, private, fun, ids, tags) { # nolint
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

  checkmate::assert_function(fun, "x", TRUE)
  if (!checkmate::test_logical(fun(self$values, self), len = 1)) {
    stop("'fun' should evaluate to a scalar logical.")
  }

  new_checks <- rbind(checks,
                      data.table::data.table(ids = list(ids),
                                             tags = list(tags),
                                             fun = list(body(fun))))

  .check_custom(self, self$values, new_checks, NULL, TRUE)

  private$.checks <- new_checks

  invisible(self)
}