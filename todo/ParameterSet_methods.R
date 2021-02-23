# This method is finished and can be added at anytime. However I am
#  generally unsure if/when `add`/`remove` methods are required.
.ParameterSet__add <- function(self, private, prms) {
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
}

# This is incomplete and needs better support for deps, trafo, checks.
#  However as above I am unsure if these methods are ever needed.
.ParameterSet__remove <- function(self, private, ...) {
    if (!is.null(private$.trafo)) {
       warning("Rransformations are not included in extraction.")
    }

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
}