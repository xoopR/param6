.check_custom <- function(self, values, checks, id, error_on_fail) {
  if (!is.null(checks) && nrow(checks)) {
    if (!is.null(id)) {
      ids <- NULL
      checks <- subset(checks, grepl(paste0(id, collapse = "|"), ids))
    }

    # `for` instead of `vapply` allows early breaking
    for (i in seq_along(checks$fun)) {
      .y <- checks$fun[[i]]
      ivalues <- .get_values(self, get_private(self), values, checks$ids[[i]],
                             checks$tags[[i]])
      ok <- as.function(list(x = ivalues, self = self, .y))()
      if (!ok) {
        return(.return_fail(sprintf("Check on '%s' failed.", deparse(.y)),
                            error_on_fail))
      }
    }
  }

  return(TRUE)
}