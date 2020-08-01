ParameterSetCollection <- R6::R6Class("ParameterSetCollection",
  inherit = ParameterSet,
  public = list(
    initialize = function(sets) {
      checkmate::assert_list(sets, types = "ParameterSet")
      checkmate::assert_names(names(sets), type = "unique")
      private$.sets <- sets
    },

    add = function(sets) {
      checkmate::assert_list(sets, types = "ParameterSet")
      sets <- c(private$.sets, sets)
      checkmate::assert_names(names(sets), type = "unique")
      private$.sets <- sets
      invisible(self)
    },

    remove = function(sets) {
      checkmate::assert_character(sets)
      private$.sets <- private$.sets[!(names(private$.sets) %in% sets)]
      invisible(self)
    }
  ),

  active = list(
    params = function() {
      dt <- rbindlist(lapply(private$.sets, function(x) x$params))
      nr <- lapply(private$.sets, function(x) nrow(x$params))
      dt$Id <- paste(rep(names(private$.sets), nr), dt$Id, sep = "_")
      dt
    },

    ids = function() {
      names(self$supports)
    },

    supports = function() {
      s <- unlist(lapply(private$.sets, function(x) x$supports), recursive = FALSE)
      names(s) <- sub(".", "_", names(s), fixed = T)
      return(s)
    },

    tags = function() {
      s <- unlist(lapply(private$.sets, function(x) x$tags), recursive = FALSE)
      names(s) <- sub(".", "_", names(s), fixed = T)
      return(s)
    },

    values = function(vals) {
      if (missing(vals)) {
        v <- unlist(lapply(private$.sets, function(x) x$values), recursive = FALSE)
        names(v) <- sub(".", "_", names(v), fixed = T)
        return(v)
      } else {
        sets <- vapply(strsplit(names(vals), "_"), "[[", character(1), 1)
        names(vals) <- sapply(strsplit(names(vals), "_"), function(x) paste0(x[-1], collapse = "_"))

        sapply(unique(sets), function(aset) {
          private$.sets[[aset]]$values <- vals[sets == aset]
        })
      }
    },

    sets = function() {
      private$.sets
    }
  ),

  private = list(
    .sets = list()
  )
)

#' @export
as.data.table.ParameterSetCollection <- function(x, ...) { # nolint
  rbindlist(lapply(x$sets, as.data.table))
}
