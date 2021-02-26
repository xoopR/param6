#' @import set6
#' @import R6
#' @importFrom data.table as.data.table data.table
"_PACKAGE"

# nocov start
utils::globalVariables(c("support_dictionary", "on"))


.onLoad = function(libname, pkgname) { # nolint
  assign("support_dictionary", load_support(), envir = topenv())
}
# nocov end
