#' @title S3 Parameter Constructor
#' @description The `prm` class is required for [ParameterSet] objects, it
#' allows specifying a parameter as a named set and optionally setting values
#' and tags.
#' @param id (`character(1)`) \cr
#' Parameter identifier.
#' @param support `([set6::Set]|character(1))` \cr
#' Either a set object from
#' \CRANpkg{set6} or a character representing the set if it is already present
#' in the [support_dictionary]. If a [set6::Set] is provided then the set and
#' its string representation are added automatically to [support_dictionary]
#' in order to provide fast internal checks. Common sets (such as the reals,
#' naturals, etc.) are already provided in [support_dictionary].
#' @param value `ANY` \cr
#' Optional to assign the parameter, will internally
#' be checked that it lies within the given support.
#' @param tags (`character()`) \cr
#' An optional character vector of tags to apply to the parameter. On their own
#' tags offer little extra benefit, however they can be assigned properties
#' when creating [ParameterSet] objects that enable them to be more powerful.
#' @param .check For internal use only.
#' @examples
#' library(set6)
#'
#' # Constructing a prm with a Set support
#' prm(
#'  id = "a",
#'  support = Reals$new(),
#'  value = 1
#' )
#'
#' # Constructing a prm with a support already in the dictionary
#' prm(
#'  id = "a",
#'  support = "reals",
#'  value = 1
#' )
#'
#' # Adding tags
#' prm(
#'  id = "a",
#'  support = "reals",
#'  value = 1,
#'  tags = c("tag1", "tag2")
#' )
#' @export
prm <- function(id, support, value = NULL, tags = NULL, .check = TRUE) {

  checkmate::assert_character(id, len = 1)
  if (id == "c") {
    stop("'c' is a reserved id in param6.")
  }

  # if character, check to see if exists in dictionary otherwise error
  if (checkmate::test_character(support, len = 1)) {
    if (!support_dictionary$has(support)) {
      stop("'support' given as character but does not exist in support_dictionary.") # nolint
    }
    str_support <- support
    support <- support_dictionary$get(str_support)
  # if Set, check to see if exists in dictionary otherwise add and return string
  } else if (checkmate::test_class(support, "Set")) {
    str_support <- as.character(support)
    if (!support_dictionary$has(str_support)) {
      orig_uni <- set6::useUnicode()
      set6::useUnicode(FALSE)
      support_dictionary$add(keys = str_support, values = support)
      set6::useUnicode(orig_uni)
    }
  } else {
    stop("'support' should be given as a character scalar or Set.")
  }

  if (!is.null(tags)) {
    tags <- unique(checkmate::assert_character(tags, null.ok = TRUE))
    if ("c" %in% tags) {
      stop("'c' is a reserved tag in param6.")
    }
  }

  if (!is.null(value) && .check) {
    if (length(value) > 1) {
      assert_contains(support, as.Tuple(value))
    } else {
      assert_contains(support, value)
    }
  }

  param <- list(id = id, support = str_support, value = value, tags = tags)
  class(param) <- "prm"
  param
}

#' @title Coercion Methods to prm
#' @description Methods for coercing various objects to a [prm].
#' @param x (`ANY`) \cr Object to coerce.
#' @param ... (`ANY`) \cr Other arguments, currently unused.
#' @export
as.prm <- function(x, ...) { # nolint
  UseMethod("as.prm")
}

#' @rdname as.prm
#' @export
as.prm.ParameterSet <- function(x, ...) {
  unname(Map(prm,
    id = x$ids,
    support = get_private(x)$.supports,
    value = expand_list(x$ids, x$values),
    tags = expand_list(x$ids, x$tags),
    .check = FALSE
  ))
}

#' @rdname as.prm
#' @export
as.prm.data.table <- function(x, ...) { # nolint
  checkmate::assertSubset(colnames(x), c("Id", "Support", "Value", "Tags"))
  unname(Map(prm,
    id = x$Id,
    support = x$Support,
    value = x$Value,
    tags = x$Tags
  ))
}
