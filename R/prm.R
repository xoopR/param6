prm <- function(id, support, value = NULL, tags = NULL) {
  checkmate::assert_character(id, len = 1)
  if (id == "c") {
    stop("'c' is a reserved id in param6.")
  }
  # if character check to see if exists in dictionary otherwise error
  if (checkmate::test_character(support, len = 1)) {
    if (!support_dictionary$has(support)) {
      stop("'suppport' given as character but does not exist in support_dictionary.")
    }
    str_support <- support
    support <- support_dictionary$get(str_support)
  # if Set check to see if exists in dictionary otherwise add and return string
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
    checkmate::assert_character(tags, null.ok = TRUE)
    if ("c" %in% tags) {
      stop("'c' is a reserved tag in param6.")
    }
  }

  if (!is.null(value)) {
    assertContains(support, value)
  }

  param <- list(id = id, support = str_support, value = value, tags = tags)
  class(param) <- "prm6"
  param
}