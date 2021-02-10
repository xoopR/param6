param_formula_to_list <- function(params) {

  id <- checkmate::assertNames(names(params), "unique")

  params <- sapply(params, function(param) {
    tags <- set <- value <- NULL

      if (class(param)[1] != "formula") {
        set <- param
      } else {
        set <- eval(param[[2]])
        param <- param[[3]]
        if (class(param)[1] != "call") {
          value <- param
        } else {
          ftags <- grepl("^tags\\(.*\\)$", param)
          if (any(ftags)) {
            tags <- as.character(param[ftags][[1]])[-1]
            value <- param[!ftags][-1][[1]]
          } else {
            tags <- as.character(param[-1])
          }
        }
      }

      list(set = set, value = value, tags = tags)
  })

  if (is.null(unlist(params[2, ]))) {
    value <- NULL
  } else {
    value <- params[2, ]
    if (length(value) == 1) {
      names(value) <- id
    }
  }
  if (is.null(unlist(params[3, ]))) {
    tags = NULL
  } else {
    tags = params[3, ]
    if (length(tags) == 1) {
      names(tags) <- id
    }
  }

  support <- params[1,]
  if (length(support) == 1) {
      names(support) <- id
  }

  list(
      id = id,
      support = support,
      value = value,
      tags = tags
  )
}

get_params <- function(..., id, support, value, tags) {
  if (...length()) {
    params <- param_formula_to_list(list(...))
  } else {
    lng <- length(id)
    params <- list(
      id = checkmate::assertNames(unlist(id), "unique"),
      support = checkmate::assertList(support, len = lng),
      value = checkmate::assertList(value, len = lng, null.ok = TRUE),
      tags = checkmate::assertList(tags, len = lng, null.ok = TRUE)
    )
  }
}

set_supports <- function(params, private) {
    orig_uni <- set6::useUnicode()
    set6::useUnicode(FALSE)
    for (i in seq_along(params$support)) {
        .x <- params$support[[i]]
        # if Set then check if already exists, if so return character representation, otherwise
        #  add to temporary
        if (testSet(.x)) {
            char = as.character(.x)
            if (private$.tmpreg$has(char)) {
              private$.supports$t <- c(private$.supports$t,
                named_list(as.character(.x), names(params$support)[[i]]))
            } else if (support_dictionary$has(char)) {
              private$.supports$p <- c(private$.supports$p,
                named_list(as.character(.x), names(params$support)[[i]]))
            } else {
              private$.tmpreg$add(named_list(.x, as.character(.x)))
              private$.supports$t <- c(private$.supports$t,
                named_list(as.character(.x), names(params$support)[[i]]))
            }
        } else if (checkmate::test_character(.x, len = 1)) {
          if (private$.tmpreg$has(char)) {
            private$.supports$t <- c(private$.supports$t,
              named_list(.x, names(params$support)[[i]]))
          } else if (support_dictionary$has(char)) {
            private$.supports$p <- c(private$.supports$p,
              named_list(.x, names(params$support)[[i]]))
          } else {
            stop(sprintf("Support given as character (%s) but not found in temporary registry or support_dictionary.", .x)) # nolint
          }
        } else {
          stop(sprintf("Support %s should either inherit from Set or character."))
        }
      }
      # invert the list so names = supports and els = param ids w. supports
      private$.supports$p <- invert_list(private$.supports$p)
      private$.supports$t <- invert_list(private$.supports$t)
      set6::useUnicode(orig_uni)
      invisible(NULL)
}