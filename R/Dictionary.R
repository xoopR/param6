#' @title R6 Dictionary
#' @description The dictionary is solely used as a mutable interface for the
#' [support_dictionary]. It is exported and documented here for ease of use
#' but is not extensively tested as it is minimally user-facing.
#' @export
Dictionary <- R6Class("Dictionary",
  private = list(
    .items = list(),
    .types = NULL,
    deep_clone = function(name, value) {
      switch(name,
             ".items" = sapply(value, function(.x) {
               if (inherits(.x, "R6")) {
                 .x$clone(deep = TRUE)
               } else {
                 .x
               }
             }),
             value
      )
    }
  ),

  public = list(
    #' @description Constructs a `Dictionary` object.
    #' @param x (`list()`) \cr
    #' A named list with the names corresponding to the items to add to the
    #' dictionary, where the keys are the list names and the values are the
    #' list elements. Names must be unique.
    #' @param types (`character()`) \cr
    #' If non-NULL then `types` creates a typed dictionary in which all
    #' elements of the dictionary must inherit from these `types`. Any class
    #' can be given to `types` as long as there is a valid `as.character`
    #' method associated with the class.
    initialize = function(x = list(), types = NULL) {
      .Dictionary__initialize(self, private, x, types)
    },

    #' @description Add new items to the dictionary.
    #' @param x (`list()`) \cr
    #' Same as initialize, items to add to the list.
    #' @param keys (`character()`) \cr
    #' If `x` is NULL then `keys` and `values` can be provided to add the
    #' new items by a character vector of keys and list of values instead.
    #' @param values (`list()`) \cr
    #' If `x` is NULL then `keys` and `values` can be provided to add the
    #' new items by a list of keys and values instead.
    add = function(x = list(), keys = NULL, values = NULL) {
      .Dictionary__add(self, private, x, keys, values)
    },

    #' @description Change the name of a given key.
    #' @param key (`character(1)`) \cr
    #' Key to rename.
    #' @param new_key (`character(1)`) \cr
    #' New name of key, must not already exist in dictionary.
    rekey = function(key, new_key) {
      .Dictionary__rekey(self, private, key, new_key)
    },

    #' @description Removes the given item from the list.
    #' @param key (`character(1)`) \cr
    #' Key of item to remove.
    remove = function(key) .Dictionary__remove(self, private, key),

    #' @description Gets the given items from the dictionary. If only one
    #' item is requested then returns the (unlisted) item, or if multiple items
    #' are requested as the dictionary is typed, then the unlisted items are
    #' returned.
    #' @param keys (`character()`) \cr
    #' Keys of items to get.
    get = function(keys) .Dictionary__get(self, private, keys),

    #' @description Gets the given items from the dictionary as list.
    #' @param keys (`character()`) \cr
    #' Keys of items to get.
    get_list = function(keys) .Dictionary__get_list(self, private, keys),

    #' @description Checks if the given key is in the list, returns a logical.
    #' @param key (`character(1)`) \cr
    #' Key to check.
    has = function(key) .Dictionary__has(self, private, key),

    #' @description Checks if the given value is in the list, returns a
    #' logical.
    #' @param value (`ANY`) \cr
    #' Value to check.
    has_value = function(value) .Dictionary__has_value(self, private, value),

    #' @description Prints dictionary.
    #' @param n (`integer(1)`) \cr
    #' Number of items to print on either side of ellipsis.
    print = function(n = 2) .Dictionary__print(self, private, n),

    #' @description Summarises dictionary.
    #' @param n (`integer(1)`) \cr
    #' Number of items to print on either side of ellipsis.
    summary = function(n = 2) .Dictionary__summary(self, private, n),

    #' @description Merges another dictionary, or list of dictionaries, into
    #' self.
    #' @param x (`Dictionary(1) | list()`) \cr
    #' Dictionary or list of dictionaries to merge in, must have unique keys.
    merge = function(x) .Dictionary__merge(self, private, x)
  ),

  active = list(
    #' @field keys None -> `character()` \cr
    #' Get dictionary keys.
    keys = function() names(private$.items),

    #' @field values None -> `list()` \cr
    #' Get dictionary values.
    values = function() .Dictionary__values(self, private),

    #' @field items `list() -> self` / None -> `list()` \cr
    #' If `x` is missing then returns the dictionary items. \cr
    #' If `x` is not missing then used to set items in the dictionary.
    items = function(x) .Dictionary__items(self, private, x),

    #' @field length None -> `integer(1)` \cr
    #' Get dictionary length as number of items.
    length = function() length(private$.items),

    #' @field typed None -> `logical(1)` \cr
    #' Get if the dictionary is typed (`TRUE`) or not (`FALSE`).
    typed = function() !is.null(private$.types),

    #' @field types None -> `character()` \cr
    #' Get the dictionary types (NULL if un-typed).
    types = function() private$.types
  )
)
