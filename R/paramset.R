ParamSet <- R6::R6Class("ParamSet",
  public = list(
    initialize = function(..., support = NULL, value = NULL, tag = NULL){

      if (!is.null(support)) {

        if(length(support) == 0) {
          stop("ParamSet must be constructed with at least one parameter.")
        }

        assertSetList(support)
        assertNames(names(support), type = "unique")
        private$.support = support

        if(!is.null(tag)){
          assert(length(tag) == length(support))
          assertList(tag)
          names(tag) = names(support)
          private$.tag = tag
        } else {
          tag = vector("list", length(support))
          names(tag) = names(support)
          private$.tag = tag
        }

        if (!is.null(value)) {
          assert(length(value) == length(support))
          assertList(value)
          names(value) = names(support)
          mapply(function(x, y) if(!is.null(y)) assertContains(x, y), support, value)
          private$.value = value
        }

      } else {

        if(...length() == 0) {
          stop("ParamSet must be constructed with at least one parameter.")
        }

        params = sapply(list(...), makeParam)

        private$.support = params[1,]
        names(private$.support) = colnames(params)
        private$.value = params[2,]
        names(private$.value) = colnames(params)
        private$.tag = params[3,]
        names(private$.tag) = colnames(params)
      }

      private$.value = private$.value[!sapply(private$.value, is.null)]
      invisible(self)
    },

    print = function(){
      dt = self$params
      dt$Support = sapply(dt$Support, function(x) x$strprint())
      ftag = sapply(dt$Tag, function(x) if(!is.null(x)) paste0("{", paste0(x[[1]], collapse = ", "), "}"))
      if(length(ftag) != 1 | !is.null(ftag[[1]])){
        dt$Tag = ftag
      }

      print(dt)
    },

    add = function(...){
      psnew = ParamSet$new(...)
      sets = list(self, psnew)
      ids = unlist(sapply(sets, function(x) x$ids))
      d = duplicated(ids)
      if(any(d)){
        stop(sprintf("ids must be unique, duplicated ids: %s", paste0("{",paste0(ids[d], collapse = ", "),"}")))
      }

      # messy needs fixing
      self$.__enclos_env__$private$.support = unlist(lapply(sets, function(x) x$.__enclos_env__$private$.support), recursive = FALSE)
      self$.__enclos_env__$private$.value = unlist(lapply(sets, function(x) x$.__enclos_env__$private$.value), recursive = FALSE)
      self$.__enclos_env__$private$.tag = unlist(lapply(sets, function(x) x$.__enclos_env__$private$.tag), recursive = FALSE)

      invisible(self)
    },

    remove = function(...){
      params = unlist(list(...))
      private$.support[params] <- NULL
      private$.value[params] <- NULL
      private$.tag[params] <- NULL

      invisible(self)
    },

    get_values = function(tag){
      values = private$.value
      vals = vector("list", length(self$ids))
      names(vals) = self$ids
      vals[match(names(values), self$ids, 0)] = values
      if (!missing(tag)) {
        vals[grepl(tag, private$.tag)]
      }

      vals
    },

    subset = function(ids){
      assert_subset(ids, self$ids)
      ids = intersect(self$ids, ids)
      private$.support = self$supports[names(self$supports) %in% ids]
      private$.value = self$values[names(self$values) %in% ids]
      private$.tag = self$tags[names(self$tags) %in% ids]

      invisible(self)
    },

    add_dep = function(id, on, type = c("Equal", "NotEqual", "AnyOf", "NotAnyOf"), cond){
      assert_choice(id, self$ids)
      assert_choice(on, self$ids)
      if (id == on) {
        stopf("A param cannot depend on itself!")
      }
      type = match.arg(type)

      assert_condition(on, self$supports[on][[1]], type, cond)

      newDT = rbind(private$.deps, data.table(id = id, on = on, type = type, cond = cond))
      assert_no_cycles(newDT)
      private$.deps = newDT

      invisible(self)
    }
  ),

  active = list(
    # change to public and add filters?
    params = function(){
      data.table::data.table(Id = self$ids, Support = private$.support,
                             Value = self$get_values(), Tag = private$.tag)
    },

    supports = function(){
      private$.support
    },

    tags = function(){
      private$.tag
    },

    values = function(vals){
      if (missing(vals)) {
        return(private$.value)
      } else {
        vals = vals[names(vals) %in% self$ids]
        mapply(function(x,y) if(!is.null(y)) assertContains(x,y), self$supports[names(vals)], vals)
        private$.value = vals
      }
    },

    # change to public and add filters?
    ids = function(){
      names(private$.support)
    },

    length = function(){
      nrow(self$params)
    },

    deps = function(){
      private$.deps
    },

    has_deps = function() {
      nrow(private$.deps) > 0L
    }
  ),

  private = list(
    .support = list(),
    .value = list(),
    .tag = list(),
    .deps = data.table(id = character(0L), on = character(0L), type = character(0L), cond = list())
  )
  )

#' @export
as.data.table.ParamSet <- function(x, ...){
  x$params
}

as.ParamSet <- function(x,...){
  UseMethod("as.ParamSet")
}
as.ParamSet.data.table <- function(x, ...){
  checkmate::assertSubset(colnames(x), c("Id", "Support", "Value","Tag"))
  assertSetList(x$Support)
  assertNames(x$Id, type = "strict")
  support = x$Support

  value = x$Value
  if(!is.null(value)) names(value) = x$Id
  tag = x$Tag
  if(!is.null(tag)) names(tag) = x$Id

  names(support) = x$Id
  ParamSet$new(support = support, value = value, tag = tag)
}

# less efficient than $add, needs work
#' @export
rbind.ParamSet <- function(...){
  ps = list(...)
  as.ParamSet(rbindlist(lapply(ps, as.data.table)))
}
