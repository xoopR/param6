ParamSet <- R6::R6Class("ParamSet",
  public = list(
    initialize = function(..., support, value){

      if (!missing(support)) {
        private$.support = support
        if (!missing(value)) {
          private$.value = value
        }
      } else {
        if(...length() == 0) {
          stop("Need Parameters")
        }

        params = list(...)

        par_tab = sapply(params, function(x){
          if(class(x)[1] != "formula") {
            assertSet(x)
            return(list(set = x, val = NA))
          } else {
            set = eval(x[[2]])
            assertContains(set, x[[3]])
            return(list(set = set, val = x[[3]]))
          }
        })

        private$.support = par_tab[1,]
        names(private$.support) = names(params)
        private$.value = par_tab[2,]
        names(private$.value) = names(params)
      }

      invisible(self)
    },

    print = function(){
      dt = self$params
      dt$Support = sapply(dt$Support, function(x) x$strprint())
      print(dt)
    },

    add = function(...){
      psnew = ParamSet$new(...)
      sets = list(self, psnew)
      ids = unlist(sapply(sets, function(x) x$ids))
      d = duplicated(ids)
      if(any(d)){
        stop("ids must be unique, duplicated ids: %s", paste0("{",paste0(ids[d], collapse = ", "),"}"))
      }

      # messy needs fixing
      self$.__enclos_env__$private$.support = unlist(sapply(sets, function(x) x$.__enclos_env__$private$.support), recursive = FALSE)
      self$.__enclos_env__$private$.value = unlist(sapply(sets, function(x) x$.__enclos_env__$private$.value), recursive = FALSE)
    },

    remove = function(params){
      ind <- names(private$.support) %in% params
      private$.support <- private$.support[!ind]
      private$.value <- private$.value[!ind]
    }
  ),

  active = list(
    params = function(){
      data.table::data.table(Id = names(private$.support), Support = private$.support,
                             Value = private$.value)
    },

    supports = function(){
      private$.support
    },

    values = function(vals){
      if(missing(vals)) {
        return(private$.value)
      } else {

        ind = match(self$params$Id, names(vals), nomatch = 0)
        tab = cbind(self$params[ind != 0, ], data.table(newval = vals[ind]))

        newvals = lapply(split(tab, seq(nrow(tab))), function(x) {
          set = x[[2]][[1]]
          oldval = x[[3]][[1]]
          newval = x[[4]][[1]]

          if(!is.na(newval)){
            if(is.na(oldval)) {
              assertContains(set, newval, x[[1]][[1]])
            } else if(oldval != newval) {
              assertContains(set, newval, x[[1]][[1]])
            }
          }

          newval
        })

        private$.value[ind] = newvals
      }
    },

    ids = function(){
      self$params$Id
    }
  ),

  private = list(
    .support = list(),
    .value = list()
  ))

as.data.table.ParamSet <- function(x, ...){
  x$params
}

as.ParamSet <- function(x,...){
  UseMethod("as.ParamSet")
}
as.ParamSet.data.table <- function(x, ...){
  assert(all(colnames(x) == c("Id", "Support", "Value")))
  assertSetList(x$Support)
  assertNames(x$Id, type = "strict")
  s = x$Support
  v = x$Value
  names(s) = names(v) = x$Id
  ParamSet$new(support = s, value = v)
}

# less efficient than $add, needs work
rbind.ParamSet <- function(...){
  ps = list(...)
  as.ParamSet(rbindlist(lapply(ps, as.data.table)))
}
