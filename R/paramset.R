ParamSet <- R6::R6Class("ParamSet",
  public = list(
    initialize = function(...){

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

      private$.params = par_tab[1,]
      private$.values = par_tab[2,]
    },

    print = function(){
      dt = self$params
      dt$Support = sapply(dt$Support, function(x) x$strprint())
      print(dt)
    }
  ),

  active = list(
    params = function(){
      data.table::data.table(Name = names(private$.params), Support = private$.params,
                             Values = private$.values)
    },

    values = function(vals){
      if(missing(vals)) {
        return(private$.values)
      } else {

        ind = match(self$params$Name, names(vals), nomatch = 0)
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

        private$.values[ind] = newvals
      }
    }
  ),

  private = list(
    .params = list(),
    .values = list()
  ))
