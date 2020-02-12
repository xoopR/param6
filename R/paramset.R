paramset <- R6::R6Class("paramset",
  public = list(
    initialize = function(...){

      if(...length() == 0) {
        stop("Need Parameters")
      }

      params = list(...)
      assertList(params)

      val_lst <- rep(list(bquote()), length(params))
      names(val_lst) <- names(params)

      par_lst <- val_lst

      for (i in 1:length(params)) {
        name = names(params)[[i]]

        if(class(params[[i]])[[1]] == "formula"){
          set = assertSet(eval(params[[i]][[2]]))
          val = params[[i]][[3]]
          if (assert(set$contains(val)))
            val_lst[[name]] = val
        } else {
          set = assertSet(params[[i]])
          val_lst[[name]] = NA
        }

        par_lst[[name]] = set

      }

      private$.params <- par_lst
      private$.values <- val_lst
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
        vals = vals[names(vals) %in% self$params$Name]
        for(i in 1:length(vals)){
          ind = match(names(vals)[[i]], self$params$Name)
          if(is.na(vals[[i]]))
            private$.values[names(vals)[[i]]] = NA
          else if (self$params$Support[[ind]]$contains(vals[[i]]))
            private$.values[names(vals)[[i]]] = vals[[i]]
          else
            stop(sprintf("%s doesn't lie in support of %s (%s).", vals[[i]], names(vals)[[i]], self$params$Support[[ind]]$strprint()))
        }
      }
    }
  ),

  private = list(
    .params = list(),
    .values = list()
  ))
