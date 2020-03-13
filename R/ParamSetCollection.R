ParamSetCollection = R6::R6Class("ParamSetCollection",
  public = list(
    initialize = function(sets){
      assert_list(sets, types = "ParamSet")
      assert_names(names(sets), type = "unique")
      private$.sets = sets
    },

    print = function(){
      dt = rbindlist(mlr3misc::map(private$.sets, function(x) x$params))
      nr = mlr3misc::map(private$.sets, function(x) nrow(x$params))
      dt$Id = paste(rep(names(private$.sets), nr), dt$Id, sep = "_")
      dt$Support = sapply(dt$Support, function(x) x$strprint())
      ftag = sapply(dt$Tag, function(x) if(!is.null(x)) paste0("{", paste0(x, collapse = ", "), "}"))
      if(length(ftag) != 1 | !is.null(ftag[[1]])){
        dt$Tag = ftag
      }

      print(dt)
    },

    add = function(sets){
      assert_list(sets, types = "ParamSet")
      sets = c(private$.sets, sets)
      assert_names(names(sets), type = "unique")
      private$.sets = sets
    },

    remove = function(sets){
      assertCharacter(sets)
      private$.sets = private$.sets[!(names(private$.sets) %in% sets)]
    },

    get_values = function(tag){
      values = self$values
      vals = vector("list", length(self$ids))
      names(vals) = self$ids
      vals[match(names(values), self$ids, 0)] = values
      if (!missing(tag)) {
        return(vals[grepl(tag, self$tags)])
      } else {
        return(vals)
      }
    }
  ),

  active = list(
    params = function(){
      dt = rbindlist(mlr3misc::map(private$.sets, function(x) x$params))
      nr = mlr3misc::map(private$.sets, function(x) nrow(x$params))
      dt$Id = paste(rep(names(private$.sets), nr), dt$Id, sep = "_")
      dt
    },

    supports = function(){
      s = unlist(mlr3misc::map(private$.sets, function(x) x$supports), recursive = FALSE)
      names(s) = sub(".","_",names(s),fixed=T)
      return(s)
    },

    tags = function(){
      s = unlist(mlr3misc::map(private$.sets, function(x) x$tags), recursive = FALSE)
      names(s) = sub(".","_",names(s),fixed=T)
      return(s)
    },

    length = function(){
      nrow(self$params)
    },

    ids = function(){
      names(self$supports)
    },

    values = function(vals){
      if (missing(vals)) {
        v = unlist(mlr3misc::map(private$.sets, function(x) x$values), recursive = FALSE)
        names(v) = sub(".","_",names(v),fixed=T)
        return(v)
      } else {
        sets = vapply(strsplit(names(vals),"_"),"[[",character(1),1)
        names(vals) = sapply(strsplit(names(vals),"_"),function(x) paste0(x[-1],collapse="_"))

        sapply(unique(sets), function(aset){
          private$.sets[[aset]]$values = vals[sets == aset]
        })
      }
    }
  ),

  private = list(
    .sets = list()
  )
)
