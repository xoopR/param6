makeParam <- function(param){
  tag = set = value = NULL

  if(class(param)[1] != "formula") {
    set = assertSet(param)
  } else {
    set = assertSet(eval(param[[2]]))
    param = param[[3]]
    if(class(param)[1] != "call"){
      value = assert_contains(set, param)
    } else {
      tags = grepl("^tags\\(.*\\)$", param)
      if(any(tags)){
        tag = as.character(param[tags][[1]])[-1]
        value = assert_contains(set, param[!tags][-1][[1]])
      } else {
        tag = as.character(param[-1])
      }
    }
  }

  list(set = set, value = value, tag = tag)
}
