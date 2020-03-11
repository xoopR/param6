assertContains = function(set, value, name){
  if(set$contains(value)) {
    invisible(value)
  } else {
    if(!missing(name)) {
      stop(sprintf("%s does not lie in support of %s (%s).", value, name, set$strprint()))
    } else {
      stop(sprintf("%s does not lie in %s.", value, set$strprint()))
    }
  }
}

assert_no_cycles = function(lookup){
  check = data.table(lookup[,2])
  checks = data.table(lookup[,1])

  for(i in seq_along(transpose(lookup))){
    check = merge(check, lookup, by.x = "on", by.y = "id", sort = FALSE)[,2]
    colnames(check) = "on"
    checks = cbind(checks, check)

    checker = apply(checks, 1, function(x) any(duplicated(x)))
    if(any(checker)){
      stop(sprintf("Cycles detected starting from id(s): %s", paste0("{",paste0(checks$id[checker], collapse=","),"}")))
    }


  }
}

assert_condition = function(id, support, type, cond){
  if(type %in% c("Equal","AnyOf")){
    msg = sprintf("%s does not lie in support of %s (%s). Condition is not possible.", cond, id, support$strprint())
  } else {
    msg = sprintf("%s does not lie in support of %s (%s). Condition is redundant.", cond, id, support$strprint())
  }

  if(!(testContains(support, cond))){
    stop(msg)
  }

}
