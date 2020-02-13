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
