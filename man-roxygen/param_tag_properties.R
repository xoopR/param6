#' @param tag_properties (`list()`) \cr
#'  List of tag properties. Currently supported properties are: i) 'required' -
#'  parameters with this tag property must be non-NULL; ii) 'linked' - only one
#'  parameter in a linked tag group can be non-NULL and the others should be
#'  NULL, this only makes sense with an associated `trafo`; iii) 'unique' -
#'  parameters with this tag must have no duplicated elements, only makes sense
#'  for vector parameters; iv) 'immutable' - parameters with this tag cannot be
#' updated after construction.
