#' @title Construct OpenMLRunParameter
#'
#' @param name [\code{character(1)}]\cr
#'    The name of the parameter.
#' @param value [\code{character(1)}]\cr
#'    The value of the parameter.
#' @param component [\code{character(1)}]\cr
#'    The implementation name of a component, if the parameter belongs to this component.
#'    This name must match a component of the implementation.
#'
#' @export
#' @aliases OpenMLRunParameter
makeOpenMLRunParameter = function(name, value, component = NA_character_) {
  assertString(name)
  assertString(value)
  assertString(component, na.ok = TRUE)

  makeS3Obj("OpenMLRunParameter",
    name = name,
    value = value,
    component = component
  )
}

# ***** Methods *****

# show
#' @export
print.OpenMLRunParameter = function(x, ...)  {
  if (!is.na(x$component))
    s = sprintf(' (parameter of component %s)', x$component)
  else
    s = ""
  # FIXME does this work for arbitary values? unit test this
  catf("%s %s = %s", s, x$name, x$value)
}
