# @title Construct OMLRunParameter.
#
# @param name [\code{character(1)}]\cr
#    The name of the parameter.
# @param value [\code{character(1)}]\cr
#    The value of the parameter.
# @param component [\code{character(1)}]\cr
#    The implementation name of a component, if the parameter belongs to this component.
#    This name must match a component of the implementation.
# @export
# @aliases OMLRunParameter
# @family run related functions
makeOMLRunParameter = function(name, value, component = NA_character_) {
  assertString(name)
#  assertString(value)
  assertString(component, na.ok = TRUE)
  if (length(value) > 1) stopf("length of parameter '%s' is more than one", name)

  makeS3Obj("OMLRunParameter",
    name = name,
    value = value,
    component = component
  )
}

# show
# @export
print.OMLRunParameter = function(x, ...)  {
  if (!is.na(x$component))
    s = sprintf(' (parameter of component %s)', x$component)
  else
    s = ""
  # FIXME: make it better
  val = try(as.character(x$value))
  catf("%s %s = %s", s, x$name, ifelse(is.error(val), "can't print this data type", x$value))
}
