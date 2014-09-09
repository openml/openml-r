#' Construct OpenMLImplementationParameter.
#'
#' @param name [\code{character(1)}]\cr
#'   The name of the parameter.
#' @param data.type [\code{character(1)}]\cr
#'   The data type of the parameter. Should be either integer, numeric, string, vector, matrix or object.
#' @param default.value [\code{character(1)}]\cr
#'   The default value of the parameter.
#' @param description [\code{character(1)}]\cr
#'   A description of what this parameter does.
#' @param recommended.range [\code{character(1)}]\cr
#'   Minimal/maximal value and/or a recommended range of values. 
#' @export
#' @aliases OpenMLImplementationParameter
makeOpenMLImplementationParameter = function(
  name, 
  data.type = NA_character_, 
  default.value = NA_character_, 
  description = NA_character_,
  recommended.range = NA_character_
  ) {
  
  assertString(name)
  assertString(default.value, na.ok = TRUE)
  assertString(description, na.ok = TRUE)
  assertString(recommended.range, na.ok = TRUE)
  
  makeS3Obj("OpenMLImplementationParameter", 
      name = name,
      data.type = data.type,
      default.value = default.value,
      description = description,
      recommended.range = recommended.range
  )
}

# ***** Methods *****

# show
#' @export
print.OpenMLImplementationParameter = function(x, ...) {  
  catfNotNA = function(text, obj) {
    if (!all(is.na(obj)))
      catf(text, collapse(obj, sep = "; "))
  }
  catf('Parameter %s:', x$name)
  catfNotNA('\ttype             : %s', x$data.type)
  catfNotNA('\tdefault          : %s', x$default.value)
  catfNotNA('\trecommended range: %s', x$recommended.range)
  cat('\n')
  cat(collapse(paste0(strwrap(x$description, width = getOption("width") - 2), '\n'), sep = ''))
  cat(collapse(rep("_", getOption("width") - 2), sep = ""))
}
