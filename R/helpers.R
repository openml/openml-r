showInfo = function(verbosity, msg, ...) {
  showMessage(verbosity, msg, ..., minlev = 1L)
}

showDebug = function(verbosity, msg, ...) {
  showMessage(verbosity, msg, ..., minlev = 2L)
}

showMessage = function(verbosity, msg, ..., minlev) {
  if (is.null(verbosity))
    verbosity = getOMLConfig()$verbosity
  else
    verbosity = asInt(verbosity)
  if (verbosity >= minlev)
    messagef(msg, ...)
}

rename = function(x) {
  if (is.data.frame(x)) {
    setnames(x, str_replace_all(names(x), "_", "."))
  } else {
    names(x) = str_replace_all(names(x), "_", ".")
  }
  x
}

convertNodeSetToList = function(ns, fun = NULL) {
  li = lapply(ns, xmlValue)
  if (!is.null(fun))
    li = lapply(li, fun)
  names(li) = lapply(ns, xmlGetAttr, "name")
  li
}
