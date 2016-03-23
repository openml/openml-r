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

checkUserConfirmation = function(type) {
  assertChoice(type, choices = c("dataset", "flow", "task", "run"))

  if (isTRUE(as.logical(getOMLConfig()$confirm.upload))) {
    catf("Do you really want to upload the %s? (yes|no)", type)
    reaction = readLines(con = stdin(), 1L)
    return(grepl(reaction, "yes"))
  }
  return(TRUE)
}

rename = function(x) {
  if (is.data.table(x)) {
    setnames(x, stri_replace_all_fixed(names(x), "_", "."))
  } else {
    names(x) = stri_replace_all_fixed(names(x), "_", ".")
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

arff.reader = function(file){
  reader = getOMLConfig()$arff.reader
  if (reader == "RWeka") RWeka::read.arff(file) else
    farff::readARFF(file, show.info = FALSE)
}

arff.writer = function(x, file){
  reader = getOMLConfig()$arff.reader
  if (reader == "RWeka") RWeka::write.arff(x, file = file) else
      farff::writeARFF(x, path = file)
}

getValidOMLDataSetStatusLevels = function() {
  c("active", "deactivated", "in_preparation")
}

catfNotNA = function(text, obj) {
  if (!all(is.na(obj)))
    catf(text, collapse(obj, sep = "; "))
}
