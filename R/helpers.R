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

# Helper to convert OpenML server variable names (separated with underscores)
# to openml-r names (separated by dots).
#
# @param names [character]
#   Vector of variable names.
# @return [character] "Renamed" variable names.
convertNamesOMLToR = function(names) {
  assertCharacter(names, any.missing = FALSE, all.missing = FALSE)
  # a_b_c to a.b.c
  new.names = gsub("_", ".", names)
  # did to data.id
  new.names = gsub("^did$", "data.id", new.names)
  # ServerVar to server.var
  new.names = gsub("([a-z])([A-Z])", "\\1.\\L\\2", new.names, perl = TRUE)
  # make first character lower case
  new.names = sub("^(.[a-z])", "\\L\\1", new.names, perl = TRUE)
  return(new.names)
}

getRVersionString = function() {
  paste0("R_", collapse(R.Version()[c("major", "minor")], "."))
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

generateAPICall = function(api.call, task.id = NULL, flow.id = NULL, run.id = NULL, uploader.id = NULL,
  number.of.instances = NULL, number.of.features = NULL, number.of.classes = NULL, number.of.missing.values = NULL,
  tag = NULL, data.name = NULL, limit = NULL, offset = NULL) {
  is.sorted = function(x) ifelse(is.unsorted(x), "Must contain increasing values", TRUE)
  assertSorted = makeAssertionFunction(is.sorted)
  assertString(api.call)
  if (!is.null(task.id)) assertIntegerish(task.id)
  if (!is.null(flow.id)) assertIntegerish(flow.id)
  if (!is.null(run.id)) assertIntegerish(run.id)
  if (!is.null(uploader.id)) assertIntegerish(uploader.id)
  if (!is.null(number.of.instances)) {
    if (length(number.of.instances) == 1) number.of.instances = rep(number.of.instances, 2)
    assertIntegerish(number.of.instances, lower = 1, null.ok = TRUE, min.len = 1, max.len = 2)
    assertSorted(number.of.instances)
    number.of.instances = collapse(number.of.instances, sep = "..")
  }
  if (!is.null(number.of.features)) {
    if (length(number.of.features) == 1) number.of.features = rep(number.of.features, 2)
    assertIntegerish(number.of.features, lower = 1, null.ok = TRUE, min.len = 1, max.len = 2)
    assertSorted(number.of.features)
    number.of.features = collapse(number.of.features, sep = "..")
  }
  if (!is.null(number.of.classes)) {
    if (length(number.of.classes) == 1) number.of.classes = rep(number.of.classes, 2)
    assertIntegerish(number.of.classes, lower = 2, null.ok = TRUE, min.len = 1, max.len = 2)
    assertSorted(number.of.classes)
    number.of.classes = collapse(number.of.classes, sep = "..")
  }
  if (!is.null(number.of.missing.values)) {
    if (length(number.of.missing.values) == 1) number.of.missing.values = rep(number.of.missing.values, 2)
    assertIntegerish(number.of.missing.values, lower = 0, null.ok = TRUE, min.len = 1, max.len = 2)
    assertSorted(number.of.missing.values)
    number.of.missing.values = collapse(number.of.missing.values, sep = "..")
  }
  if (!is.null(tag)) assertString(tag, na.ok = FALSE)
  if (!is.null(data.name)) assertString(data.name, na.ok = FALSE)
  if (!is.null(limit)) assertIntegerish(limit, len = 1)
  if (!is.null(offset)) assertIntegerish(offset, len = 1)

  if (length(run.id) > 1)
    run.id = collapse(run.id)
  if (length(task.id) > 1)
    task.id = collapse(task.id)
  if (length(flow.id) > 1)
    flow.id = collapse(flow.id)
  if (length(uploader.id) > 1)
    uploader.id = collapse(uploader.id)
  if (length(tag) > 1)
    tag = collapse(tag, sep = "/")
  url.args = list(
    task = task.id,
    flow = flow.id,
    run = run.id,
    uploader = uploader.id,
    tag = tag,
    number_instances = number.of.instances,
    number_features = number.of.features,
    number_classes = number.of.classes,
    number_missing_values = number.of.missing.values,
    data_name = data.name,
    limit = limit,
    offset = offset
  )
  url.args = Filter(function(x) !is.null(x), url.args)

  api.call = paste0(api.call, "/", collapseNamedList(url.args, sep = "/", collapse = "/"))

  return(api.call)
}
