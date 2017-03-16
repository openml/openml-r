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
  new.names = stri_replace_all_fixed(names, "_", ".")
  # did to data.id
  new.names = gsub("^did$", "data.id", new.names)
  # ServerVar to server.var
  new.names = gsub("([a-z])([A-Z])", "\\1.\\L\\2", new.names, perl = TRUE)
  # make first character lower case
  new.names = sub("^(.[a-z])", "\\L\\1", new.names, perl = TRUE)
  return(new.names)
}

convertTagListToTagString = function(l) {
  vcapply(extractSubList(l, "tags", simplify = FALSE), function(x) collapse(unlist(x), ", "))
}

getRVersionString = function() {
  stri_paste("R_", collapse(R.Version()[c("major", "minor")], "."))
}

# Helper funtion to ask user for confirmation.
#
# @param type [character]
#   Object type, e.g., dataset.
# @param confirm.upload [logical(1)]
#   Confirmation wanted?
#   Default is the config setting.
# @return [logical(1)]
checkUserConfirmation = function(type, confirm.upload = NULL) {
  assertChoice(type, choices = c("dataset", "flow", "task", "run"))
  if (is.null(confirm.upload)) {
    confirm.upload = as.logical(getOMLConfig()$confirm.upload)
  }
  assertFlag(confirm.upload)

  if (confirm.upload) {
    catf("Do you really want to upload the %s? (yes|no)", type)
    reaction = readLines(con = stdin(), 1L)
    return(grepl(reaction, "yes", fixed = TRUE))
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

# collapse numeric values without using scientific representation of large numbers
collapseNotScientific = function(x, ...) {
  assertNumeric(x, null.ok = TRUE)
  if (!is.null(x)) 
    collapse(format(x, scientific = FALSE, trim = TRUE), ...) else
      return(NULL)
}

# argcheck if values are in an increasing order
assertSortedInt = function(x, ..., .var.name = vname(x)) {
  checkSorted = function(x) ifelse(is.unsorted(x), "Must contain increasing values", TRUE)
  assertSorted = makeAssertionFunction(checkSorted)
  
  assertIntegerish(x, ..., .var.name = .var.name)
  if (length(x) == 1) 
    x = rep(x, times = 2)
  assertSorted(x, .var.name = .var.name)
}

generateAPICall = function(api.call, task.id = NULL, flow.id = NULL, run.id = NULL, uploader.id = NULL,
  task.type = NULL, number.of.instances = NULL, number.of.features = NULL, number.of.classes = NULL,
  number.of.missing.values = NULL, tag = NULL, data.name = NULL, data.tag = NULL,
  limit = NULL, offset = NULL, status = NULL) {

  assertString(api.call)
  task.id = collapseNotScientific(assertIntegerish(task.id, null.ok = TRUE))
  flow.id = collapseNotScientific(assertIntegerish(flow.id, null.ok = TRUE))
  run.id = collapseNotScientific(assertIntegerish(run.id, null.ok = TRUE))
  uploader.id = collapseNotScientific(assertIntegerish(uploader.id, null.ok = TRUE))
  
  if (!is.null(task.type)) {
    types = listOMLTaskTypes(verbosity = 0)
    assertChoice(task.type, choices = types$name)
    task.type = types$id[types$name == task.type]
  }
  
  number.of.instances = collapseNotScientific(assertSortedInt(number.of.instances, 
    lower = 1, null.ok = TRUE), sep = "..")
  number.of.features = collapseNotScientific(assertSortedInt(number.of.features, 
    lower = 1, null.ok = TRUE), sep = "..")
  number.of.classes = collapseNotScientific(assertSortedInt(number.of.classes,
    lower = 1, null.ok = TRUE), sep = "..")
  number.of.missing.values = collapseNotScientific(assertSortedInt(number.of.missing.values, 
    lower = 0, null.ok = TRUE), sep = "..")

  if (!is.null(tag)) tag = collapse(assertString(tag, na.ok = FALSE, null.ok = TRUE), sep = "/")
  data.name = assertString(data.name, na.ok = FALSE, null.ok = TRUE)
  data.tag = assertString(data.tag, na.ok = FALSE, null.ok = TRUE)
  limit = collapseNotScientific(assertIntegerish(limit, len = 1, null.ok = TRUE))
  offset = collapseNotScientific(assertIntegerish(offset, len = 1, null.ok = TRUE))
  if (!is.null(status)) assertChoice(status, choices = getValidOMLDataSetStatusLevels())
  
  url.args = list(
    task = task.id,
    flow = flow.id,
    run = run.id,
    uploader = uploader.id,
    tag = tag,
    type = task.type,
    number_instances = number.of.instances,
    number_features = number.of.features,
    number_classes = number.of.classes,
    number_missing_values = number.of.missing.values,
    data_name = data.name,
    data_tag = data.tag,
    limit = limit,
    offset = offset,
    status = status
  )
  url.args = Filter(function(x) !is.null(x), url.args)

  api.call = stri_paste(api.call, "/", collapseNamedList(url.args, sep = "/", collapse = "/"))

  return(api.call)
}

convertNameValueListToRow = function(x) {
  value = lapply(x, function(x) x$value)
  name = vcapply(x, function(x) x$name)
  setNames(value, name)
}

convertNameValueListToDF = function(x) {
  x = lapply(x, convertNameValueListToRow)
  cols = names(x[[1]]) #unique(unlist(lapply(x, names)))
  na.ind = which(vnapply(x, length) == 0)
  x[na.ind] = lapply(seq_along(na.ind), function(x) setNames(rep(list(NA), length(cols)), cols))
  #as.list(setNames(rep(NA, length(cols)), cols)))
  x = rbindlist(x, fill = TRUE)
  return(x)
}

extractRVersionFromFlow = function(flow) {
  version = strsplit(flow$dependencies, ",")[[1L]]
  stri_replace_all_fixed(version[stri_detect_fixed(version, "R_")], "R_", "")
}

# forget all memoised listing functions
forgetAll = function() {
  forget(listOMLDataSetQualities)
  forget(listOMLDataSets)
  forget(listOMLEstimationProcedures)
  forget(listOMLEvaluationMeasures)
  forget(listOMLFlows)
  forget(listOMLRunEvaluations)
  forget(listOMLRuns)
  forget(listOMLTasks)
  forget(listOMLTaskTypes)
}
