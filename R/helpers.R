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

generateAPICall = function(api.call, task.id = NULL, flow.id = NULL,
  run.id = NULL, uploader.id = NULL, tag = NULL, limit = NULL, offset = NULL) {
  assertString(api.call)
  if (!is.null(task.id)) assertIntegerish(task.id)
  if (!is.null(flow.id)) assertIntegerish(flow.id)
  if (!is.null(run.id)) assertIntegerish(run.id)
  if (!is.null(uploader.id)) assertIntegerish(uploader.id)
  if (!is.null(tag)) assertString(tag, na.ok = FALSE)
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
    limit = limit,
    offset = offset
  )
  url.args = Filter(function(x) !is.null(x), url.args)
  
  api.call = paste0(api.call, "/", collapseNamedList(url.args, sep = "/", collapse = "/"))
  
  return(api.call) 
}
