#' @title Get OpenML Study information.
#'
#' @description
#' A OpenML study is a collection of OpenML objects with a specific tag defined by the user (i.e. "study_X").
#'
#' @param study.id [\code{character(1)}]\cr
#' @template arg_verbosity
#' @return [\code{OMLStudy}].
#' @family downloading functions
#' @export
getOMLStudy = function(study.id = NULL, verbosity = NULL) {
  study.id = collapseNotScientific(assertIntegerish(study.id, null.ok = TRUE))
  api.call = generateAPICall(paste0("json/study/", study.id))

  content = doAPICall(api.call = api.call, file = NULL, verbosity = verbosity, method = "GET")
  if (is.null(content)) return(data.frame())
  study = fromJSON(txt = content)$study

  # make proper names
  names(study) = convertNamesOMLToR(names(study))
  study = lapply(study, function(x) {
    if (!is.null(names(x)))
      names(x) = convertNamesOMLToR(names(x))
    return(x)
  })

  # convert to numeric values
  study = rapply(study, type.convert, as.is = TRUE, how = "replace")
  return(setClasses(study, "OMLStudy"))
}

#' @export
print.OMLStudy = function(x, ...) {
  catf('\n Study "%s" (Study ID %i)', x$name, x$id)
  catf('  Description           : %s', BBmisc::clipString(x$description, 80))
  catf('  Creation Date         : %s', x$creation.date)
  catf('  Tag                   : %s', x$tag$name)
  catf('  Number of Data Sets   : %s', length(x$data$data.id))
  catf('  Number of Tasks       : %s', length(x$tasks$task.id))
  catf('  Number of Flows       : %s', length(x$flows$flow.id))
  # catf('  Number of Setups      : %s', length(x$setups$setup.id))
  catf('  Number of Runs        : %s', length(x$runs$run.id))
  # catf('  Data IDs         : %s', BBmisc::clipString(BBmisc::collapse(x$data$data.id), 80))
  # catf('  Task IDs         : %s', BBmisc::clipString(BBmisc::collapse(x$tasks$task.id), 80))
  # catf('  Flow IDs         : %s', BBmisc::clipString(BBmisc::collapse(x$flows$flow.id), 80))
  # catf('  Setup IDs        : %s', BBmisc::clipString(BBmisc::collapse(x$setups$setup.id), 80))
  # catf('  Run IDs          : %s', BBmisc::clipString(BBmisc::collapse(x$runs$run.id), 80))
}
