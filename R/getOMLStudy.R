.getOMLStudy = function(study = NULL, verbosity = NULL) {
  #study = collapseNotScientific(assertIntegerish(study, null.ok = TRUE))
  assert(checkIntegerish(study, null.ok = TRUE), checkString(study, null.ok = TRUE))
  study = if (is.numeric(study)) collapseNotScientific(study) else study
  api.call = generateAPICall(paste0("json/study/", study))

  content = doAPICall(api.call = api.call, file = NULL, verbosity = verbosity, method = "GET")
  if (is.null(content)) return(data.frame())
  ret = fromJSON(txt = content)$study

  # make proper names
  names(ret) = convertNamesOMLToR(names(ret))
  ret = lapply(ret, function(x) {
    if (!is.null(names(x)))
      names(x) = convertNamesOMLToR(names(x))
    return(x)
  })

  # convert to numeric values
  ret = rapply(ret, type.convert, as.is = TRUE, how = "replace")
  return(setClasses(ret, "OMLStudy"))
}

#' @title Get OpenML Study information.
#'
#' @description
#' A OpenML study is a collection of OpenML objects with a specific tag defined by the user (i.e. "study_X").
#' If you create a study through the website \url{https://www.openml.org/new/study}, you can also specify an alias which can be used to access the study.
#'
#' @template note_memoise
#'
#' @param study [\code{numeric(1)}|\code{character(1)}]\cr
#'   Either the id or the alias of a study.
#' @template arg_verbosity
#' @return [\code{OMLStudy}].
#' @family downloading functions
#' @export
getOMLStudy = memoise(.getOMLStudy)
