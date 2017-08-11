#' @title list OpenML Studies.
#'
#' @description
#' Retrives a list of available studies.
#'
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family listing functions
#' @export
listOMLStudies = function(verbosity = NULL) {
  api.call = generateAPICall("json/study/list")

  content = doAPICall(api.call = api.call, file = NULL, verbosity = verbosity, method = "GET")
  if (is.null(content)) return(data.frame())

  study = fromJSON(txt = content)$study$study

  if (is.list(study$name)) {
    empty = vnapply(study$name, length) == 0
    study$name = unlist(replace(study$name, which(empty), NA_character_))
  }
  study = as.data.frame(rapply(study, type.convert, as.is = TRUE, how = "replace"))

  # make proper names
  names(study) = convertNamesOMLToR(names(study))
  names(study) = gsub("^id$", "study.id", names(study))

  return(study)
}
