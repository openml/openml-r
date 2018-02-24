#' @title Extract IDs of a OMLStudy object
#'
#' @description
#' Extracts either all \code{data.id}s, \code{task.id}s, \code{flow.id}s or \code{run.id}s from an \code{\link{OMLStudy}} object.
#'
#' @param object [\code{\link{OMLStudy}}] \cr
#' The OMLStudy object.
#' @param type [\code{character(1)}] \cr
#' A character that specifies which ids should be extracted from the study.
#' Can be either "data.id", "task.id", "flow.id" or "run.id".
#' @return [\code{numeric}].
#' @export
extractOMLStudyIds = function(object, type) {
  assertClass(object, "OMLStudy")
  assertChoice(type, choices = c("data.id", "task.id", "flow.id", "run.id"))

  ret = switch(type,
    data.id = object$data$data.id,
    task.id = object$tasks$task.id,
    flow.id = object$flows$flow.id,
    run.id = object$runs$run.id
  )
  if (is.null(ret))
    messagef("No '%s's found in this study.", type)
  return(ret)
}
