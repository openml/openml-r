#' @title Extract IDs of a OMLStudy object
#'
#' @description
#' Extracts either all \code{data.id}s, \code{task.id}s, \code{flow.id}s or \code{run.id}s from an \code{OMLStudy} object.
#'
#' @param object [\code{OMLStudy}] \cr
#' The OMLStudy object.
#' @param type [\code{character(1)}] \cr
#' A character that specifies which ids should be extracted from the study.
#' Can be either "data.id", "task.id", "flow.id" or "run.id".
#' @param chunk.size [\code{integer(1)}] \cr
#' If the number of ids to be returned exceeds "chunk.size", a list of ids is returned.
#' Each list element contains not more than "chunk.size" elements.
#' Default is 400.
#' @return [\code{numeric}].
#' @export
extractOMLStudyIds = function(object, type, chunk.size = 400) {
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

  if (length(ret) > chunk.size) {
    messagef("More than '%s' ids found. Returning a list of ids.", chunk.size)
    ret = BBmisc::chunk(ret, chunk.size = chunk.size)
  }
  return(ret)
}
