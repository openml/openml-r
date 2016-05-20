
#' @title Converts a mlr task to an OpenML data set.
#'
#' @description
#' Converts a \code{\link[mlr]{Task}} to an \code{\link{OMLDataSet}}.
#'
#' @param task [\code{\link[mlr]{Task}}]\cr
#'   A mlr task.
#' @template arg_description
#'
#' @return [\code{\link{OMLDataSet}}].
#' @family data set-related functions
#' @export
convertMlrTaskToOMLDataSet = function(task, description = NULL){
  assert(checkClass(description, "character"), checkClass(description, "OMLDataSetDescription"), checkNull(description))
  assertClass(task, "Task")
  
  if (is.null(description))
    description = as.character(task$task.desc$id)
  
  if (isTRUE(checkClass(description, "OMLDataSetDescription"))) {
    desc = description
  } else {
    desc = makeOMLDataSetDescription(
      name = task$task.desc$id,
      version = "1",
      description = description,
      format = "ARFF",
      upload.date = as.POSIXct(Sys.time()),
      default.target.attribute = task$task.desc$target,
      status = "active"
    )
  }
  
  cns = colnames(mlr::getTaskData(task))
  
  oml.data = makeOMLDataSet(desc = desc,
    data = mlr::getTaskData(task),
    colnames.old = cns,
    colnames.new = cns,
    target.features = mlr::getTaskTargetNames(task)
  )
  return(oml.data)
}
