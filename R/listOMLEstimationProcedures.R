.listOMLEstimationProcedures = function(verbosity = NULL) {
  content = doAPICall(api.call = "json/estimationprocedure/list", file = NULL,
    verbosity = verbosity, method = "GET")
  res = fromJSON(txt = content)$estimationprocedures$estimationprocedure
  task.types = .listOMLTaskTypes(verbosity = 0)
  row.names(task.types) = task.types$id
  data.frame(
    est.id = as.integer(res$id),
    task.type = as.character(task.types[as.character(res$ttid), "name"]),
    name = res$name,
    stringsAsFactors = TRUE
  )
}

#' @title List available estimation procedures.
#'
#' @description
#' The returned \code{data.frame} contains the \code{est.id} and the corresponding
#' name of the estimation procedure.
#'
#' @template note_memoise
#'
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family listing functions
#' @export
#' @example inst/examples/listOMLEstimationProcedures.R
listOMLEstimationProcedures = memoise(.listOMLEstimationProcedures)
