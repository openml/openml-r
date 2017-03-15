.listOMLTaskTypes = function(verbosity = NULL) {
  content = doAPICall(api.call = "json/tasktype/list", file = NULL, verbosity = verbosity, method = "GET")
  res = fromJSON(txt = content)$task_types$task_type
  res$id = as.integer(res$id)
  return(res[, c("id", "name")])
}

#' @title List available OpenML task types.
#'
#' @description
#' The returned \code{data.frame} contains the type \code{id} and the character
#' name of the OpenML task type.
#'
#' @template note_memoise
#'
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family listing functions
#' @family task-related functions
#' @export
#' @example inst/examples/listOMLTaskTypes.R
listOMLTaskTypes = memoise(.listOMLTaskTypes)
