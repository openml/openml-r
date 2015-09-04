#' @title Delete an OpenML flow.
#'
#' @description This will delete one of your uploaded \code{\link{OMLFlow}s}.
#'
#' @param flow.id [\code{integer(1)}]\cr
#'   The flow ID. Of course, you can only delete your own flows.
#' @template arg_verbosity
#' @export
deleteOMLFlow = function(flow.id, verbosity = NULL) {
  flow.id = asCount(flow.id)
  
  response = try(doAPICall(api.call = "flow", method = "DELETE", id = flow.id), silent = TRUE)
  if (is.error(response)) {
    stop("Unknown flow. Please check the flow.id.")
  }
  doc = parseXMLResponse(response, "Deleting flow", "flow_delete", as.text = TRUE)
  showInfo(verbosity, "Flow %s succesfully deleted.", flow.id)
}
