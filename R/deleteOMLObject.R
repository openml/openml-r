#' @title Delete an OpenML object.
#'
#' @description
#' This will delete one of your uploaded datasets, tasks, flows or runs.
#' Note that you can only delete the objects you uploaded.
#'
#' @template arg_id
#' @template arg_object
#' @template arg_verbosity
#' @family data set-related functions
#' @family task-related functions
#' @family flow-related functions
#' @family run-related functions
#' @export
deleteOMLObject = function(id, object = c("data", "task", "flow", "run", "study"), verbosity = NULL) {
  id = asCount(id)
  assertChoice(object, choices = c("data", "task", "flow", "run", "study"))

  response = doAPICall(api.call = object, method = "DELETE", id = id)

  if (!is.null(content(response)))
    parseXMLResponse(response, paste("Deleting", object), paste0(object, "_delete"), as.text = TRUE)
  showInfo(verbosity, "The %s with ID %s was succesfully deleted.", object, id)

  return(invisible(response))
}
