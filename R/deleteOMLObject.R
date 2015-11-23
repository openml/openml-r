#' @title Delete an OpenML object
#'
#' @description This will delete one of your uploaded datasets, tasks, flows or runs.
#'
#' @param id [\code{integer(1)}]\cr
#'   The ID of the respective object. Note that you can only delete the objects you uploaded.
#' @param object [\code{character(1)}]\cr
#'   A character that specifies the object you want to delete from the server. Can be either
#'   \code{"data"}, \code{"task"}, \code{"flow"} or \code{"run"}.
#' @template arg_verbosity
#' @family dataset related functions
#' @family task related functions
#' @family flow related functions
#' @family run related functions
#' @export
deleteOMLObject = function(id, object = c("data", "task", "flow", "run"), verbosity = NULL){
  id = asCount(id)
  assertChoice(object, choices = c("data", "task", "flow", "run"))
  
  response = try(doAPICall(api.call = object, method = "DELETE", id = id))
  if (is.error(response)) {
    stopf("Unknown %1$s. Please check the %1$s id", object)
  }
  if (!is.null(content(response))) {
    doc = parseXMLResponse(response, paste("Deleting", object), 
     paste0(object, "_delete"), as.text = TRUE)
  }
  showInfo(verbosity, "The %s with id %s was succesfully deleted.", object, id)
}