#' @title Tagging of OpenML objects
#'
#' @description
#' Add or remove a specific tag to a OpenML data, task, flow or run.
#' 
#' @template arg_id 
#' @template arg_object
#' @param tag [\code{character(1)}]\cr
#'  The tag that should be added.
#' @param method [\code{character(1)}]\cr
#'  Specify if tag should be added or removed.
#' @template arg_verbosity
#' @family dataset related functions
#' @family task related functions
#' @family flow related functions
#' @family run related functions
#' @export
tagOMLObject = function(id, object = c("data", "task", "flow", "run"), 
  tag, method = c("add", "remove"), verbosity = NULL) {
  
  id = asCount(id)
  assertChoice(object, choices = c("data", "task", "flow", "run"))
  assertString(tag)
  assertChoice(method, choices = c("add", "remove"))
  if (object == "task") stop("Tagging of tasks currently not supported by the server.")
  
  action = ifelse(method == "add", "tag", "untag")
  api.string = collapse(c(object, action), sep = "/")
  post.args = setNames(list(id, tag), c(paste0(object,"_id"), "tag"))
  response = doAPICall(api.call = api.string, method = "POST", file = NULL, verbosity = 0,
    post.args = post.args)
  
  if (status_code(response) == 500) {
    parseXMLResponse(response, paste(action, object, id), as.text = TRUE)
  } else {
    res = parseXMLResponse(response, paste(action, object, id), as.text = TRUE)
    showInfo(verbosity, "Successful to %s tag '%s' for %s '%i'.", method, tag, object, id)
  }
  
  return(invisible(response))
}
