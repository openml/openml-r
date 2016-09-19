#' @title Tagging of OpenML objects
#'
#' @description
#' Add or remove a specific tag to a OpenML data, task, flow or run.
#'
#' @template arg_ids
#' @template arg_object
#' @param tags [\code{character}]\cr
#'  The tags that should be added/removed.
#' @template arg_verbosity
#' @family data set-related functions
#' @family task-related functions
#' @family flow-related functions
#' @family run-related functions
#' @rdname tagging
#' @export
tagOMLObject = function(ids, object = c("data", "task", "flow", "run"), tags, verbosity = NULL) {
  lapply(ids, function(id)
    multipleTagsOMLObject(id, object, tags, method = "add", verbosity = verbosity))
  return(invisible(NULL))
}

#' @rdname tagging
#' @export
untagOMLObject = function(ids, object = c("data", "task", "flow", "run"), tags, verbosity = NULL) {
  lapply(ids, function(id)
    multipleTagsOMLObject(id, object, tags, method = "remove", verbosity))
  return(invisible(NULL))
}

multipleTagsOMLObject = function(id, object = c("data", "task", "flow", "run"),
  tags, method = c("add", "remove"), verbosity = NULL) {
  lapply(tags, function(tag) {
    singleTagOMLObject(id, object, tag, method, verbosity)
  })
  return(invisible(NULL))
}

singleTagOMLObject = function(id, object = c("data", "task", "flow", "run"),
  tag, method = c("add", "remove"), verbosity = NULL) {

  id = asCount(id)
  assertChoice(object, choices = c("data", "task", "flow", "run"))
  assertString(tag, na.ok = FALSE)
  assertChoice(method, choices = c("add", "remove"))

  action = ifelse(method == "add", "tag", "untag")
  api.string = collapse(c(object, action), sep = "/")
  post.args = setNames(list(id, tag), c(paste0(object,"_id"), "tag"))
  doAPICall(api.call = api.string, method = "POST", file = NULL, verbosity = 0,
    post.args = post.args)

  return(invisible(NULL))
}
