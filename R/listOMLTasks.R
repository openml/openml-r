#' @title List available OpenML tasks
#'
#' @description
#' The returned data.frame contains the \code{task_id}, the data set id \code{did},
#' the \code{status} and some describing data qualities.
#'
#' @param type [\code{integer(1)}]\cr
#'   The task type you want to list tasks for.
#'   See code \link{listOMLTaskTypes}.
#'   Default is \code{1} for classification.
#' @template arg_hash
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @export
listOMLTasks = function(type = 1L, session.hash = getSessionHash(), verbosity = NULL) {
  type = asInt(type)
  url = getAPIURL("openml.tasks", task_type_id = type)
  content = downloadXML(url, NULL, verbosity = verbosity, session_hash = session.hash)
  xml = parseXMLResponse(content, "Getting task list", "tasks", as.text = TRUE)

  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  quals = rbindlist(lapply(blocks, function(node) {
    children = xmlChildren(node)
    qualities = names(children) == "quality"
    row = c(
      list(
        as.integer(xmlValue(children[["task_id"]])),
        as.factor(xmlValue(children[["task_type"]])),
        as.integer(xmlValue(children[["did"]])),
        as.factor(xmlValue(children[["status"]]))
      ),
      as.list(as.integer(vcapply(children[qualities], xmlValue)))
    )
    names(row) = c("task_id", "task_type", "did", "status",
      vcapply(children[qualities], xmlAttrs))
    row
  }), fill = TRUE)

  as.data.frame(quals)
}
