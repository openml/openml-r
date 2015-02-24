#' @title List available OpenML tasks
#'
#' @description
#' The returned \code{data.frame} contains the \code{task_id}, the data set id \code{did},
#' the \code{status} and some describing data qualities.
#'
#' @param type [\code{integer(1)}]\cr
#'   The task type you want to list tasks for.
#'   See code \link{listOMLTaskTypes}.
#'   Default is \code{1} for classification.
#' @template arg_hash
#' @template arg_verbosity
#' @template arg_status
#' @return [\code{data.frame}].
#' @export
listOMLTasks = function(type = 1L, session.hash = getSessionHash(),
  verbosity = NULL, status = "active") {
  type = asInt(type)
  assertString(session.hash)
  status.levels = c("active", "deactivated", "in_preparation")
  assertSubset(status, status.levels)

  url = getAPIURL("openml.tasks", task_type_id = type)
  content = try(downloadXML(url, NULL, verbosity = verbosity, session_hash = session.hash), silent = TRUE)

  if (is.error(content))
    return(data.frame())

  xml = parseXMLResponse(content, "Getting task list", "tasks", as.text = TRUE)

  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  df = rbindlist(lapply(blocks, function(node) {
    children = xmlChildren(node)
    is.quality = names(children) == "quality"
    is.input = names(children) == "input"
    inputs = children[is.input]
    names(inputs) = vcapply(inputs, function(x) xmlAttrs(x)[["name"]])

    info = list(
        task_id = as.integer(xmlValue(children[["task_id"]])),
        task_type = xmlValue(children[["task_type"]]),
        did = as.integer(xmlValue(children[["did"]])),
        status = xmlValue(children[["status"]]),
        name = xmlValue(children[["name"]]),
        estimation_procedure = as.integer(xmlValue(inputs[["estimation_procedure"]])),
        evaluation_measures = collapse(as.character(xmlValue(inputs[["evaluation_measures"]])))
    )
    qualities = convertNodeSetToList(children[is.quality], as.integer)
    c(info, qualities)
  }), fill = TRUE)
  df$status = factor(df$status, levels = status.levels)

  # subset status level
  ret = droplevels(as.data.frame(df)[df$status%in%status, ])
  row.names(ret) = NULL
  return(ret)
}
