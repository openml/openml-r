#' @title Get table of available tasks OpenML server.
#'
#' @description
#' The returned data.frame contains the \code{task_id}, the data set id \code{did},
#' the \code{status} and some describing data qualities.
#'
#' @param id [\code{integer(1)}]\cr
#'   ID of task type you want to list tasks for.
#'   See code \link{getOpenMLTaskTypeList}}
#'   Default is 1.
#' @template arg_verbosity
#' @return [\code{list}]. One element per task type, containing \code{id}, \code{name} and
#'   \code{description}.
#' @export
getOpenMLTaskList = function(type = 1L, verbosity = NULL) {
  type = asInt(type)
  url = getAPIURL("openml.tasks", task_type_id = 1)
  contents = downloadXML(url, NULL, verbosity)
  xml = parseXMLResponse(contents, "Getting task list", "tasks", as.text = TRUE)
  # get list of blocks for tasks
  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  quals = list()
  for (i in seq_along(blocks)) {
    node = blocks[[i]]
    quals1 = xmlChildren(node)[-(1:4)]
    quals2 = as.list(as.numeric(vcapply(quals1, xmlValue)))
    names(quals2) = vcapply(quals1, xmlAttrs)
    # make sure that empty row without qualities are not dropped by rbind.fill
    quals2$.foo = 1
    quals[[i]] = as.data.frame(quals2, stringsAsFactors = FALSE)
  }
  quals = do.call(rbind.fill, quals)
  quals = cbind(
    task_did = xmlValsMultNsI(xml, "/oml:tasks/oml:task/oml:task_id"),
    task_type = xmlValsMultNsS(xml, "/oml:tasks/oml:task/oml:task_type"),
    did = xmlValsMultNsI(xml, "/oml:tasks/oml:task/oml:did"),
    status = xmlValsMultNsS(xml, "/oml:tasks/oml:task/oml:status"),
    quals,
    stringsAsFactors = FALSE
  )
  quals$.foo = NULL
  return(quals)
}

