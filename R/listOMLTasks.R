#' @title List available OpenML tasks
#'
#' @description
#' The returned \code{data.frame} contains the \code{task_id}, the data set id \code{did},
#' the \code{status} and some describing data qualities.
#'
#' @template arg_verbosity
#' @template arg_status
#' @return [\code{data.frame}].
#' @export
listOMLTasks = function(verbosity = NULL, status = "active") {
  #type = asInt(type)
  status.levels = c("active", "deactivated", "in_preparation")
  assertSubset(status, status.levels)

  content = try(doAPICall(api.call = "task/list", file = NULL, verbosity = verbosity, method = "GET"))

  if (is.error(content))
    return(data.frame())

  xml = parseXMLResponse(content, "Getting task list", "tasks", as.text = TRUE)

  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  df = rbindlist(lapply(blocks, function(node) {
    children = xmlChildren(node)
    is.quality = names(children) == "quality"
    is.input = names(children) == "input"
    is.tag = names(children) == "tag"
    inputs = children[is.input]
    tags = children[is.tag]
    tags = lapply(tags, xmlValue)
    tags = collapse(tags, sep = ", ")
    names(inputs) = vcapply(inputs, function(x) xmlAttrs(x)[["name"]])

    info = list(
        task_id = as.integer(xmlValue(children[["task_id"]])),
        task_type = xmlValue(children[["task_type"]]),
        did = as.integer(xmlValue(children[["did"]])),
        status = xmlValue(children[["status"]]),
        name = xmlValue(children[["name"]]),
        tags = tags,
        estimation_procedure = as.integer(xmlValue(inputs[["estimation_procedure"]])),
        evaluation_measures = collapse(as.character(xmlValue(inputs[["evaluation_measures"]])))
    )
    qualities = convertNodeSetToList(children[is.quality], as.integer)
    c(info, qualities)
  }), fill = TRUE)

  estproc = listOMLEstimationProcedures()
  row.names(estproc) = estproc$est.id
  df$estimation_procedure = estproc[as.character(df$estimation_procedure), "name"]
  df$status = factor(df$status, levels = status.levels)

  # subset status level
  ret = droplevels(as.data.frame(df)[df$status %in% status, ])
  row.names(ret) = NULL
  return(ret)
}
