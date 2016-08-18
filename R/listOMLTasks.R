.listOMLTasks = function(NumberOfInstances = NULL, NumberOfFeatures = NULL,
  NumberOfClasses = NULL, NumberOfMissingValues = NULL,
  tag = NULL, limit = NULL, offset = NULL, status = "active", verbosity = NULL) {
  assertSubset(status, getValidOMLDataSetStatusLevels())

  api.call = generateAPICall("task/list",
    NumberOfInstances = NumberOfInstances, NumberOfFeatures = NumberOfFeatures,
    NumberOfClasses = NumberOfClasses, NumberOfMissingValues = NumberOfMissingValues,
    tag = tag, limit = limit, offset = offset)

  content = doAPICall(api.call = api.call, file = NULL, verbosity = verbosity, method = "GET")

  d = parseXMLResponse(content, as.text = TRUE, return.doc = FALSE) #xmlRoot(xmlParse(content))

  # get values from each XML string
  string.list = xmlSApply(d, getChildrenStrings)
  # get indices where string.status is included in status
  string.ind = which(vcapply(string.list, function(X) X["status"]) %in% status)

  # subset with respect to 'status' (speedup)
  string.list = string.list[string.ind]
  child.list = sapply(d[string.ind], xmlChildren)

  info = lapply(1:length(string.list), function(X) {
    strings = string.list[[X]]
    child = child.list[[X]]
    # get index of input and quality childrens
    ind = names(child) %in% c("input", "quality")
    # FIXME: not looking up names with xmlAttrs would speedup the code, can we use fixed names here?
    # get the name attributes of them
    names = vcapply(child[ind], xmlAttrs, "name")
    # replace the names of the children-string with the names attribute
    names(strings)[ind] = names
    # get the tag indices and paste them together as single column
    tag.ind = names(strings) == "tag"
    strings = c(strings[!tag.ind], "tags" = collapse(strings[tag.ind], sep = ", "))
    out.vars = c("task_id", "task_type", "did", "status", "format", "name", "target_feature", "tags",
      "estimation_procedure", "evaluation_measures", names[names(names)%in%"quality"])
    return(as.list(strings[out.vars]))
  })
  li = as.data.frame(rbindlist(info, fill = TRUE))
  li = li[, !is.na(colnames(li))]
  int.vars = setdiff(colnames(li), c("task_type", "status", "format", "name", "target_feature", "tags", "evaluation_measures"))
  li[, int.vars] = lapply(int.vars, function(x) as.integer(li[, x]))

  estproc = listOMLEstimationProcedures(verbosity = 0L)
  row.names(estproc) = estproc$est.id
  li$estimation_procedure = droplevels(estproc[as.character(li$estimation_procedure), "name"])
  li$status = as.factor(li$status)

  #FIXME: do we want to replace _ by . in colnames?
  colnames(li) = gsub("_", ".", colnames(li))
  return(li)
}

#' @title List available OpenML tasks.
#'
#' @description
#' The returned \code{data.frame} contains the \code{task_id}, the data set id \code{did},
#' the \code{status} and some describing data qualities.
#'
#' @template note_memoise
#'
#' @template arg_NumberOfInstances
#' @template arg_NumberOfFeatures
#' @template arg_NumberOfClasses
#' @template arg_NumberOfMissingValues
#' @template arg_tag
#' @template arg_limit
#' @template arg_offset
#' @template arg_status
#' @template arg_verbosity
#'
#' @return [\code{data.frame}].
#' @family listing functions
#' @family task-related functions
#' @export
#' @example inst/examples/listOMLTasks.R
listOMLTasks = memoise(.listOMLTasks)
