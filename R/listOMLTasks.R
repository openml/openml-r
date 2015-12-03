#' @title List available OpenML tasks.
#'
#' @description
#' The returned \code{data.frame} contains the \code{task_id}, the data set id \code{did},
#' the \code{status} and some describing data qualities.
#'
#' @template arg_verbosity
#' @template arg_status
#' @return [\code{data.frame}].
#' @family listing functions
#' @family task related functions
#' @export
listOMLTasks = function(verbosity = NULL, status = "active") {
  assertSubset(status, getValidOMLDataSetStatusLevels())

  content = try(doAPICall(api.call = "task/list", file = NULL, verbosity = verbosity, method = "GET"))

  if (is.error(content)) return(data.frame())

  d = xmlRoot(xmlParse(content))

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
    out.vars = c("task_id", "task_type", "did", "status", "name", "tags",
                 "estimation_procedure", "evaluation_measures", names[names(names)%in%"quality"])
    return(as.list(strings[out.vars]))
  })
  li = as.data.frame(rbindlist(info, fill = TRUE))
  li = li[, !is.na(colnames(li))]
  int.vars = setdiff(colnames(li), c("task_type", "status", "name", "tags", "evaluation_measures"))
  li[, int.vars] = lapply(int.vars, function(x) as.integer(li[, x]))

  estproc = listOMLEstimationProcedures(verbosity = FALSE)
  row.names(estproc) = estproc$est.id
  li$estimation_procedure = droplevels(estproc[as.character(li$estimation_procedure), "name"])
  li$status = as.factor(li$status)

  #FIXME: do we want to replace _ by . in colnames?
  return(li)
}
