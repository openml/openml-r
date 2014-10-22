#' @title Download a task from the OpenML repository.
#'
#' @description
#' This function downloads an OpenML task and all associated files from the OpenML repository,
#' intermediately stores the files on disk and creates an S3 object which completely specifies the task.
#'
#' Usually there is no reason to set the \code{fetch.*} arguments to \code{FALSE},
#' as you want all information completely encapsulated in the task object.
#'
#' @param id [\code{integer(1)}]\cr
#'   ID number of task on OpenML server, used to retrieve the task.
#' @param fetch.data.set.description [\code{logical(1)}]\cr
#'   Should the data set description also be downloaded?
#'   Default is \code{TRUE}.
#' @param fetch.data.set [\code{logical(1)}]\cr
#'   Should the data set also be downloaded?
#'   Default is \code{TRUE}.
#' @param fetch.data.splits [\code{logical(1)}]\cr
#'   Should the data splits (for resampling) also be downloaded?
#'   Default is \code{TRUE}.
#' @template arg_ignore.cache
#' @template arg_showinfo
#' @return \code{\link{OpenMLTask}}.
#' @export
#' @examples
#' # Download task and access relevant information to start running experiments
#' \dontrun{
#' task = downloadOpenMLTask(id = 1)
#' show(task)
#' print(task$type)
#' print(task$target.features)
#' print(head(task$data.desc$data.set))
#' }
downloadOpenMLTask = function(id, fetch.data.set.description = TRUE, fetch.data.set = TRUE,
  fetch.data.splits = TRUE, ignore.cache = FALSE, show.info = getOpenMLOption("show.info")) {
  id = asInt(id)
  assertFlag(fetch.data.set.description)
  assertFlag(fetch.data.set)
  assertFlag(fetch.data.splits)
  assertFlag(show.info)

  if (fetch.data.set && !fetch.data.set.description)
    stop("You cannot download a data set without also downloading the data set description!")

  # fn.data.set.desc = file.path(dir, "data_set_description.xml")
  # fn.data.set = file.path(dir, "data_set.ARFF")
  # fn.data.splits = file.path(dir, "data_splits.ARFF")

  if (show.info)
    messagef("Downloading task %i from OpenML repository.", id)

  url = getAPIURL("openml.tasks.search", task_id = id)
  doc = parseXMLResponse(url, "Getting task", "task")
  task = parseOpenMLTask(doc)

  if (fetch.data.set.description) {
    task$data.desc = downloadOpenMLDataSetDescription(task$data.desc.id, ignore.cache = ignore.cache, show.info = show.info)
  }

  if (fetch.data.set) {
    # FIXME: this is duplicated code, we already have functions for that
    # FIXME: keep it simple. just download everything?
    task$data.desc$data.set = downloadOpenMLDataSet(task$data.desc$id, ignore.cache = ignore.cache, show.info = show.info)
    # make valid column names
    task$data.desc$original.col.names = colnames(task$data.desc$data.set)
    task$data.desc$new.col.names = make.names(task$data.desc$original.col.names, unique = TRUE)
    target.inds = which(task$data.desc$original.col.names %in% task$target.features )
    task$target.features = task$data.desc$new.col.names[target.inds]
    colnames(task$data.desc$data.set) = task$data.desc$new.col.names
  }

  if (fetch.data.splits) {
    # No real error handling. If no data splits are available, just print a warning and go on.
    if (task$estimation.procedure$data.splits.url == "No URL") {
      warning("There is no URL to fetch data splits from.\nEither the task type does not support data splits or the task is defective.")
      } else {
      task$estimation.procedure$data.splits = downloadOpenMLDataSplits(task, ignore.cache = ignore.cache, show.info = show.info)
    }
  }
  return(task)
}

parseOpenMLTask = function(doc) {
  getParams = function(path) {
    ns.parameters = getNodeSet(doc, paste(path, "oml:parameter", sep ="/"))
    parameters = lapply(ns.parameters, function(x) xmlValue(x))
    names(parameters) = sapply(ns.parameters, function(x) xmlGetAttr(x, "name"))
    parameters
  }

  # task
  id = xmlRValI(doc, "/oml:task/oml:task_id")
  type = xmlRValS(doc, "/oml:task/oml:task_type")
  targets = xmlValsMultNsS(doc, "/oml:task/oml:input/oml:data_set/oml:target_feature")
  params = getParams("oml:task")

  # data set description
  data.desc.id = xmlRValI(doc, "/oml:task/oml:input/oml:data_set/oml:data_set_id")
  data.desc = NULL

  # prediction
  ns.preds.features = getNodeSet(doc, "/oml:task/oml:output/oml:predictions/oml:feature")
  preds.features = lapply(ns.preds.features, function(x) xmlGetAttr(x, "type"))
  names(preds.features) = sapply(ns.preds.features, function(x) xmlGetAttr(x, "name"))
  preds = list(
    format = xmlRValS(doc, "/oml:task/oml:output/oml:predictions/oml:format"),
    features = preds.features
  )

  # estimation procedure
  # trim white space around URL to be a bit more robust
  data.splits.url = str_trim(xmlOValS(doc, "/oml:task/oml:input/oml:estimation_procedure/oml:data_splits_url"))
  if (is.null(data.splits.url))
    data.splits.url = "No URL"

  estim.proc = makeOpenMLEstimationProcedure(
    type = xmlRValS(doc, "/oml:task/oml:input/oml:estimation_procedure/oml:type"),
    data.splits.url = data.splits.url,
    data.splits = data.frame(),
    parameters = getParams("/oml:task/oml:input/oml:estimation_procedure")
  )

  # measures
  measures = xmlValsMultNsS(doc, "/oml:task/oml:input/oml:evaluation_measures/oml:evaluation_measure")

  task = makeOpenMLTask(
    id = id,
    type = type,
    target.features = targets,
    pars = params,
    data.desc.id = data.desc.id,
    data.desc = data.desc,
    estimation.procedure = estim.proc,
    preds = preds,
    evaluation.measures = measures
  )
  convertOpenMLTaskSlots(task)
}

convertParam = function(params, name, fun) {
  if (!is.null(params[[name]]))
    params[[name]] = fun(params[[name]])
  return(params)
}

convertOpenMLTaskSlots = function(task) {
  # convert estim params to correct types
  p = task$estimation.procedure$parameters
  p = convertParam(p, "number_repeats", as.integer)
  p = convertParam(p, "number_folds", as.integer)
  task$estimation.procedure$parameters = p

  #task$evaluation.measures = strsplit(task$evaluation.measures, split = ",")[[1]]
  return(task)
}
