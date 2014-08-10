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
#' @param dir [\code{character(1)}]\cr
#'   Directory where downloaded files from the repository are stored. If the directory does not exist,
#'   it will be created.
#'   Default is the path of the per-session temporary directory \code{tempdir()}.
#' @param clean.up [\code{logical(1)}]\cr
#'   Should the downloaded files be removed from disk at the end?
#'   Default is \code{TRUE}.
#' @param fetch.data.set.description [\code{logical(1)}]\cr
#'   Should the data set description also be downloaded?
#'   Default is \code{TRUE}.
#' @param fetch.data.set [\code{logical(1)}]\cr
#'   Should the data set also be downloaded?
#'   Default is \code{TRUE}.
#' @param fetch.data.splits [\code{logical(1)}]\cr
#'   Should the data splits (for resampling) also be downloaded?
#'   Default is \code{TRUE}.
#' @param show.info [\code{logical(1)}]\cr
#'   Verbose output on console?
#'   Default is \code{TRUE}.
#' @return \code{\linkS4class{OpenMLTask}} object.
#' @export
#' @examples
#' # Download task and access relevant information to start running experiments
#' \dontrun{
#' task = downloadOpenMLTask(id = 1)
#' show(task)
#' print(task$task.type)
#' print(task$task.target.features)
#' print(head(task$data.desc$data.set))
#' }

downloadOpenMLTask = function(id, dir = tempdir(), clean.up = TRUE,
  fetch.data.set.description = TRUE, fetch.data.set = TRUE, fetch.data.splits = TRUE, show.info = TRUE) {

  id = asInt(id)
  assertDirectory(dir, "w")
  assertFlag(fetch.data.set.description)
  assertFlag(fetch.data.set)
  assertFlag(fetch.data.splits)
  assertFlag(show.info)

  if (fetch.data.set && !fetch.data.set.description)
    stop("You cannot download a data set without also downloading the data set description!")

  fn.task = file.path(dir, "task.xml")
  fn.data.set.desc = file.path(dir, "data_set_description.xml")
  fn.data.set = file.path(dir, "data_set.ARFF")
  fn.data.splits = file.path(dir, "data_splits.ARFF")

  on.exit({
    if (clean.up) {
      unlink(fn.task)
      unlink(fn.data.set.desc)
      unlink(fn.data.set)
      unlink(fn.data.splits)
      unlink(fn.task)
      if (show.info)
        messagef("All intermediate XML and ARFF files are now removed.")
    }
  })

  if (show.info) {
    messagef("Downloading task %i from OpenML repository.", id)
    messagef("Intermediate files (XML and ARFF) will be stored in : %s", dir)
  }

  downloadAPICallFile(api.fun = "openml.tasks.search", file = fn.task, task_id = id, show.info = show.info)
  task = parseOpenMLTask(fn.task)

  if (fetch.data.set.description) {
    downloadOpenMLDataSetDescription(task$task.data.desc.id, fn.data.set.desc, show.info)
    task$task.data.desc = parseOpenMLDataSetDescription(fn.data.set.desc)
  }

  if (fetch.data.set) {
    downloadOpenMLDataSet(task$task.data.desc$url, fn.data.set, show.info)
    task$task.data.desc$data.set = parseOpenMLDataSet(task$task.data.desc, fn.data.set)

    # make valid column names
    task$task.data.desc$original.col.names = colnames(task$task.data.desc$data.set)
    task$task.data.desc$new.col.names = make.names(task$task.data.desc$original.col.names, unique=TRUE)
    target.inds = which(task$task.data.desc$original.col.names %in% task$task.target.features )
    task$task.target.features = task$task.data.desc$new.col.names[target.inds]
    colnames(task$task.data.desc$data.set) = task$task.data.desc$new.col.names
  }

  if (fetch.data.splits) {
    # No real error handling. If no data splits are available, just print a warning and go on.
    if (task$task.estimation.procedure$data.splits.url == "No URL") {
      warning("There is no URL to fetch data splits from.
        Either the task type does not support data splits or the task is defective.")
      } else {
      downloadOpenMLDataSplits(task$task.estimation.procedure$data.splits.url, fn.data.splits, show.info)
      task$task.estimation.procedure$data.splits = parseOpenMLDataSplits(task$task.data.desc$data.set, fn.data.splits)
    }
  }

  return(task)
}

parseOpenMLTask = function(file) {
  doc = parseXMLResponse(file, "Getting task", "task")

  getParams = function(path) {
    ns.parameters = getNodeSet(doc, paste(path, "oml:parameter", sep ="/"))
    parameters = lapply(ns.parameters, function(x) xmlValue(x))
    names(parameters) = sapply(ns.parameters, function(x) xmlGetAttr(x, "name"))
    parameters
  }

  # task
  task.id = xmlRValI(doc, "/oml:task/oml:task_id")
  task.type = xmlRValS(doc, "/oml:task/oml:task_type")
  targets = xmlValsMultNsS(doc, "/oml:task/oml:input/oml:data_set/oml:target_feature")
  params = getParams("oml:task")

  # data set description
  data.desc.id = xmlRValI(doc, "/oml:task/oml:input/oml:data_set/oml:data_set_id")
  data.desc = NULL

  # prediction
  ns.preds.features = getNodeSet(doc, "/oml:task/oml:output/oml:predictions/oml:feature")
  preds.features = lapply(ns.preds.features, function(x) xmlGetAttr(x, "type"))
  names(preds.features) = sapply(ns.preds.features, function(x) xmlGetAttr(x, "name"))
  task.preds = list(
    format = xmlRValS(doc, "/oml:task/oml:output/oml:predictions/oml:format"),
    features = preds.features
  )

  # estimation procedure

  data.splits.url = xmlOValS(doc, "/oml:task/oml:input/oml:estimation_procedure/oml:data_splits_url")
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
    task.id = task.id,
    task.type = task.type,
    task.target.features = targets,
    task.pars = params,
    task.data.desc.id = data.desc.id,
    task.data.desc = data.desc,
    task.estimation.procedure = estim.proc,
    task.preds = task.preds,
    task.evaluation.measures = measures
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
  p = task$task.estimation.procedure$parameters
  p = convertParam(p, "number_repeats", as.integer)
  p = convertParam(p, "number_folds", as.integer)
  task$task.estimation.procedure$parameters = p

  #task$task.evaluation.measures = strsplit(task$task.evaluation.measures, split=",")[[1]]
  return(task)
}



#FIXME: respect row id attribute
