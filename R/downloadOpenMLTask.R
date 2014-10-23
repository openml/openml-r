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
#' @template arg_ignore.cache
#' @template arg_verbosity
#' @return [\code{\link{OpenMLTask}}]
#' @export
#' @examples
#' # Download task and access relevant information to start running experiments
#' \dontrun{
#' task = downloadOpenMLTask(id = 1)
#' show(task)
#' print(task$type)
#' print(task$target.features)
#' print(head(task$data.set$data))
#' }
downloadOpenMLTask = function(id, ignore.cache = FALSE, verbosity = NULL) {
  id = asInt(id)

  showInfo(verbosity, "Downloading task '%i' from OpenML repository.", id)

  f = findInCacheTask(id, create = TRUE)

  # get XML description
  if (!f$found || ignore.cache) {
    url = getAPIURL("openml.task.get", task_id = id)
    path = getCacheTaskPath(id, "task.xml")
    task.contents = downloadXML(url, path, verbosity)
  } else {
    showInfo(verbosity, "Found in cache.")
    task.contents = readLines(getCacheTaskPath(id, "task.xml"))
  }
  task.xml = parseXMLResponse(task.contents, "Getting task", "task", as.text = TRUE)
  task = parseOpenMLTask(task.xml)

  # this goes through cache
  ds = downloadOMLDataSet(task$data.desc.id, ignore.cache, verbosity)
  task$data.set = ds
  
  # No real error handling. If no data splits are available, just print a warning and go on.
  if (task$estimation.procedure$data.splits.url == "No URL") {
    warning("There is no URL to fetch data splits from.\nEither the task type does not support data splits or the task is defective.")
  } else {
    task$estimation.procedure$data.splits = downloadOpenMLDataSplits(task, ignore.cache, verbosity)
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
  data.set = NULL

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
    data.set = data.set,
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

# download data splits file from URL to local file
downloadOpenMLDataSplits = function(task, ignore.cache = FALSE, verbosity = NULL) {
  id = task$id
  url = task$estimation.procedure$data.splits.url
  fn = getCacheTaskPath(id, "datasplits.arff")
  data = downloadARFF(url, fn, verbosity)
  parseOpenMLDataSplits(task, data)
}

parseOpenMLDataSplits = function(task, data) {
  # slightly converts the splits data frame
  # rename the "repeat" column to "rep" + and make all indices 1-based, they are 0-based on the server
  colnames(data)[colnames(data) == "repeat"] = "rep"
  ri = data$rowid
  rns = rownames(task$data.set$data)
  # FIXME: use match()!
  data$rowid = sapply(ri, function(x) which(x == rns))
  data$rep = data$rep + 1
  data$fold = data$fold + 1
  return(data)
}

