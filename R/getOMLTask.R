#' @title Get an OpenML task.
#'
#' @description
#' This function downloads an OpenML task and all associated files from the OpenML repository,
#' caches the files on disk and creates an S3 object which completely specifies the task. Note that
#' the associated data set is not downloaded, only some information describing it. To retrieve the
#' whole \code{OMLDataSet} object, please use \code{\link{getOMLDataSet}}.
#'
#' @param id [\code{integer(1)}]\cr
#'   The task ID.
#' @template arg_hash
#' @template arg_verbosity
#' @return [\code{\link{OMLTask}}]
#' @seealso To retrieve the corresponding data set: \code{\link{getOMLDataSet}}
#' @export
#' @examples
#' # Download task and access relevant information to start running experiments
#' \dontrun{
#' task = getOMLTask(id = 1)
#' show(task)
#' print(task$type)
#' print(task$target.features)
#' print(head(task$data.set$data))
#' task$data.set = getOMLDataSet(task)
#' print(head(task$data.set$data))
#' }
getOMLTask = function(id, session.hash = getSessionHash(), verbosity = NULL) {
  id = asCount(id)

  showInfo(verbosity, "Downloading task '%i' from OpenML repository.", id)

  f = findInCacheTask(id, create = TRUE)

  # get XML description
  if (!f$task.found) {
    url = getAPIURL("openml.task.get", task_id = id)
    path = getCacheTaskPath(id, "task.xml")
    task.contents = downloadXML(url, path, verbosity, session_hash = session.hash)
  } else {
    showInfo(verbosity, "Task XML found in cache.")
    task.contents = readLines(getCacheTaskPath(id, "task.xml"))
  }
  doc = parseXMLResponse(task.contents, "Getting task", "task", as.text = TRUE)

  # parsing helper
  getParams = function(path) {
    ns.parameters = getNodeSet(doc, paste(path, "oml:parameter", sep ="/"))
    parameters = lapply(ns.parameters, function(x) xmlValue(x))
    names(parameters) = sapply(ns.parameters, function(x) xmlGetAttr(x, "name"))
    parameters
  }

  # parse task
  id = xmlRValI(doc, "/oml:task/oml:task_id")
  type = xmlRValS(doc, "/oml:task/oml:task_type")
  targets = xmlValsMultNsS(doc, "/oml:task/oml:input/oml:data_set/oml:target_feature")
  params = getParams("oml:task")
  tags = xmlOValsMultNsS(doc, "/oml:run/oml:tag", NA_character_)

  # parse data set description
  data.desc.id = xmlRValI(doc, "/oml:task/oml:input/oml:data_set/oml:data_set_id")
  data.set = NULL

  # parse prediction info
  ns.preds.features = getNodeSet(doc, "/oml:task/oml:output/oml:predictions/oml:feature")
  preds.features = lapply(ns.preds.features, function(x) xmlGetAttr(x, "type"))
  names(preds.features) = sapply(ns.preds.features, function(x) xmlGetAttr(x, "name"))
  preds = list(
    format = xmlRValS(doc, "/oml:task/oml:output/oml:predictions/oml:format"),
    features = preds.features
  )

  # parse estimation procedure
  # trim white space around URL to be a bit more robust
  data.splits.url = str_trim(xmlOValS(doc, "/oml:task/oml:input/oml:estimation_procedure/oml:data_splits_url"))
  if (is.null(data.splits.url))
    data.splits.url = "No URL"

  estim.proc = makeOMLEstimationProcedure(
    type = xmlRValS(doc, "/oml:task/oml:input/oml:estimation_procedure/oml:type"),
    data.splits.url = data.splits.url,
    data.splits = data.frame(),
    parameters = getParams("/oml:task/oml:input/oml:estimation_procedure")
  )

  # parse measures
  measures = xmlValsMultNsS(doc, "/oml:task/oml:input/oml:evaluation_measures/oml:evaluation_measure")

  task = makeOMLTask(
    id = id,
    type = type,
    target.features = targets,
    tags = tags,
    pars = params,
    data.desc.id = data.desc.id,
    data.set = data.set,
    estimation.procedure = estim.proc,
    preds = preds,
    evaluation.measures = measures
  )
  # convert estim params to correct types
  p = task$estimation.procedure$parameters
  if (!is.null(p[["number_repeats"]]))
    p[["number_repeats"]] = as.integer(p[["number_repeats"]])
  if (!is.null(p[["number_folds"]]))
    p[["number_folds"]] = as.integer(p[["number_folds"]])
  task$estimation.procedure$parameters = p

  # this is temporarily needed to parse the data splits
  task$data.set = getOMLDataSet(task)

  # No real error handling. If no data splits are available, just print a warning and go on.
  if (!f$datasplits.found) {
    url.dsplits = task$estimation.procedure$data.splits.url
    if (url.dsplits == "No URL") {
      warning("There is no URL to fetch data splits from.\nEither the task type does not support data splits or the task is defective.")
    } else {
      fn.dsplits = getCacheTaskPath(task$id, "datasplits.arff")
      data = downloadARFF(url.dsplits, fn.dsplits, verbosity)
      task$estimation.procedure$data.splits = parseOMLDataSplits(task, data)
    }
  } else {
    showInfo(verbosity, "Data splits found in cache.")
    data = read.arff(getCacheTaskPath(id, "datasplits.arff"))
    task$estimation.procedure$data.splits = parseOMLDataSplits(task, data)
  }
  task$data.set$data = NULL
  return(task)
}

parseOMLDataSplits = function(task, data) {
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
