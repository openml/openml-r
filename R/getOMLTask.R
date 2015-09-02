#' @title Get an OpenML task.
#'
#' @description
#' This function downloads an OpenML task and all associated files from the OpenML repository,
#' caches the files on disk and creates an S3 object which completely specifies the task.
#'
#' @template arg_task_id
#' @template arg_verbosity
#' @return [\code{\link{OMLTask}}]
#' @export
#' @examples
#' # Download task and access relevant information to start running experiments
#' \dontrun{
#' task = getOMLTask(1)
#' print(task)
#' print(task$task.type)
#' print(task$input$data.set)
#' print(head(task$input$data.set$data))
#' }
getOMLTask = function(task.id, verbosity = NULL) {
  id = asCount(task.id)
  showInfo(verbosity, "Downloading task '%i' from OpenML repository.", id)

  f = findCachedTask(id)

  # get XML description
  if (!f$task.xml$found) {
    url = getAPIURL("task", get.arg = id)
    task.contents = downloadXML(url, f$task.xml$path,
      verbosity, post = FALSE)
  } else {
    showInfo(verbosity, "Task XML found in cache.")
    task.contents = readLines(f$task.xml$path)
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
  task.id = xmlRValI(doc, "/oml:task/oml:task_id")
  task.type = xmlRValS(doc, "/oml:task/oml:task_type")
  parameters = getParams("oml:task")
  tags = xmlOValsMultNsS(doc, "/oml:run/oml:tag", NA_character_)
  data.set.output = filterNull(list(data.set.id = xmlOValI(doc, "/oml:task/oml:output/oml:data_set/oml:data_set_id"),
    target.features = xmlOValsMultNsS(doc, "/oml:task/oml:output/oml:data_set/oml:target_feature")))
  if (length(data.set.output) == 0)
    data.set.output = NULL
  # parse estimation procedure
  # trim white space around URL to be a bit more robust
  data.splits.url = stri_trim_both(xmlOValS(doc, "/oml:task/oml:input/oml:estimation_procedure/oml:data_splits_url"))
  if (is.null(data.splits.url))
    data.splits.url = "No URL"

  estim.proc = makeOMLEstimationProcedure(
    type = xmlRValS(doc, "/oml:task/oml:input/oml:estimation_procedure/oml:type"),
    data.splits.url = data.splits.url,
    data.splits = data.frame(),
    parameters = getParams("/oml:task/oml:input/oml:estimation_procedure")
  )

  # get the data set
  targets = xmlValsMultNsS(doc, "/oml:task/oml:input/oml:data_set/oml:target_feature")
  if (length(targets) == 0) {
    notEmpty = function(string) {
      if (string == "")
        return(NULL)
      return(string)
    }
    targets = c(notEmpty(xmlOValS(doc, "/oml:task/oml:input/oml:data_set/oml:target_feature_left")),
      notEmpty(xmlOValS(doc, "/oml:task/oml:input/oml:data_set/oml:target_feature_right")),
      notEmpty(xmlOValS(doc, "/oml:task/oml:input/oml:data_set/oml:target_feature_event")))
  }

  data.set.input = getOMLDataSet(xmlRValI(doc, "/oml:task/oml:input/oml:data_set/oml:data_set_id"), verbosity = verbosity)

  input = list(
    data.set = data.set.input,
    estimation.procedure = estim.proc,
    evaluation.measures = xmlValsMultNsS(doc, "/oml:task/oml:input/oml:evaluation_measures/oml:evaluation_measure"),
    cost.matrix = xmlOValS(doc, "/oml:task/oml:input/oml:cost_matrix")
  )

  # parse prediction info
  ns.preds.features = getNodeSet(doc, "/oml:task/oml:output/oml:predictions/oml:feature")
  preds.features = lapply(ns.preds.features, function(x) xmlGetAttr(x, "type"))
  names(preds.features) = sapply(ns.preds.features, function(x) xmlGetAttr(x, "name"))
  preds = list(
    format = xmlRValS(doc, "/oml:task/oml:output/oml:predictions/oml:format"),
    features = preds.features
  )

  output = list(
    data.set = data.set.output,
    predictions = preds
  )

  task = makeOMLTask(
    task.id = task.id,
    task.type = task.type,
    input = input,
    parameters = parameters,
    output = output,
    tags = tags
  )
  # convert estim params to correct types
  p = task$input$estimation.procedure$parameters
  if (!is.null(p[["number_repeats"]]))
    p[["number_repeats"]] = as.integer(p[["number_repeats"]])
  if (!is.null(p[["number_folds"]]))
    p[["number_folds"]] = as.integer(p[["number_folds"]])
  task$input$estimation.procedure$parameters = p

  # replace targets with new column names
  targets = task$input$data.set$colnames.new[unlist(lapply(targets, function(x) which(x == task$input$data.set$colnames.old)))]
  task$input$data.set$target.features = targets

  if (task.type == "Supervised Classification") {
    if (!is.factor(task$input$data.set$data[, targets])) {
      showInfo(verbosity, "Target column not a factor. Converting and going on.")
      task$input$data.set$data[, targets] = as.factor(as.character(task$input$data.set$data[, targets]))
    }
  } else if (task.type == "Supervised Regression") {
    task$input$data.set$data[, targets] = as.numeric(task$input$data.set$data[, targets])
  }
  # No real error handling. If no data splits are available, just print a warning and go on.
  if (!f$datasplits.arff$found) {
    url.dsplits = task$input$estimation.procedure$data.splits.url
    if (url.dsplits == "No URL") {
      warning("There is no URL to fetch data splits from.\nEither the task type does not support data splits or the task is defective.")
    } else {
      data = downloadARFF(url.dsplits, f$datasplits.arff$path, verbosity)
      task$input$estimation.procedure$data.splits = parseOMLDataSplits(task, data)
    }
  } else {
    showInfo(verbosity, "Data splits found in cache.")
    data = arff.reader(f$datasplits.arff$path)
    task$input$estimation.procedure$data.splits = parseOMLDataSplits(task, data)
  }
  return(task)
}

parseOMLDataSplits = function(task, data) {
  # slightly converts the splits data frame
  # rename the "repeat" column to "rep" + and make all indices 1-based, they are 0-based on the server
  colnames(data)[colnames(data) == "repeat"] = "rep"
  ri = data$rowid
  rns = rownames(task$input$data.set$data)
  # FIXME: use match()!
  data$rowid = as.vector(sapply(ri, function(x) which(x == rns)))
  data$rep = data$rep + 1
  data$fold = data$fold + 1
  return(data)
}
