#' @title Get an OpenML task.
#'
#' @description
#' Given a task ID, the corresponding \code{\link{OMLTask}} will be downloaded
#' (if not in cache) and returned.
#'
#' @template arg_task.id
#' @template arg_cache_only
#' @template arg_verbosity
#' @return [\code{\link{OMLTask}}].
#' @family downloading functions
#' @family task-related functions
#' @export
#' @example inst/examples/getOMLTask.R
getOMLTask = function(task.id, cache.only = FALSE, verbosity = NULL) {
  id = asCount(task.id)
  assertFlag(cache.only)
  #showInfo(verbosity, "Downloading task '%i' from OpenML repository.", id)

  # get XML description
  down = downloadOMLObject(id, object = "task", cache.only = cache.only, verbosity = verbosity)
  doc = down$doc
  f = down$files

  # parsing helper
  task = parseOMLTask(doc, cache.only = cache.only, verbosity = verbosity)
  task.type = task$task.type
  targets = xmlValsMultNsS(doc, "/oml:task/oml:input/oml:data_set/oml:target_feature")
  if (length(targets) == 0) {
    notEmpty = function(string) {
      if (is.null(string) || string == "")
        return(NULL)
      return(string)
    }
    targets = c(notEmpty(xmlOValS(doc, "/oml:task/oml:input/oml:data_set/oml:target_feature_left")),
                notEmpty(xmlOValS(doc, "/oml:task/oml:input/oml:data_set/oml:target_feature_right")),
                notEmpty(xmlOValS(doc, "/oml:task/oml:input/oml:data_set/oml:target_feature_event")))
  }

  # convert estim params to correct types
  p = task$input$estimation.procedure$parameters
  if (!is.null(p[["number_repeats"]]))
    p[["number_repeats"]] = as.integer(p[["number_repeats"]])
  if (!is.null(p[["number_folds"]]))
    p[["number_folds"]] = as.integer(p[["number_folds"]])
  task$input$estimation.procedure$parameters = p

  # replace targets with new column names
  targets = task$input$data.set$colnames.new[unlist(lapply(targets, function(x) which(x == task$input$data.set$colnames.old)))]
  task$input$target.features = targets

  if (task.type == "Supervised Classification") {
    if (!is.factor(task$input$data.set$data[, targets])) {
      showInfo(verbosity, "Target column not a factor. Converting and going on.")
      task$input$data.set$data[, targets] = as.factor(as.character(task$input$data.set$data[, targets]))
    }
  } else if (task.type == "Supervised Regression") {
    task$input$data.set$data[, targets] = as.numeric(task$input$data.set$data[, targets])
  }

  # FIXME: What should happen if no data splits are available?
  # No real error handling. If no data splits are available, just print a warning and go on.
  url.dsplits = task$input$estimation.procedure$data.splits.url
  if (url.dsplits != "No URL") {
    # FIXME: see https://github.com/openml/website/issues/25 when this is solved, we might change this line:
    data = suppressWarnings(arff.reader(f$datasplits.arff$path))
      #tryCatch(suppressWarnings(arff.reader(f$datasplits.arff$path)), error = function(e) NULL)
    #if (!is.null(data))
    task$input$estimation.procedure$data.splits = parseOMLDataSplits(task, data)
  } #else warning("Task not providing datasplits.")

  return(task)
}

parseOMLTask = function(doc, verbosity = NULL, cache.only = FALSE) {
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
  tags = xmlOValsMultNsS(doc, "/oml:task/oml:tag", NA_character_)
  data.set.output = filterNull(list(data.set.id = xmlOValI(doc, "/oml:task/oml:output/oml:data_set/oml:data_set_id"),
                                    target.features = xmlOValsMultNsS(doc, "/oml:task/oml:output/oml:data_set/oml:target_feature")))
  if (length(data.set.output) == 0)
    data.set.output = NULL
  # parse estimation procedure
  # trim white space around URL to be a bit more robust
  data.splits.url = xmlOValS(doc, "/oml:task/oml:input/oml:estimation_procedure/oml:data_splits_url")
  data.splits.url = ifelse(is.null(data.splits.url), "No URL", stri_trim_both(data.splits.url))

  estim.proc = makeOMLEstimationProcedure(
    type = xmlRValS(doc, "/oml:task/oml:input/oml:estimation_procedure/oml:type"),
    data.splits.url = data.splits.url,
    data.splits = data.frame(),
    parameters = getParams("/oml:task/oml:input/oml:estimation_procedure")
  )

  # get the data set
  data.set.input = getOMLDataSet(xmlRValI(doc, "/oml:task/oml:input/oml:data_set/oml:data_set_id"),
                                 verbosity = verbosity, cache.only = cache.only)

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

  makeOMLTask(
    task.id = task.id,
    task.type = task.type,
    input = input,
    parameters = parameters,
    output = output,
    tags = tags
  )
}

parseOMLDataSplits = function(task, data) {
  # slightly converts the splits data frame
  # rename the "repeat" column to "rep" + and make all indices 1-based, they are 0-based on the server
  colnames(data)[colnames(data) == "repeat"] = "rep"
  # FIXME: use match()!
  #ri = data$rowid
  #rns = rownames(task$input$data.set$data)
  #data$rowid = as.vector(sapply(ri, function(x) which(x == rns)))
  #data$rowid = match(ri, rns)
  # FIXME: even match() is too slow for big data sets...
  #   The unit test in getOMLTask suggests taht we can use this instead (no need to use task in function-arg):
  rowid = if (min(data$rowid) == 0) (data$rowid+1) else data$rowid
  data$rowid = as.integer(rowid)
  data$rep = data$rep + 1
  data$fold = data$fold + 1
  return(data)
}
