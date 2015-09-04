#' @title Get an OpenML run.
#'
#' @description
#' This function downloads an OpenML run including all server and user computed metrics.
#'
#' @param run.id [\code{integer(1)}]\cr
#'   The run's ID.
#' @param get.predictions [\code{logical(1)}]\cr
#'   Should the associated prediction ARFF be retrieved as well?
#'   Default is \code{TRUE}.
#' @template arg_verbosity
#' @return [\code{\link{OMLRun}}]
#' @seealso To retrieve the corresponding predictions: \code{\link{getOMLPredictions}}
#' @export
getOMLRun = function(run.id, get.predictions = TRUE, verbosity = NULL) {
  id = asCount(run.id)
  assertFlag(get.predictions)

  f = findCachedRun(id)

  if (!f$description.xml$found) {
    content = doAPICall("run", id = id, file = f$description.xml$path, verbosity, method = "GET")
  } else {
    showInfo(verbosity, "Run description found in cache.")
    content = readLines(f$description.xml$path)
  }
  doc = parseXMLResponse(content, "Getting run", "run", as.text = TRUE)

  parseData = function(path) {
    # parse datasets
    path.ds = paste(path, "oml:dataset", sep ="/")
    ns.datasets = getNodeSet(doc, path.ds)
    datasets = lapply(seq_along(ns.datasets), function(i) {
      list(
        did = xmlRValR(doc, paste(path.ds, "[", i, "]/oml:did", sep='')),
        name = xmlRValS(doc, paste(path.ds, "[", i, "]/oml:name", sep='')),
        url = xmlRValS(doc, paste(path.ds, "[", i, "]/oml:url", sep=''))
      )})
    datasets = convertListOfRowsToDataFrame(datasets, strings.as.factors = FALSE)

    # parse files
    path.fls = paste(path, "oml:file", sep ="/")
    ns.fls = getNodeSet(doc, path.fls)
    files = lapply(seq_along(ns.fls), function(i) {
      list(
        did = xmlRValR(doc, paste(path.fls, "[", i, "]/oml:did", sep='')),
        name = xmlRValS(doc, paste(path.fls, "[", i, "]/oml:name", sep='')),
        url = xmlRValS(doc, paste(path.fls, "[", i, "]/oml:url", sep=''))
      )})
    files = convertListOfRowsToDataFrame(files, strings.as.factors = FALSE)

    # parse evaluations
    path.evals = paste(path, "oml:evaluation", sep ="/")
    ns.evals = getNodeSet(doc, path.evals)

    evals = rbindlist(lapply(ns.evals, function(node) {
      children = xmlChildren(node)
      row = list(
        as.integer(xmlValue(children[["did"]])),
        xmlValue(children[["name"]]),
        xmlValue(children[["flow_id"]]),
        xmlValue(children[["label"]]),
        as.numeric(xmlValue(children[["value"]])),
        as.numeric(xmlValue(children[["stdev"]])),
        xmlValue(children[["array_data"]]),
        as.integer(xmlValue(children[["sample_size"]]))
      )
      names(row) = c("did", "name", "flow_id", "label", "value", "stdev", "array.data", "sample.size")
      row
    }), fill = TRUE)
    makeOMLIOData(datasets = datasets, files = files, evaluations = as.data.frame(evals))
  }

  run.args = filterNull(list(
    run.id = xmlREValI(doc, "/oml:run/oml:run_id"),
    uploader = xmlREValI(doc, "/oml:run/oml:uploader"),
    uploader.name = xmlOValS(doc, "/oml:run/oml:uploader.name"),
    task.id = xmlREValI(doc, "/oml:run/oml:task_id"),
    task.type = xmlOValS(doc, "/oml:run/oml:task_type"),
    task.evaluation.measure = xmlOValS(doc, "/oml:task_evaluation_measure"),
    flow.id = xmlRValI(doc, "/oml:run/oml:flow_id"),
    flow.name = xmlOValS(doc, "/oml:run/oml:flow.name"),
    setup.id = xmlREValI(doc, "/oml:run/oml:setup_id"),
    setup.string = xmlOValS(doc, "/oml:run/oml:setup_string"),
    error.message = xmlOValS(doc, "/oml:run/oml:error_message"),
    tags = xmlOValsMultNsS(doc, "/oml:run/oml:tag"),
    input.data = parseData("/oml:run/oml:input_data"),
    output.data = parseData("/oml:run/oml:output_data"),
    parameter.setting = list()
  ))

  # parse parameters
  ns.pars = getNodeSet(doc, "/oml:run/oml:parameter_setting")
  run.args[["parameter.setting"]] = lapply(seq_along(ns.pars), function(i) {
    args = filterNull(list(
      name = xmlRValS(doc, paste("/oml:run/oml:parameter_setting[",i,"]/oml:name", sep='')),
      value = xmlRValS(doc, paste("/oml:run/oml:parameter_setting[",i,"]/oml:value", sep='')),
      component = xmlOValS(doc, paste("/oml:run/oml:parameter_setting[",i,"]/oml:component", sep=''))
    ))
    do.call(makeOMLRunParameter, args)
  })

  if (get.predictions) {
    f = findCachedRun(run.args$run.id)
    if (!f$predictions.arff$found) {
      fls = run.args$output.data$files
      url = fls[fls$name == "predictions", "url"]
      if (is.null(url)) {
        warning("No URL found to retrieve predictions from.")
        pred = NULL
      } else {
        pred = downloadARFF(url, f$predictions.arff$path, verbosity)
      }
    } else {
      showInfo(verbosity, "Predictions found in cache.")
      pred = arff.reader(f$predictions.arff$path)
    }
    run.args[["predictions"]] = pred
  }
  do.call(makeOMLRun, run.args)
}
