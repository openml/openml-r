#' Download OpenML run results from the server.
#'
#' @param id [\code{numeric}]\cr
#'   The task ID.
#' @template arg_ignore.cache
#' @template arg_showinfo
#' @param clean.up [\code{logical(1)}]\cr
#'   Should the downloaded run xml file be removed at the end?
#'   Default is \code{TRUE}.
#' @return [\code{\link{OpenMLRunResults}}]
#' @export
downloadOpenMLRunResults = function(id, ignore.cache = FALSE, show.info = getOpenMLOption("show.info")) {
  id = asInt(id)
  assertFlag(show.info)

  fn = file.path("runs", id, sprintf("%i.xml", id))
  url = getAPIURL("openml.run.get", run_id = id)
  # FIXME: cache?
  doc = parseXMLResponse(url, "Getting run results", "run")
  parseOpenMLRunResults(doc)
}

parseOpenMLRunResults = function(doc) {
  parseData = function(path) {
    # parse datasets
    path.ds = paste(path, "oml:dataset", sep ="/")
    ns.datasets = getNodeSet(doc, path.ds)
    datasets = list()
    for (i in seq_along(ns.datasets)) {
      args = list()
      args[["did"]] = xmlRValR(doc, paste(path.ds, "[", i, "]/oml:did", sep=''))
      args[["name"]] = xmlRValS(doc, paste(path.ds, "[", i, "]/oml:name", sep=''))
      args[["url"]] = xmlRValS(doc, paste(path.ds, "[", i, "]/oml:url", sep=''))
      datasets = c(datasets, args)
    }

    # parse evaluations
    path.evals = paste(path, "oml:evaluation", sep ="/")
    ns.evals = getNodeSet(doc, path.evals)
    evals = vector("list", length(ns.evals))
    for (i in seq_along(ns.evals)) {
      args = list()
      args[["did"]] = xmlOValR(doc, paste(path.evals, "[", i, "]/oml:did", sep=''))
      args[["name"]] = xmlRValS(doc, paste(path.evals, "[", i, "]/oml:name", sep=''))
      args[["implementation"]] = xmlRValR(doc, paste(path.evals, "[", i, "]/oml:implementation", sep=''))
      args[["value"]] = xmlOValR(doc, paste(path.evals, "[", i, "]/oml:value", sep=''))
      args[["array.data"]] = xmlOValS(doc, paste(path.evals, "[", i, "]/oml:array_data", sep=''))
      evals[[i]] = args
    }
    evals = do.call(rbind.fill, lapply(evals, as.data.frame))
    return(do.call(makeOpenMLIOData, list(dataset = datasets, evaluation = evals)))
  }

  run.args = list()

  run.args[["run.id"]] = xmlREValI(doc, "/oml:run/oml:run_id")
  run.args[["uploader"]] = xmlREValI(doc, "/oml:run/oml:uploader")
  run.args[["task.id"]] = xmlREValI(doc, "/oml:run/oml:task_id")
  run.args[["implementation.id"]] = xmlRValS(doc, "/oml:run/oml:implementation_id")
  run.args[["setup.id"]] = xmlREValI(doc, "/oml:run/oml:setup_id")
  run.args[["setup.string"]] = xmlOValS(doc, "/oml:run/oml:setup_string")
  run.args[["error.message"]] = xmlOValS(doc, "/oml:run/oml:error_message")

  # parse parameters
  ns.pars = getNodeSet(doc, "/oml:run/oml:parameter_setting")
  par.set = vector("list", length = length(ns.pars))
  for (i in seq_along(ns.pars)) {
    args = list()
    args[["name"]] = xmlRValS(doc, paste("/oml:run/oml:parameter_setting[",i,"]/oml:name", sep=''))
    args[["value"]] = xmlRValS(doc, paste("/oml:run/oml:parameter_setting[",i,"]/oml:value", sep=''))
    args[["component"]] = xmlOValS(doc, paste("/oml:run/oml:parameter_setting[",i,"]/oml:component", sep=''))
    par.set[[i]] = do.call(makeOpenMLRunParameter, args)
  }
  run.args[["parameter.setting"]] = par.set

  run.args[["input.data"]] = parseData("/oml:run/oml:input_data")
  run.args[["output.data"]] = parseData("/oml:run/oml:output_data")

  return(do.call(makeOpenMLRunResults, run.args))
}
