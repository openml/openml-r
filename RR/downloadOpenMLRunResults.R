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
    datasets = lapply(seq_along(ns.datasets), function(i) {
      list(
        did = xmlRValR(doc, paste(path.ds, "[", i, "]/oml:did", sep='')),
        name = xmlRValS(doc, paste(path.ds, "[", i, "]/oml:name", sep='')),
        url = xmlRValS(doc, paste(path.ds, "[", i, "]/oml:url", sep=''))
    )})

    # parse evaluations
    path.evals = paste(path, "oml:evaluation", sep ="/")
    ns.evals = getNodeSet(doc, path.evals)
    evals = lapply(seq_along(ns.evals), function(i) {
      data.frame(
        did = xmlOValR(doc, paste(path.evals, "[", i, "]/oml:did", sep='')),
        name = xmlRValS(doc, paste(path.evals, "[", i, "]/oml:name", sep='')),
        implementation = xmlRValR(doc, paste(path.evals, "[", i, "]/oml:implementation", sep='')),
        value = xmlOValR(doc, paste(path.evals, "[", i, "]/oml:value", sep='')),
        array.data = xmlOValS(doc, paste(path.evals, "[", i, "]/oml:array_data", sep=''))
      )
    })
    makeOpenMLIOData(dataset = datasets, evaluation = do.call(rbind.fill, evals))
  }

  run.args = filterNull(list(
    run.id = xmlREValI(doc, "/oml:run/oml:run_id"),
    uploader = xmlREValI(doc, "/oml:run/oml:uploader"),
    task.id = xmlREValI(doc, "/oml:run/oml:task_id"),
    implementation.id = xmlRValS(doc, "/oml:run/oml:implementation_id"),
    setup.id = xmlREValI(doc, "/oml:run/oml:setup_id"),
    setup.string = xmlOValS(doc, "/oml:run/oml:setup_string"),
    error.message = xmlOValS(doc, "/oml:run/oml:error_message"),
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
    do.call(makeOpenMLRunParameter, args)
  })

  do.call(makeOpenMLRunResults, run.args)
}
