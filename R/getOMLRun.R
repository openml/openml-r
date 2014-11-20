#' @title Get an OpenML run.
#'
#' @description
#' This function downloads an OpenML run including all server and user computed metrics.
#' The associated predictions can be retrieved with \code{\link{getOMLPredictions}}.
#'
#' @param id [\code{integer(1)}]\cr
#'   The task ID.
#' @template arg_hash
#' @template arg_verbosity
#' @return [\code{\link{OMLRun}}]
#' @seealso To retrieve the corresponding predictions: \code{\link{getOMLPredictions}}
#' @export
getOMLRun = function(id, session.hash = getSessionHash(), verbosity = NULL) {
  id = asCount(id)

  url = getAPIURL("openml.run.get", run_id = id)
  content = downloadXML(url, NULL, verbosity, session_hash = session.hash)
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
        xmlValue(children[["implementation"]]),
        xmlValue(children[["label"]]),
        as.numeric(xmlValue(children[["value"]])),
        as.numeric(xmlValue(children[["stdev"]])),
        xmlValue(children[["array_data"]]),
        as.integer(xmlValue(children[["sample_size"]]))
      )
      names(row) = c("did", "name", "impl", "label", "value", "stdev", "array.data", "sample.size")
      row
    }), fill = TRUE)
    makeOMLIOData(datasets = datasets, files = files, evaluations = as.data.frame(evals))
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
    do.call(makeOMLRunParameter, args)
  })

  # parse tags
  run.args[["tags"]] = xmlOValsMultNsS(doc, "/oml:run/oml:tag")

  do.call(makeOMLRun, run.args)
}

#' @title Construct OMLRunParameter
#'
#' @param name [\code{character(1)}]\cr
#'    The name of the parameter.
#' @param value [\code{character(1)}]\cr
#'    The value of the parameter.
#' @param component [\code{character(1)}]\cr
#'    The implementation name of a component, if the parameter belongs to this component.
#'    This name must match a component of the implementation.
#'
#' @export
#' @aliases OMLRunParameter
makeOMLRunParameter = function(name, value, component = NA_character_) {
  assertString(name)
  assertString(value)
  assertString(component, na.ok = TRUE)

  makeS3Obj("OMLRunParameter",
            name = name,
            value = value,
            component = component
  )
}

# show
#' @export
print.OMLRunParameter = function(x, ...)  {
  if (!is.na(x$component))
    s = sprintf(' (parameter of component %s)', x$component)
  else
    s = ""
  catf("%s %s = %s", s, x$name, x$value)
}