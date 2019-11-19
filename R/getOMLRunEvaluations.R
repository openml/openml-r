# https://test.openml.org/api/v1/evaluation/setup/list/flow/2117/task/403/limit/100 works, while https://test.openml.org/api/v1/evaluation/setup/list/flow/6794 returns nothing.

# Result size limits are okay, as long as I can somehow reliably iterate with the offset, i.e.
# https://test.openml.org/api/v1/evaluation/setup/list/flow/2117/task/403/function/predictive_accuracy/limit/1/offset/2



# #' @title Get performances and hyperparameters for flows or tasks
# #'
# #' @description
# #' Given an run id, the corresponding \code{\link{OMLRun}} including all server
# #' and user computed metrics is downloaded if not already available in cache.
# #'
# #' @param flow.id [\code{integer(1)}]\cr
# #'   The flow ID.
# #' @param task.id [\code{integer(1)}]\cr
# #'   The task ID.
# #' @param eval_measure [\code{character}]
# #' @template arg_cache_only
# #' @param only.xml [\code{logical(1)}]\cr
# #'   Should only the XML be downloaded?
# #' @template arg_verbosity
# #' @return [\code{\link{OMLRun}}].
# #' @family downloading functions
# #' @family run-related functions
# #' @example inst/examples/getOMLRun.R
# #' @export
# getOMLRun = function(run.id, cache.only = FALSE, only.xml = FALSE, verbosity = NULL) {
#   id = asCount(run.id)
#   assertFlag(cache.only)

#   down = downloadOMLObject(id, object = "run", cache.only = cache.only, only.xml = only.xml, verbosity = verbosity)
#   f = down$files
#   doc = down$doc

#   run.args = filterNull(list(
#     run.id = xmlREValI(doc, "/oml:run/oml:run_id"),
#     uploader = xmlREValI(doc, "/oml:run/oml:uploader"),
#     uploader.name = xmlOValS(doc, "/oml:run/oml:uploader.name"),
#     task.id = xmlREValI(doc, "/oml:run/oml:task_id"),
#     task.type = xmlOValS(doc, "/oml:run/oml:task_type"),
#     task.evaluation.measure = xmlOValS(doc, "/oml:run/oml:task_evaluation_measure"),
#     flow.id = xmlRValI(doc, "/oml:run/oml:flow_id"),
#     flow.name = xmlOValS(doc, "/oml:run/oml:flow_name"),
#     setup.id = xmlREValI(doc, "/oml:run/oml:setup_id"),
#     setup.string = xmlOValS(doc, "/oml:run/oml:setup_string"),
#     error.message = xmlOValS(doc, "/oml:run/oml:error_message"),
#     tags = xmlOValsMultNsS(doc, "/oml:run/oml:tag"),
#     input.data = parseData(doc, "/oml:run/oml:input_data"),
#     output.data = parseData(doc, "/oml:run/oml:output_data"),
#     parameter.setting = list()
#   ))

#   # parse parameters
#   ns.pars = getNodeSet(doc, "/oml:run/oml:parameter_setting")
#   run.args[["parameter.setting"]] = lapply(seq_along(ns.pars), function(i) {
#     args = filterNull(list(
#       name = xmlRValS(doc, paste("/oml:run/oml:parameter_setting[", i, "]/oml:name", sep = "")),
#       value = xmlRValS(doc, paste("/oml:run/oml:parameter_setting[", i, "]/oml:value", sep = "")),
#       component = xmlOValS(doc, paste("/oml:run/oml:parameter_setting[", i, "]/oml:component", sep = ""))
#     ))
#     do.call(makeOMLRunParameter, args)
#   })
#   par.names = vcapply(run.args[["parameter.setting"]], function(x) x$name)
#   run.args[["parameter.setting"]] = setNames(run.args[["parameter.setting"]], par.names)
#   #setClasses(run.args[["parameter.setting"]], "OMLRunParList")

#   # get the predictions
#   f = findCachedRun(run.args$run.id)

#   if (!f$predictions.arff$found) {
#     showInfo(verbosity, "No ARFF file containing the predictions found.")
#     pred = NULL
#   } else {
#     #showInfo(verbosity, "Predictions found in cache.")
#     pred = arff.reader(f$predictions.arff$path)
#   }
#   run.args[["predictions"]] = pred

#   return(do.call(makeOMLRun, run.args))
# }

# parseData = function(doc, path) {
#   # parse datasets
#   path.ds = paste(path, "oml:dataset", sep = "/")
#   ns.datasets = getNodeSet(doc, path.ds)
#   datasets = lapply(seq_along(ns.datasets), function(i) {
#     list(
#       data.id = xmlRValR(doc, paste(path.ds, "[", i, "]/oml:did", sep = "")),
#       name = xmlRValS(doc, paste(path.ds, "[", i, "]/oml:name", sep = "")),
#       url = xmlRValS(doc, paste(path.ds, "[", i, "]/oml:url", sep = ""))
#     )})
#   datasets = convertListOfRowsToDataFrame(datasets, strings.as.factors = FALSE)

#   # parse files
#   path.fls = paste(path, "oml:file", sep = "/")
#   ns.fls = getNodeSet(doc, path.fls)
#   files = lapply(seq_along(ns.fls), function(i) {
#     list(
#       data.id = xmlRValR(doc, paste(path.fls, "[", i, "]/oml:did", sep = "")),
#       name = xmlRValS(doc, paste(path.fls, "[", i, "]/oml:name", sep = "")),
#       url = xmlRValS(doc, paste(path.fls, "[", i, "]/oml:url", sep = ""))
#     )})
#   files = convertListOfRowsToDataFrame(files, strings.as.factors = FALSE)

#   # parse evaluations
#   path.evals = paste(path, "oml:evaluation", sep = "/")
#   ns.evals = getNodeSet(doc, path.evals)

#   evals = setDF(rbindlist(lapply(ns.evals, function(node) {
#     children = xmlChildren(node)
#     row = list(
#       as.integer(xmlValue(children[["did"]])),
#       xmlValue(children[["name"]]),
#       xmlValue(children[["flow_id"]]),
#       xmlValue(children[["label"]]),
#       as.numeric(xmlValue(children[["value"]])),
#       as.numeric(xmlValue(children[["stdev"]])),
#       xmlValue(children[["array_data"]]),
#       as.integer(xmlValue(children[["sample_size"]]))
#     )
#     cv.info = xmlAttrs(node)[c("repeat", "fold")]
#     if (is.null(cv.info)) cv.info = c(NA, NA)
#     row = c(row, cv.info)
#     names(row) = c("data.id", "name", "flow_id", "label", "value", "stdev", "array.data", "sample.size", "repeat", "fold")
#     row
#   }), fill = TRUE))
#   makeOMLIOData(datasets = datasets, files = files, evaluations = evals)
# }
