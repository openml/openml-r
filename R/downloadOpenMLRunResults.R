downloadOpenMLRunResults <- function(id, dir = getwd(), show.info = TRUE) {
  fn.get.run <- file.path(dir, sprintf("get_run_%g.xml", id))
  downloadAPICallFile(api.fun = "openml.run.get", file = fn.get.run, run_id = id, show.info = show.info)
  results <- parseOpenMLRunResults(fn.get.run)
  return(results)
}

parseOpenMLRunResults <- function(file) {
  doc <- parseXMLResponse(file, "Getting run results", "run")

  parseData <- function(path) {
    # parse datasets
    path.ds <- paste(path, "oml:dataset", sep ="/")
    ns.datasets <- getNodeSet(doc, path.ds)
    datasets <- list()
    for (i in seq_along(ns.datasets)) {
      args <- list()
      args[["did"]] <- xmlRValR(doc, paste(path.ds, "[", i, "]/oml:did", sep=''))
      args[["name"]] <- xmlRValS(doc, paste(path.ds, "[", i, "]/oml:name", sep=''))
      args[["url"]] <- xmlRValS(doc, paste(path.ds, "[", i, "]/oml:url", sep=''))
      datasets <- c(datasets, args)
    }
    
    # parse evaluations
    # FIXME: make this more beautiful
    path.evals <- paste(path, "oml:evaluation", sep ="/")
    ns.evals <- getNodeSet(doc, path.evals)
    evals <- list()
    for (i in seq_along(ns.evals)) {
      args <- list()
      args[["did"]] <- xmlOValR(doc, paste(path.evals, "[", i, "]/oml:did", sep=''))
      args[["name"]] <- xmlRValS(doc, paste(path.evals, "[", i, "]/oml:name", sep=''))
      args[["implementation"]] <- xmlRValR(doc, paste(path.evals, "[", i, "]/oml:implementation", sep=''))
      args[["value"]] <- xmlOValR(doc, paste(path.evals, "[", i, "]/oml:value", sep=''))
      args[["array.data"]] <- xmlOValS(doc, paste(path.evals, "[", i, "]/oml:array_data", sep=''))
      evals <- c(evals, list(args))
    }
    
    return(do.call(OpenMLData, list(dataset = datasets, evaluation = evals)))
  }
  
  run.args <- list()
  
  run.args[["run.id"]] <- xmlREValI(doc, "/oml:run/oml:run_id")
  run.args[["uploader"]] <- xmlREValI(doc, "/oml:run/oml:uploader")
  run.args[["task.id"]] <- xmlREValI(doc, "/oml:run/oml:task_id")
  run.args[["implementation.id"]] <- xmlRValS(doc, "/oml:run/oml:implementation_id")
  run.args[["setup.id"]] <- xmlREValI(doc, "/oml:run/oml:setup_id")
  run.args[["error.message"]] <- xmlOValS(doc, "/oml:run/oml:error_message")
  
  # parse parameters
  par.set <- list()
  ns.pars <- getNodeSet(doc, "/oml:run/oml:parameter_setting")
  for (i in seq_along(ns.pars)) {
    args <- list()
    args[["name"]] <- xmlRValS(doc, paste("/oml:run/oml:parameter_setting[",i,"]/oml:name", sep=''))
    args[["value"]] <- xmlRValS(doc, paste("/oml:run/oml:parameter_setting[",i,"]/oml:value", sep=''))
    args[["component"]] <- xmlOValS(doc, paste("/oml:run/oml:parameter_setting[",i,"]/oml:component", sep=''))
    par.set <- c(par.set, do.call(OpenMLRunParameter, args))
  }
  run.args[["parameter.setting"]] <- par.set
  
  run.args[["input.data"]] <- parseData("/oml:run/oml:input_data")
  run.args[["output.data"]] <- parseData("/oml:run/oml:output_data")
  
  return(do.call(OpenMLRunResults, run.args))
}