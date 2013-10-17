downloadOpenMLRunResults <- function(id, dir = getwd(), show.info = TRUE) {
  fn.get.run <- file.path(dir, sprintf("get_run_%g.xml", id))
  downloadAPICallFile(api.fun = "openml.run.get", file = fn.get.run, run_id = id, show.info = show.info)
  results <- parseOpenMLRunResults(fn.get.run)
  return(results)
}

parseOpenMLRunResults <- function(file) {
  doc <- parseXMLResponse(file, "Getting run results", "get_run")
  getMetrics <- function(path) {
    ns.names <- getNodeSet(doc, paste(path, "oml:metric/oml:name", sep ="/"))
    metric.names <- lapply(ns.names, function(x) xmlValue(x))
    ns.labels <- getNodeSet(doc, paste(path, "oml:metric/oml:label", sep ="/"))
    metric.labels <- lapply(ns.labels, function(x) xmlValue(x))
    ns.values <- getNodeSet(doc, paste(path, "oml:metric/oml:value", sep ="/"))
    metric.values <- lapply(ns.values, function(x) xmlValue(x))
    
    metrics <- mapply(list, metric.names, metric.labels, metric.values, SIMPLIFY=FALSE)
    for(i in seq_along(metrics)) {
      names(metrics[[i]]) <- c("name", "label", "value")
    }
    return(metrics)
  }
  run.id <- xmlRValS(doc, "/oml:get_run/oml:run_id")
  # FIXME: remove as soon as the new format is used!
  task.id <- ""
  user.id <- ""
  # FIXME: uncomment as soon as the new format is used!
  # task.id <- xmlRValS(doc, "/oml:get_run/oml:task_id")
  # user.id <- xmlRValS(doc, "/oml:get_run/oml:user_id")
  implementation.id <- xmlRValS(doc, "/oml:get_run/oml:setup/oml:implementation")
  
  # FIXME: what about recursive parameters (pars of sub-components)?
  # FIXME: should we use a list here instead of a named character vector?
  ns.pars <- getNodeSet(doc, "/oml:get_run/oml:setup/oml:parameters/oml:parameter")
  par.names <- unlist(lapply(ns.params, function(x) xmlGetAttr(x, "name")))
  par.names <- unlist(lapply(str_split(par.names, "_"), function(x) x[2]))
  parameters <- as.character(unlist(lapply(ns.params, function(x) xmlGetAttr(x, "value"))))
  names(parameters) <- par.names 
  
  metrics <- getMetrics("/oml:get_run/oml:output_data")
  
  results <- OpenMLRunResults(
    run.id = run.id,
    task.id = task.id,
    user.id = user.id,
    implementation.id = implementation.id,
    parameters = parameters,
    metrics = metrics
  )
  return(results)
}