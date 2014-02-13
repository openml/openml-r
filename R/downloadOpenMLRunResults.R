downloadOpenMLRunResults <- function(id, dir = getwd(), show.info = TRUE) {
  fn.get.run <- file.path(dir, sprintf("get_run_%g.xml", id))
  downloadAPICallFile(api.fun = "openml.run.get", file = fn.get.run, run_id = id, show.info = show.info)
  results <- parseOpenMLRunResults(fn.get.run)
  return(results)
}

parseOpenMLRunResults <- function(file) {
  doc <- parseXMLResponse(file, "Getting run results", "run")
  getMetrics <- function(path) {
    ns.names <- getNodeSet(doc, paste(path, "oml:evaluation/oml:name", sep ="/"))
    metric.names <- unlist(lapply(ns.names, function(x) xmlValue(x)))
    #ns.labels <- getNodeSet(doc, paste(path, "oml:metric/oml:label", sep ="/"))
    #metric.labels <- unlist(lapply(ns.labels, function(x) xmlValue(x)))
    ns.values <- getNodeSet(doc, paste(path, "oml:evaluation/oml:value", sep ="/"))
    metric.values <- unlist(lapply(ns.values, function(x) xmlValue(x)))
    
    metrics <- data.frame(metric.values)#, metric.labels)
    rownames(metrics) <- metric.names
    colnames(metrics) <- c("value")#, "label")
    
    #metrics <- mapply(list, metric.names, metric.labels, metric.values, SIMPLIFY=FALSE)
    #for(i in seq_along(metrics)) {
    #  names(metrics[[i]]) <- c("name", "label", "value")
    #}
    return(metrics)
  }
  run.id <- xmlRValS(doc, "/oml:run/oml:run_id")
  uploader <- xmlRValS(doc, "/oml:run/oml:uploader")
  task.id <- xmlRValS(doc, "/oml:run/oml:task_id")
  implementation.id <- xmlRValS(doc, "/oml:run/oml:implementation_id")
  setup.id <- xmlRValS(doc, "/oml:run/oml:setup_id")
  
  data.set.id <- xmlRValS(doc, "/oml:run/oml:input_data/oml:dataset/oml:did")
  data.set.name <- xmlRValS(doc, "/oml:run/oml:input_data/oml:dataset/oml:name")
  data.set.url <- xmlRValS(doc, "/oml:run/oml:input_data/oml:dataset/oml:url")
  
  # FIXME: what about recursive parameters (pars of sub-components)?
  # FIXME: should we use a list here instead of a named character vector?
  ns.pars <- getNodeSet(doc, "/oml:run/oml:setup/oml:parameters/oml:parameter")
  par.names <- unlist(lapply(ns.pars, function(x) xmlGetAttr(x, "name")))
  par.names <- unlist(lapply(str_split(par.names, "_"), function(x) x[2]))
  parameters <- as.character(unlist(lapply(ns.pars, function(x) xmlGetAttr(x, "value"))))
  names(parameters) <- par.names 
  
  pred.id <- xmlRValS(doc, "/oml:run/oml:output_data/oml:dataset/oml:did")
  pred.name <- xmlRValS(doc, "/oml:run/oml:output_data/oml:dataset/oml:name")
  pred.url <- xmlRValS(doc, "/oml:run/oml:output_data/oml:dataset/oml:url")
  
  metrics <- getMetrics("/oml:run/oml:output_data")
  
  # FIXME: add data set descriptions
  results <- OpenMLRunResults(
    run.id = run.id,
    task.id = task.id,
    uploader = uploader,
    implementation.id = implementation.id,
    setup.id = setup.id,
    #data.set.desc = OpenMLDataSetDescription(id = data.set.id, name = data.set.name, url = data.set.url),
    parameters = parameters,
    #pred.desc = OpenMLDataSetDescription(id = pred.id, name = pred.name, url = pred.url),
    metrics = metrics
  )
  return(results)
}