downloadOpenMLTaskResults <- function(id, dir = getwd(), show.info = TRUE) {
  fn.task.results <- file.path(dir, sprintf("results_task_%g.xml", id))
  downloadAPICallFile(api.fun = "openml.task.evaluations", file = fn.task.results, task_id = id, show.info = show.info)
  results <- parseOpenMLTaskResults(fn.task.results)
  return(results)
}

parseOpenMLTaskResults <- function(file) {
  doc <- parseXMLResponse(file, "Getting task results", "task_results")
  getMetrics <- function() {
    ns.ids <- getNodeSet(doc, "/oml:task_results/oml:evaluations/oml:run/oml:run_id")
    run.ids <- unlist(lapply(ns.ids, function(x) xmlValue(x)))
    
    ns.impl.names <- getNodeSet(doc, "/oml:task_results/oml:evaluations/oml:run/oml:implementation")
    impl.names <- unlist(lapply(ns.impl.names, function(x) xmlValue(x)))
    
    ns.metrics <- getNodeSet(doc, "/oml:task_results/oml:evaluations/oml:run/oml:measure")
    metric.names <- unlist(lapply(ns.metrics, function(x) xmlGetAttr(x, "name")))
    metric.values <- as.numeric(unlist(lapply(ns.metrics, function(x) xmlValue(x))))
    
    unique.names <- unique(metric.names)
    
    metrics <- as.data.frame(impl.names)
    for(i in seq_along(unique.names)) {
      metrics <- cbind(metrics, metric.values[metric.names == unique.names[i]])
    }
    colnames(metrics) <- c("implementation", unique.names)
    rownames(metrics) <- run.ids
    
    return(metrics)
  }
  task.id <- xmlRValS(doc, "/oml:task_results/oml:task_id")
  task.name <- xmlRValS(doc, "/oml:task_results/oml:task_name")
  task.type.id <- xmlRValS(doc, "/oml:task_results/oml:task_type_id")
  input.data <- xmlRValS(doc, "/oml:task_results/oml:input_data")
  estim.proc <- xmlRValS(doc, "/oml:task_results/oml:estimation_procedure")
  
  if(length(getNodeSet(doc, "/oml:task_results/oml:evaluations/oml:run")) != 0) {
    # FIXME: works ONLY IF for each run the same metrics were computed!
    metrics <- getMetrics()
  } else {
    metrics <- data.frame()
  }
  
  results <- OpenMLTaskResults(
    task.id = task.id,
    task.name = task.name,
    task.type.id = task.type.id,
    input.data = input.data,
    estimation.procedure = estim.proc,
    metrics = metrics
  )
  return(results)
}