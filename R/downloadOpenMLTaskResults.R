#' Download OpenML task results from the server.
#' 
#' @param id [\code{numeric}]\cr 
#'   The task ID.
#' @param dir [\code{character(1)}]\cr 
#'   The directory where to save the downloaded run xml. The file is called "results_task_id.xml" where "id" is 
#'   replaced by the actual id. Default is the working directory.
#' @param show.info [\code{logical(1)}]\cr
#'   Verbose output on console?
#'   Default is \code{TRUE}.
#' @param clean.up [\code{logical(1)}]\cr
#'   Should the downloaded xml file be removed at the end? 
#'   Default is \code{TRUE}.
#' @return [\code{\link{OpenMLTaskResults}}]
#' @export
downloadOpenMLTaskResults <- function(id, dir = getwd(), show.info = TRUE, clean.up = TRUE) {
  fn.task.results <- file.path(dir, sprintf("results_task_%g.xml", id))
  downloadAPICallFile(api.fun = "openml.task.evaluations", file = fn.task.results, task_id = id, 
    show.info = show.info)
  results <- parseOpenMLTaskResults(fn.task.results)
  if (clean.up)
    unlink(fn.task.results)
  return(results)
}

parseOpenMLTaskResults <- function(file) {
  doc <- parseXMLResponse(file, "Getting task results", "task_evaluations")
  getMetrics <- function(ns.runs) {
    task.res <- list()
    for (i in seq_along(ns.runs)) {
      run.id <- xmlRValI(doc, paste("/oml:task_evaluations/oml:evaluation", 
        "[", i, "]/oml:run_id", sep=''))
      setup.id <- xmlRValI(doc, paste("/oml:task_evaluations/oml:evaluation", 
        "[", i, "]/oml:setup_id", sep=''))
      impl.id <- xmlRValS(doc, paste("/oml:task_evaluations/oml:evaluation", 
        "[", i, "]/oml:implementation_id", sep=''))
      impl <- xmlRValI(doc, paste("/oml:task_evaluations/oml:evaluation", 
        "[", i, "]/oml:implementation", sep=''))
      ns.metrics <- getNodeSet(doc, paste("/oml:task_evaluations/oml:evaluation",
        "[", i, "]/oml:measure", sep=''))
      
      metric.names <- unlist(lapply(ns.metrics, function(x) xmlGetAttr(x, "name")))
      # FIXME: os_information must remain a string
      metric.values <- as.numeric(unlist(lapply(ns.metrics, function(x) xmlValue(x))))
      
      task.res[[i]] <- data.frame(run.id, setup.id, impl.id, impl, t(metric.values))
      colnames(task.res[[i]])[-(1:4)] <- metric.names
    }
    metrics <- do.call(rbind.fill, task.res)
    row.names(metrics) <- metrics$run.id
    metrics$run.id <- NULL
    return(metrics)
  }
  task.id <- xmlRValS(doc, "/oml:task_evaluations/oml:task_id")
  task.name <- xmlRValS(doc, "/oml:task_evaluations/oml:task_name")
  task.type.id <- xmlRValS(doc, "/oml:task_evaluations/oml:task_type_id")
  input.data <- xmlRValS(doc, "/oml:task_evaluations/oml:input_data")
  estim.proc <- xmlRValS(doc, "/oml:task_evaluations/oml:estimation_procedure")
  
  ns.runs <- getNodeSet(doc, "/oml:task_evaluations/oml:evaluation")
  if (length(ns.runs) != 0) {
    metrics <- getMetrics(ns.runs)
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